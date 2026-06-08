# database related utility functions for the SSPF application
import geoalchemy2
import sqlalchemy
import pandas as pd
from sqlalchemy import create_engine
import geopandas as gpd
from psycopg2 import sql
import os

# Module-level engine cache to avoid creating multiple engines
_engine_cache = None

def get_database_connection_string():
    db_user = os.getenv("DB_USER", "postgres")
    db_password = os.getenv("DB_PASSWORD", "password")
    db_host = os.getenv("DB_HOST", "localhost")
    db_port = os.getenv("DB_PORT", "5432")
    db_name = os.getenv("DB_NAME", "sspf_db")
    
    # use the psycopg2 format for raw connections
    raw_connection_string = f"dbname='{db_name}' user='{db_user}' host='{db_host}' port='{db_port}' password='{db_password}'"
    return raw_connection_string

def get_ogr_pg_conn_string():
    "Get the OGR connection string for PostGIS"
    db_user = os.getenv("DB_USER", "postgres")
    db_password = os.getenv("DB_PASSWORD", "password")
    db_host = os.getenv("DB_HOST", "localhost")
    db_port = os.getenv("DB_PORT", "5432")
    db_name = os.getenv("DB_NAME", "sspf_db")

    return (
        f"PG:host={db_host} "
        f"port={db_port} "
        f"dbname={db_name} "
        f"user={db_user} "
        f"password={db_password}"
    )

def get_database_engine(connect_timeout=None):
    """
    Get or create a SQLAlchemy engine. Engines are cached at module level
    to enable connection pooling and reduce overhead.
    """
    global _engine_cache
    
    # Return cached engine if it exists and no special timeout is requested
    if _engine_cache is not None and connect_timeout is None:
        return _engine_cache
    
    db_user = os.getenv("DB_USER", "postgres")
    db_password = os.getenv("DB_PASSWORD", "password")
    db_host = os.getenv("DB_HOST", "localhost")
    db_port = os.getenv("DB_PORT", "5432")
    db_name = os.getenv("DB_NAME", "sspf_db")

    connection_string = f"postgresql+psycopg2://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}"
    
    if connect_timeout:
        # Don't cache engines with custom timeouts
        engine = create_engine(
            connection_string, 
            connect_args={"connect_timeout": connect_timeout},
            pool_pre_ping=True  # Verify connections before using
        )
    else:
        # Create and cache the default engine
        engine = create_engine(
            connection_string,
            pool_pre_ping=True,  # Verify connections before using
            pool_size=10,  # Default connection pool size
            max_overflow=20  # Allow up to 20 additional connections during peaks
        )
        _engine_cache = engine
    
    return engine

def get_column_types(table_name:str, columns: list, schema=None):
    """
    Returns the data type of the columns in a table as a dict.

    Parameters
    ----------
    table_name : str
        the table name
    columns : list
        the column names
    schema : str, optional
        the schema

    returns
    -------
    dict
    """
    if "." not in table_name:
        if not schema:
            raise ValueError("Either schema must be provided or table_name must be schema-qualified")
        else:
            table_name = f"{schema}.{table_name}"

    # Use information_schema for better portability and consistency with other queries
    eng = get_database_engine()
    placeholders = ', '.join([f':col{i}' for i in range(len(columns))])
    params = {f'col{i}': col for i, col in enumerate(columns)}
    params['table_name'] = table_name
    
    query = sqlalchemy.text(f"""
        SELECT
            column_name,
            CASE 
                WHEN data_type = 'ARRAY' THEN udt_name || '[]'
                WHEN data_type = 'USER-DEFINED' THEN udt_name
                ELSE data_type
            END as formatted_type
        FROM information_schema.columns
        WHERE table_schema || '.' || table_name = :table_name
        AND column_name IN ({placeholders})
    """)
    
    df = pd.read_sql(query, eng, params=params)
    
    if df.empty:
        raise ValueError(f"No columns found matching {columns} in table {table_name}")
    
    d = dict(zip(df['column_name'], df['formatted_type']))
    
    return d

def get_table_columns(table_name, schema=None):
    """
    Returns all column names for a table as a list.

    Parameters
    ----------
    table_name : str
        the table name
    schema : str, optional
        the schema

    Returns
    -------
    list
        List of column names in the table
    """
    if "." not in table_name:
        if not schema:
            raise ValueError("Either schema must be provided or table_name must be schema-qualified")
        else:
            table_name = f"{schema}.{table_name}"
    
    eng = get_database_engine()
    query = sqlalchemy.text("""
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema || '.' || table_name = :table_name
        ORDER BY ordinal_position
    """)
    
    df = pd.read_sql(query, eng, params={'table_name': table_name})
    
    if df.empty:
        raise ValueError(f"Table {table_name} not found or has no columns")
    
    return df['column_name'].tolist()


def execute_query(query: str, params: dict = None, fetch: bool = False):
    """
    Execute a SQL query on the database.
    If fetch is True, returns the fetched results as a list of tuples.
    Returns (success, error (if any), results (if requested)) where results is None if fetch is False.
    """
    eng = get_database_engine()
    with eng.begin() as conn:
        try:
            result = conn.execute(sqlalchemy.text(query), params or {})
            if fetch:
                return True, None, result.fetchall()
            else:
                return True, None, None
        except Exception as e:
            print(f"Error executing query: {e}")
            return False, str(e), None

def create_index_sql(table_name: str, col, schema_name: str, drop_if_exists: bool = True):
    """
    Generate SQL to create index on a given table column or columns.
    col can be a string (single column) or a list of strings (multiple columns).
    """
    if isinstance(col, str):
        columns = [col]
    else:
        columns = col

    eng = get_database_engine()
    q = f"""
        SELECT column_name, data_type, udt_name
        FROM information_schema.columns
            WHERE
                table_schema = '{schema_name}'
                AND 
                table_name = '{table_name}'
                AND
                column_name IN ({', '.join([f"'{c}'" for c in columns])})
    """
    print(q)
    df = pd.read_sql(q, con=eng)
    if df.shape[0] != len(columns):
        raise ValueError(f"One or more columns {columns} do not exist in table {schema_name}.{table_name}")
    
    index_params = [
        # (col_name, index_name, index_type)
    ]

    for _, row in df.iterrows():
        index_params.append(
            (
                row["column_name"],
                f"idx_{schema_name}_{table_name}_{row['column_name']}",
                "GIN" if row["data_type"] == "ARRAY" or row["udt_name"] in ["jsonb", "hstore"] \
                    else "GIST" if row["data_type"] == "USER-DEFINED"  and row["udt_name"] == "geometry" \
                    else "BTREE"
            )
        )

    index_sql = ""
    if drop_if_exists:
        index_sql += "\n".join([f"DROP INDEX IF EXISTS {ip[1]};" for ip in index_params]) + "\n"
    index_sql += "\n".join([f"CREATE INDEX {ip[1]} ON {schema_name}.{table_name} USING {ip[2]} ({ip[0]});" for ip in index_params])
    return index_sql

def get_account_details(user_id: int, username: str):
    "Retrieve account details from the database."
    # only one of user_id or username are needed, if both are provided, username is ignored
    if not user_id and not username:
        return None
    if user_id:
        where_clause = "user_id = :val"
        params = {"val": user_id}
    else:
        where_clause = "username = :un"
        params = {"un": username.strip().lower()}
    eng = get_database_engine()
    q = sqlalchemy.text(f"""
        SELECT user_id, username, email, account_created, last_login, total_logins
        FROM gen_management.accounts
        WHERE {where_clause}
        LIMIT 1;
    """)
    df = pd.read_sql(q, eng, params=params)
    if df.empty:
        return None
    row = df.iloc[0]
    return {
        "user_id": int(row["user_id"]),
        "username": row["username"],
        "email": row["email"],
        "account_created": row["account_created"],
        "last_login": row["last_login"],
        "total_logins": int(row["total_logins"])
    }

def list_user_studies(user_id: int):
    eng = get_database_engine()
    df = pd.read_sql(
        sqlalchemy.text("SELECT study_id, study_name FROM gen_management.studies WHERE user_id = :uid ORDER BY study_name"),
        eng, params={"uid": user_id}
    )
    return [{"label": r["study_name"], "value": int(r["study_id"])} for _, r in df.iterrows()]

def get_user_and_study_names(study_id):
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("""
                SELECT username, study_name
                FROM gen_management.studies s
                WHERE s.study_id = :sid
            """),
            {"sid": study_id}
        )
        row = result.fetchone()
        if row:
            return row[0], row[1]
        else:
            return None, None

def get_study_name(study_id):
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("SELECT study_name FROM gen_management.studies WHERE study_id = :sid"),
            {"sid": study_id}
        )
        study = result.fetchone()
        return study[0] if study else None

def table_exists(table_name, schema):
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("""
                SELECT EXISTS (
                    SELECT FROM information_schema.tables 
                    WHERE table_schema = :schema AND table_name = :table
                )
            """), {"schema": schema, "table": table_name}
        ).scalar()
    return result

def drop_table(table_name, schema):
    eng = get_database_engine()
    with eng.begin() as conn:
        conn.execute(sqlalchemy.text(f"DROP TABLE IF EXISTS {schema}.{table_name}"))

def get_study_crs(study_id):
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("""
                SELECT crs
                FROM gen_management.studies
                WHERE study_id = :sid
            """),
            {"sid": study_id}
        )
        row = result.fetchone()
        return row[0] if row else None

def get_bayesian_params(study_id):
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("""
                SELECT bayesian_decay, bayesian_max_prior_weight
                FROM gen_management.studies
                WHERE study_id = :sid
            """),
            {"sid": study_id}
        )
        row = result.fetchone()
        if row:
            return row[0], row[1]
        else:
            return None, None

def delete_study_data(username, study_name, schemas, prefixes=["study_area", "roads", "crashes"]):
    "Delete study data from the given schemas"
    eng = get_database_engine()
    with eng.begin() as conn:
        for s in schemas:
            for p in prefixes:
                conn.execute(f"DROP TABLE IF EXISTS {s}.{p}_{username}_{study_name}")

def get_study_table_names(username, study_name):
    "Generate standard schema qualified table names for a given user and study. adn return as a dict."
    un, sn = username, study_name
    return {
        "input_study_area": f"inputs.study_area_{un}_{sn}",
        "input_roads": f"inputs.roads_{un}_{sn}",
        "input_crashes": f"inputs.crashes_{un}_{sn}",
        "sliding_windows_table": f"scratch.swa_sliding_windows_{un}_{sn}",
        "output_swa_short_windows": f"outputs.swa_short_windows_{un}_{sn}",
        "output_model_results": f"outputs.bayesian_estimates_{un}_{sn}",
        "input_block_group_fatals": f"inputs.block_group_fatals_{un}_{sn}",
    }
    
def table_exists(table_name, schema=None):
    """
    Check if a table exists in the database. If schema is None, needs a fully qualified table name.
    If table_name is schema qualified (schema.table_name), schema parameter is ignored.
    """
    if '.' in table_name:
        schema, table_name = table_name.split('.', 1)
    elif not schema:
        raise ValueError("Either schema must be provided or table_name must be schema qualified.")
    
    eng = get_database_engine()
    with eng.begin() as conn:
        result = conn.execute(
            sqlalchemy.text("""
                SELECT EXISTS (
                    SELECT 1 FROM information_schema.tables 
                    WHERE table_schema = :schema AND table_name = :table
                )
            """), {"schema": schema, "table": table_name}
        ).scalar()
    return result