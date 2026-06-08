# might move this to dashboard_func later to keep it consistent with the page structure
import pandas as pd
import geopandas as gpd
import sqlalchemy
import zipfile
import io
import os
import tempfile
import subprocess
import logging
from pathlib import Path
from ..global_vars import *
from . import db

logger = logging.getLogger(__name__)

EXPORT_PARAMS = {
    # input table key: output layer name
    "input_study_area": "input_study_area",
    "input_crashes": "input_crashes",
    "input_roads": "input_roads",
    "input_block_group_fatals": "input_block_group_fatals",
    "output_swa_short_windows": "sliding_window_results",
    "output_model_results": "safer_streets_model_results"
}

def _check_db_table_presence(study_id, export_params=EXPORT_PARAMS):
    """
    Check which tables are present for the study in the database.
    Returns a dict of table_key: bool indicating presence.
    """
    un, sn = db.get_user_and_study_names(study_id)
    study_table_names = db.get_study_table_names(username=un, study_name=sn)
    table_presence = {}
    for table_key in export_params.keys():
        if table_key in study_table_names and db.table_exists(table_name=study_table_names[table_key]):
            table_presence[table_key] = True
        else:
            table_presence[table_key] = False
    return table_presence

def _get_data_dictionary(study_id, export_params=EXPORT_PARAMS):
    """
    Create data dictionary for study GIS exports.
    """
    start_time = datetime.now()

    un, sn = db.get_user_and_study_names(study_id)
    study_table_names = db.get_study_table_names(username=un, study_name=sn)

    table_presence = _check_db_table_presence(study_id, export_params=export_params)

    # the data dictionary folder is located in the "data_dictionaries" folder in the same directory as this script (i.e utils)
    data_dictionary_folder = Path(__file__).parent / "data_dictionaries"

    data_dicts = {}
    for table_key, layer_name in export_params.items():
        if table_presence.get(table_key, False) and table_key != "input_study_area":
            data_dict_path = (data_dictionary_folder / f"{layer_name}.csv").resolve()
            data_dicts[layer_name] = pd.read_csv(data_dict_path)
    
    return data_dicts


def compile_study_data(study_id, output_folder, output_zip_name, export_params=EXPORT_PARAMS):
    """
    Compile study data into a downloadable file (zip)
    Contains the following files:
        - GPKG for spatial data
            - Input Study Area
            - Input Crashes
            - Input Roads
            - Fatality Block Groups
            - Sliding Windows Analysis Results (if available)
                - Short windows
                - Sliding windows (or just top N corridors?)
            - Safer Streets Model Results (if available)
                - Short windows
                - 
        - Data Dictionaries for any spatial data
        - Excel or CSVs for tabular data
        - Text files (for study metadata and configurations)
    """
    start_time = datetime.now()

    un, sn = db.get_user_and_study_names(study_id)
    study_table_names = db.get_study_table_names(username=un, study_name=sn)

    table_presence = _check_db_table_presence(study_id, export_params=export_params)

    # create temp dir to hold files
    with tempfile.TemporaryDirectory() as temp_dir:
        temp_path = Path(temp_dir)

        # create gpkg path
        gpkg_path = temp_path / f"study_data_{un}_{sn}.gpkg"

        for table_key, layer_name in export_params.items():
            if table_presence.get(table_key, False):
                ts_print(f"Exporting {table_key} to layer {layer_name}...")
                sql = create_gpkg_compatible_sql(
                    table_name=study_table_names[table_key],
                    convert_to_multi=True,
                    geom_col="geom",
                )
                ogr_export_sql_to_gpkg(
                    sql=sql,
                    gpkg_path=str(gpkg_path),
                    layer_name=layer_name,
                    geom="geom",
                    replace_layer=True,
                    spatial_index=True,
                )
            else:
                ts_print(f"  - Table {table_key} not found for study {sn}, skipping...")

        data_dicts = _get_data_dictionary(study_id=study_id, export_params=export_params)
        with pd.ExcelWriter(temp_path / "data_dictionaries.xlsx", engine="xlsxwriter") as writer:
            for layer_name, df in data_dicts.items():
                df.to_excel(writer, sheet_name=layer_name, index=False)
        
        # create zip file
        ts_print(f"Creating output zip file at {output_folder / output_zip_name}...")
        zip_path = Path(output_folder) / output_zip_name
        with zipfile.ZipFile(zip_path, mode="w", compression=zipfile.ZIP_DEFLATED) as zipf:
            # add gpkg
            zipf.write(gpkg_path, arcname=gpkg_path.name)
            # add data dictionaries
            data_dict_path = temp_path / "data_dictionaries.xlsx"
            zipf.write(data_dict_path, arcname=data_dict_path.name)

    duration = (datetime.now() - start_time).total_seconds()
    hours, remainder = divmod(int(duration), 3600)
    minutes, seconds = divmod(remainder, 60)
    time_str = f"{hours}h {minutes}m {seconds}s"
    ts_print(f"Study data compilation completed in {time_str}.")


def create_gpkg_compatible_sql(table_name: str, schema: str = None,
                               filter_sql: str = None, columns: list = None,
                               convert_to_multi=False, geom_col="geom", geom_type=None) -> str:
    """
    Create SQL to export a table with columns that work cleanly to GeoPackage-compatible format.
    If schema is not provided, table_name must be schema-qualified.
    """
    if "." not in table_name:
        if not schema:
            raise ValueError("Either schema must be provided or table_name must be schema-qualified")
        else:
            table_name = f"{schema}.{table_name}"
    
    available_columns = db.get_table_columns(table_name=table_name)

    if not columns:
        columns = available_columns
    else:
        # remove duplicates while preserving order, ensure geom_col is included
        seen = set()
        columns_deduped = []
        for c in columns + [geom_col]:
            if c not in seen:
                columns_deduped.append(c)
                seen.add(c)
        columns = [c for c in columns_deduped if c in available_columns]

    # get col types
    col_types = db.get_column_types(table_name=table_name, columns=columns)

    # build cast sql dict
    cast_sql_dict = {}
    for col, col_type in col_types.items():
        # boolean to int
        if col_type in ["boolean"]:
            cast_sql_dict[col] = _boolean_to_int_sql(col)
        # arrays to string
        elif "[]" in col_type:
            cast_sql_dict[col] = _array_to_str_sql(col)
        # varchar and char to text
        elif col_type.startswith("character varying") or col_type.startswith("character"):
            cast_sql_dict[col] = _to_text_sql(col)
        # convert to multi if specified
        elif convert_to_multi and col == geom_col:
            cast_sql_dict[col] = _convert_to_multi_geom_sql(col, homogeneous=bool(geom_type), geom_type=geom_type)
        # convert json and jsonb to text
        elif col_type in ["json", "jsonb"]:
            cast_sql_dict[col] = _to_text_sql(col)
        else:
            # keep original column
            pass

    
    sql = _create_export_sql(
        table_name=table_name,
        cols=columns,
        cast_sql_dict=cast_sql_dict,
        filter_sql=filter_sql,
    )

    return sql

def _array_to_str_sql(col, separator: str = ",") -> str:
    sep_escaped = separator.replace("'", "''")  # escape single quotes in case it is used as separator
    return f"COALESCE(ARRAY_TO_STRING({_quote_identifier(col)}, '{sep_escaped}'), '')"

def _convert_to_multi_geom_sql(col: str = "geom", homogeneous: bool = False, geom_type: str = None) -> str:
    """
    Convert a geometry column to MULTI* type.
    If homogeneous is True, specify geom_type ('POINT', 'LINESTRING', 'POLYGON')
    to ensure all geometries are of that type.
    """
    geom_type_map = {
        "POINT": 1,
        "LINESTRING": 2,
        "POLYGON": 3,
    }

    if homogeneous:
        if geom_type not in geom_type_map:
            raise ValueError(
                "geom_type must be one of 'POINT', 'LINESTRING', or 'POLYGON' when homogeneous=True"
            )
        type_code = geom_type_map[geom_type]
        return f"ST_Multi(ST_CollectionExtract({_quote_identifier(col)}, {type_code}))"

    return f"ST_Multi({_quote_identifier(col)})"

def _boolean_to_int_sql(col: str) -> str:
    return f"CASE WHEN {_quote_identifier(col)} IS NULL THEN NULL WHEN {_quote_identifier(col)} THEN 1 ELSE 0 END"

def _to_text_sql(col: str) -> str:
    "For col types that can cast directly to TEXT in Postgres"
    return f"{_quote_identifier(col)}::TEXT"

def _to_int_sql(col: str) -> str:
    "For col types that can cast directly to INT in Postgres"
    return f"{_quote_identifier(col)}::INT"

def _quote_identifier(name: str) -> str:
    """
    Quote an SQL identifier (e.g. column name) to handle special characters or reserved words.
    Not needed, but safer to use in generated SQL.
    """
    return '"' + name.replace('"', '""') + '"'

def _to_numeric_precision_sql(col: str, decimal_places: int) -> str:
    """
    Cast and round a numeric column to a specific precision (decimal places).
    
    Parameters:
    - col: Column name.
    - decimal_places: Number of decimal places to round to.
    
    Returns:
    - SQL expression that rounds the column to the specified precision.
    """
    return f"ROUND({_quote_identifier(col)}::NUMERIC, {decimal_places})"


def _create_export_sql(table_name: str, 
                       schema: str = None, 
                       cols: list = None,
                       filter_sql: str = None, 
                       cast_sql_dict: dict = None,
                       ) -> str:
    """
    Create SQL query to export a table or specific columns from a table.
    If schema is not provided, table_name must be schema-qualified.
    Parameters:
    - table_name: Name of the table to export.
    - schema: Schema of the table (optional if table_name is schema-qualified).
    - cols: List of columns to select (default is all columns).
    - filter_sql: SQL WHERE clause to filter rows (optional).
    - cast_sql_dict: Dict of columns to cast with their SQL expressions.

    Returns:
    - SQL query string.
    """
    if "." not in table_name:
        if not schema:
            raise ValueError("Either schema must be provided or table_name must be schema-qualified")
        else:
            table_name = f"{schema}.{table_name}"
    
    if cols:
        select_cols = []
        for c in cols:
            if cast_sql_dict and c in cast_sql_dict:
                cast_sql_exp = cast_sql_dict[c]
                select_cols.append(f"{cast_sql_exp} AS {_quote_identifier(c)}")
            else:
                select_cols.append(_quote_identifier(c))
        select_clause = ", ".join(select_cols)
    else:
        select_clause = "*"

    sql = f"""
        SELECT
            {select_clause}
        FROM {table_name}
        {f"WHERE {filter_sql}" if filter_sql else ""}
    """
    return sql

def ogr_export_sql_to_gpkg(sql: str, gpkg_path: str, layer_name: str, geom: str = "geom",
                          replace_layer: bool = True, spatial_index: bool = True) -> None:
    """
    Export a PostGIS SQL query to a GeoPackage layer using ogr2ogr.

    - Creates the GeoPackage if it doesn't exist.
    - If it exists:
        * replace_layer=True  -> drops existing layer table then writes fresh
        * replace_layer=False -> appends/creates layer without dropping

    Requirements:
    - sql MUST return one geometry column (can be aliased), e.g. "... , geom AS geom FROM ..."
    """
    ogr_conn_string = db.get_ogr_pg_conn_string()
    gpkg = Path(gpkg_path)

    # If gpkg exists and we want to replace the layer, drop the layer first (safe + deterministic)
    if gpkg.exists() and replace_layer:
        # GeoPackage is SQLite; easiest reliable drop is via ogrinfo SQL
        drop_cmd = [
            "ogrinfo",
            gpkg_path,
            "-sql", f'DROP TABLE IF EXISTS "{layer_name}"'
        ]
        # Don't fail if table doesn't exist; many builds return non-zero for harmless cases
        subprocess.run(drop_cmd, check=False, capture_output=True, text=True)

        # Also try dropping the gpkg_contents row (some clients leave metadata behind)
        cleanup_cmd = [
            "ogrinfo",
            gpkg_path,
            "-sql", f"DELETE FROM gpkg_contents WHERE table_name = '{layer_name}'"
        ]
        subprocess.run(cleanup_cmd, check=False, capture_output=True, text=True)

        # cleanup geometry_columns entry as well
        cleanup_geom_cmd = [
            "ogrinfo",
            gpkg_path,
            "-sql", f"DELETE FROM gpkg_geometry_columns WHERE table_name = '{layer_name}'"
        ]
        subprocess.run(cleanup_geom_cmd, check=False, capture_output=True, text=True)

    cmd = [
        "ogr2ogr",
        "-f", "GPKG",
        gpkg_path,
        ogr_conn_string,
        "-dialect", "PostgreSQL",
        "-sql", sql,
        "-nln", layer_name,
        "-lco", f"GEOMETRY_NAME={geom}",
        "--config", "PG_USE_COPY", "YES",
    ]

    if spatial_index:
        cmd += ["-lco", "SPATIAL_INDEX=YES"]

    # If file doesn't exist, create it fresh; otherwise update it
    if gpkg.exists():
        cmd += ["-update"]
    else:
        cmd += ["-overwrite"]  # safe here because file doesn't exist

    result = subprocess.run(cmd, check=False, capture_output=True, text=True)
    if result.returncode != 0:
        logger.error(
            "ogr2ogr failed\nCMD: %s\nSTDOUT:\n%s\nSTDERR:\n%s",
            " ".join(cmd), result.stdout, result.stderr
        )
        raise RuntimeError(
            f"ogr2ogr failed (exit code {result.returncode}):\n{result.stderr}"
        )
