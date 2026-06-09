# utility functions for loading data and files in the SSPF application
import psycopg2
import logging
from . import db

logger = logging.getLogger(__name__)
from dash import dcc, html
import dash_leaflet as dl
import dash_extensions as dx
import dash_bootstrap_components as dbc
from dash_extensions.javascript import assign
import sqlalchemy
import pandas as pd
import geopandas as gpd
import re
import os
import utm
from psycopg2 import sql
from ..global_vars import *
import base64, io, zipfile, tempfile, json
from shapely.geometry import shape

# --- Helper functions ---
def get_state_list(table="static.us_counties", state_col="state_name"):
    "Get list of unique states from the given table and column"
    engine = db.get_database_engine()
    query = f"""
        SELECT
            DISTINCT({state_col}::TEXT) AS state
        FROM {table};
    """
    with engine.connect() as conn:
        state_list = sorted(list(pd.read_sql(query, conn)["state"]))
    return state_list

def get_county_list(state, table="static.us_counties", state_col="state_name", county_col="namelsad"):
    "Get list of counties for a given state from the given table and columns"
    engine = db.get_database_engine()
    query = f"""
        SELECT
            {county_col}::TEXT AS county
        FROM {table}
        WHERE {state_col} = '{state}'
        ORDER BY {county_col}::TEXT;
    """
    with engine.connect() as conn:
        county_list = sorted(list(pd.read_sql(query, conn)["county"]))
    return county_list

def get_county_geojson(state, county, table="static.us_counties", state_col="state_name", county_col="namelsad"):
    "Get geometry of a specific county in a state from the given table and columns as a GeoDataFrame"
    engine = db.get_database_engine()
    query = f"""
        SELECT
            ST_AsGeoJSON(ST_Transform(ST_Union(geom), 4326)) AS geom_geojson
        FROM {table}
        WHERE {state_col} = '{state}' AND {county_col} = '{county}';
    """
    with engine.connect() as conn:
        df = pd.read_sql(query, conn)
    if df.empty or not df.iloc[0]["geom_geojson"]:
        return None
    return json.loads(df.iloc[0]["geom_geojson"])

def get_dissolved_geojson(table, schema, where_clause="TRUE"):
    "Read dissolved geometry from a table with a where clause"
    query = f"""
        SELECT
            ST_AsGeoJSON(ST_Transform(ST_Union(geom), 4326)) AS geom_geojson
        FROM {schema}.{table}
        WHERE {where_clause};
    """
    engine = db.get_database_engine()
    with engine.connect() as conn:
        df = pd.read_sql(query, conn)
    if df.empty or not df.iloc[0]["geom_geojson"]:
        return None
    return json.loads(df.iloc[0]["geom_geojson"])

def _get_centroid_lat_long(geometry):
    "Get centroid of a shapely geometry or gdf in EPSG:4326"
    # if gdf then union project to 4326 (lat, lon) and union
    if isinstance(geometry, gpd.GeoDataFrame):
        geometry = geometry.to_crs(epsg=4326).union_all()
        geometry = geometry.geometry.iloc[0]
    lat, lon = geometry.centroid.y, geometry.centroid.x
    return lat, lon

def get_utm_epsg(geometry):
    "Get UTM zone number from a shapely geometry or gdf"
    lat, lon = _get_centroid_lat_long(geometry)
    utm_data = utm.from_latlon(lat, lon)
    utm_epsg = 32600 + utm_data[2]
    return utm_epsg

def _get_state_from(geometry):
    "Get state abbreviation from a shapely geometry or gdf"
    # fist get state from centroid if it intesects with any of the states
    if isinstance(geometry, gpd.GeoDataFrame):
        geometry = geometry.to_crs(epsg=4326).union_all()
        geometry = geometry.geometry.iloc[0]
    lat, lon = _get_centroid_lat_long(geometry)
    engine = db.get_database_engine()
    query = f"""
        SELECT
            stusps
        FROM
            static.us_states
        WHERE
            ST_Intersects(geom, ST_SetSRID(ST_MakePoint({lon}, {lat}), 4326))
        LIMIT 1
    """
    with engine.connect() as conn:
        df = pd.read_sql(query, conn)
    if df.empty:
        # use entire geometry get the largest intersecting state
        query = f"""
            SELECT
                stusps
            FROM
                static.us_states
            WHERE
                ST_Intersects(geom, ST_SetSRID(ST_GeomFromGeoJSON(:geom_geojson), 4326))
            ORDER BY
                ST_Area(ST_Intersection(geom, ST_SetSRID(ST_GeomFromGeoJSON(:geom_geojson), 4326))) DESC
            LIMIT 1
        """
        with engine.connect() as conn:
            df = pd.read_sql(query, conn, params={"geom_geojson": json.dumps(geometry.__geo_interface__)})
    if df.empty:
        return None
    return df.iloc[0]["stusps"]

def validate_and_read_shp(upload_contents, upload_filename, max_mb=20, geom_types=[]):
    """
    Do some validation checks on the uploaded shapefile ZIP
    Returns (is_valid: bool, gdf: GeoDataFrame|None, message: str)
    """
    # ensure it is no more than 20 MB
    size_match = re.search(r"base64,([A-Za-z0-9+/=]+)$", upload_contents or "")
    size_bytes = int(len(size_match.group(1)) * 3 / 4) if size_match else 0
    if size_bytes > max_mb * 1024 * 1024:
        return False, None, f"File exceeds {max_mb} MB size limit."

    # ensure it is a ZIP and contains .shp, .shx, .dbf, and .prj
    try:
        _, b64 = upload_contents.split(",", 1)
        raw = base64.b64decode(b64)
    except Exception:
        return False, None, "Could not decode uploaded file. Please upload a valid ZIP archive."

    if not upload_filename.lower().endswith(".zip"):
        return False, None, "File must be a ZIP archive."
    else:
        try:
            with tempfile.TemporaryDirectory() as td:
                with zipfile.ZipFile(io.BytesIO(raw)) as zf:
                    # Ignore Mac metadata files/folders that can masquerade as shapefile parts.
                    files_in_zip = []
                    for member in zf.namelist():
                        member = member.replace("\\", "/")
                        if not member or member.endswith("/"):
                            continue
                        base = os.path.basename(member)
                        if member.startswith("__MACOSX/"):
                            continue
                        if base.startswith("._"):
                            continue
                        if base.lower() == ".ds_store":
                            continue
                        files_in_zip.append(member)

                    if not files_in_zip:
                        return False, None, "ZIP does not contain any valid files."

                    zf.extractall(td)

                required_exts = {".shp", ".shx", ".dbf", ".prj"}
                ext_by_stem = {}
                for file_path in files_in_zip:
                    stem, ext = os.path.splitext(file_path.lower())
                    if ext in required_exts:
                        ext_by_stem.setdefault(stem, set()).add(ext)

                valid_stems = [stem for stem, exts in ext_by_stem.items() if required_exts.issubset(exts)]
                if len(valid_stems) == 0:
                    return False, None, "ZIP must contain one complete shapefile set (.shp, .shx, .dbf, and .prj)."
                if len(valid_stems) > 1:
                    return False, None, "ZIP contains multiple shapefiles. Please upload a ZIP with only one shapefile."

                selected_stem = valid_stems[0]
                shp_rel_path = None
                for file_path in files_in_zip:
                    stem, ext = os.path.splitext(file_path.lower())
                    if stem == selected_stem and ext == ".shp":
                        shp_rel_path = file_path
                        break

                if not shp_rel_path:
                    return False, None, "Could not locate the .shp file in the ZIP archive."

                shp = os.path.join(td, *shp_rel_path.split("/"))
                gdf = gpd.read_file(shp)

                # fix any invalid or empty geometries
                gdf = gdf[~gdf.geometry.is_empty & gdf.geometry.notnull()]

                if gdf.empty:
                    return False, None, "Shapefile contains no valid geometries."
                if gdf.crs is None:
                    return False, None, "Shapefile has no defined CRS."
                if gdf.crs.to_epsg() is None:
                    try:
                        gdf = gdf.to_crs(epsg=4326)
                    except Exception:
                        return False, None, "Shapefile CRS could not be converted to a standard coordinate system. Please re-export with a standard CRS (e.g., EPSG:4326)."

                # check geometry types if provided
                if len(geom_types) > 0:
                    if not all(gdf.geometry.geom_type.isin(geom_types)):
                        return False, None, f"Shapefile geometries must be one of the type(s): {', '.join(geom_types)}."

                return True, gdf, "Shapefile successfully validated."
        except zipfile.BadZipFile:
            return False, None, "Uploaded file is not a valid ZIP archive."
        except Exception as e:
            return False, None, f"Could not read shapefile from ZIP: {str(e)}"
        
def get_col_list(table, schema, col_types=[]):
    "Read the attributes in gdf. Optionally provide a field_type to filter"
    q = f"""
            SELECT
                column_name
            FROM information_schema.columns
            WHERE table_schema = '{schema}'
                AND table_name = '{table}'
    """

    eng = db.get_database_engine()
    if len(col_types) > 0:
        q += f"AND LOWER(data_type) IN ({', '.join([f"'{ct.lower()}'" for ct in col_types])})"
    with eng.connect() as conn:
        cols = [row["column_name"] for row in conn.execute(sqlalchemy.text(q)).mappings().all()]
    
    return cols

def column_dropdown(id_, label, options, value=None):
    "Create a dropdown for selecting a column from a list of options"
    return dbc.Row(
        [
            dbc.Col(dbc.Label(label), md=3),
            dbc.Col(dcc.Dropdown(id=id_, options=[{"label":c,"value":c} for c in options],
                                 value=value, placeholder="Select a column…"), md=9),
        ],
        className="g-2 mb-2"
    )

def roads_cols_card(id_col_options=[], name_col_options=[], fclass_col_options=[]):
    "Card for selecting roads columns"
    return dbc.Card(
        dbc.CardBody(
            [
                column_dropdown(id_="roads-id-col", label="ID Column", options=id_col_options, value=None),
                column_dropdown(id_="roads-name-col", label="Road Name Column", options=name_col_options, value=None),
                column_dropdown(id_="roads-fclass-col", label="Functional Class Column", options=fclass_col_options, value=None),
            ]
        ),
        className="mt-2"
    )

def attribute_map_card(attr_type, source=None, source_table=None, attr_col=None, include_nulls=True, include_empty_strings=True):
    """
    Get a data table for mapping functional class values to standard set
    """
    # option values
    option_dict = {
        # attr_type: option_raw_label_list
        "fclass": [{"label": x, "value": x} for x in FUNCTIONAL_CLASSES],
        "crash_mode": [{"label": v, "value": k} for k, v in CRASH_MODES.items()],
        "crash_severity": [{"label": v, "value": k} for k, v in CRASH_SEVERITIES.items()],
    }

    # attribute mapping only relevant for built-in roads. Crashes mode and severity mappings are fixed
    if source == "builtin" and attr_type == "fclass":
        attr_map = OSM_FCLASS_MAP    
    elif source == "upload" and source_table and attr_col:
        eng = db.get_database_engine()
        q = f"""
            SELECT 
                DISTINCT(TRIM("{attr_col}"::TEXT)) AS {attr_type} 
            FROM {source_table} 
            WHERE 
                "{attr_col}" IS NOT NULL AND TRIM("{attr_col}"::TEXT) <> ''
            ORDER BY {attr_type};
        """
        with eng.connect() as conn:
            attr_values = [row[attr_type] for row in conn.execute(sqlalchemy.text(q)).mappings().all()]

        # check for null and empty string counts if requested
        null_count = 0
        empty_count = 0
        if include_nulls:
            q = f"SELECT COUNT(*) AS null_count FROM {source_table} WHERE \"{attr_col}\" IS NULL;"
            with eng.connect() as conn:
                null_count = conn.execute(sqlalchemy.text(q)).mappings().first()["null_count"]
        if include_empty_strings:
            q = f"SELECT COUNT(*) AS empty_count FROM {source_table} WHERE TRIM(\"{attr_col}\"::TEXT) = '';"
            with eng.connect() as conn:
                empty_count = conn.execute(sqlalchemy.text(q)).mappings().first()["empty_count"]

        # by default assign the first matching value from options to each raw value
        attr_map = {x: option_dict[attr_type][0]["value"] for x in attr_values}
        if null_count > 0:
            attr_map['Null Values'] = "Omit From Analysis"
        if empty_count > 0:
            attr_map['Empty String Values'] = "Omit From Analysis"
        
    else:
        attr_map = {}

    # now add a dbc item to with default mapping for each value
    rows = [{"raw": k, "mapped": v} for k, v in attr_map.items()]
    opt_objs = option_dict[attr_type]

    rows = []
    for i, raw in enumerate(attr_map.keys()):
        rows.append(
            dbc.Row(
                [
                    dbc.Col(dbc.Label(
                        children=raw,
                        id={"type": f"{attr_type}-raw", "index": i},  # pattern-matching id
                        className="fw-semibold"), md=6
                    ),
                    dbc.Col(
                        dcc.Dropdown(
                            id={"type": f"{attr_type}-dd", "index": i},            # pattern-matching id
                            options=opt_objs,
                            value=attr_map.get(raw),
                            clearable=False,
                            # placeholder="Choose standardized value…",
                        ),
                        md=6,
                    ),
                ],
                className="g-2 mb-2",
            )
        )

    return dbc.Card(dbc.CardBody(rows), className="mt-2 shadow-sm")
    
def check_uniqueness(table, col):
    """
    Check uniqueness of values in a column. Useful for ID columns. 
    Return (is_unique: bool, no_nulls: bool)
    """
    q = f"""
        SELECT
            COUNT(*) AS total_count,
            COUNT(DISTINCT "{col}") AS unique_count,
            COUNT(*) FILTER (WHERE "{col}" IS NULL) AS null_count
        FROM {table};
    """
    eng = db.get_database_engine()
    with eng.connect() as conn:
        result = conn.execute(sqlalchemy.text(q)).mappings().first()
    
    n_rows = result["total_count"]
    n_distinct = result["unique_count"]
    n_nulls = result["null_count"]

    no_nulls = True if n_nulls == 0 else False
    is_unique = True if n_rows == n_distinct else False

    return is_unique, no_nulls

def upload_study_area(store, username, study_name, state_abbr, schema="inputs"):
    """
    Upload study area to postgis inputs schema.
    Store must contain: source and geometry
    """
    source= store.get("source")
    geom=store.get("geometry")
    utm_epsg = get_utm_epsg(shape(geom))
    table_name = f"study_area_{username}_{study_name}"

    block_group_clip_table = f"inputs.block_group_fatals_{username}_{study_name}"

    # this is stored as a jsonb in the studies table to store xmin, ymin, xmax, ymax
    study_area_bbox_4326 = shape(geom).bounds
    lat, lon = _get_centroid_lat_long(shape(geom))

    eng = db.get_database_engine()

    # update source in the studies table
    q = f"""
        UPDATE gen_management.studies
        SET
            crs = {utm_epsg},
            study_area_updated = NOW(),
            study_area_data_source = '{source}',
            study_centroid_4326 = ST_SetSRID(ST_MakePoint({lon}, {lat}), 4326),
            study_area_bbox_4326 = '{json.dumps(study_area_bbox_4326)}'::jsonb,
            study_state_lookup = '{state_abbr}'
        WHERE 
            study_name = '{study_name}'
            AND 
            username = '{username}'
        ;
    """
    q += f"""
        DROP TABLE IF EXISTS {schema}.{table_name};
        CREATE TABLE {schema}.{table_name} AS
            SELECT
                ST_Transform(ST_SetSRID(ST_GeomFromGeoJSON(:geom), 4326), {utm_epsg}) AS geom
        ;

        CREATE INDEX ON {schema}.{table_name} USING GIST(geom);
        ANALYZE {schema}.{table_name};
    """
    print(f"Uploading study area for {username}_{study_name} at {pd.Timestamp.now()}")
    with eng.begin() as conn:
        conn.execute(sqlalchemy.text(q), {"geom": json.dumps(geom)})

    print(f"Creating clipped block group table for {username}_{study_name} at {pd.Timestamp.now()}")
    # also create a clipped block group table for the study area
    q = f"""
        DROP TABLE IF EXISTS _tmp_sa_{username}_{study_name};
        CREATE TEMP TABLE _tmp_sa_{username}_{study_name} AS (
            SELECT ST_Transform(geom, 4326) AS geom FROM {schema}.{table_name}
        );
        CREATE INDEX ON _tmp_sa_{username}_{study_name} USING GIST(geom);
        ANALYZE _tmp_sa_{username}_{study_name};

        DROP TABLE IF EXISTS {block_group_clip_table};
        CREATE TABLE {block_group_clip_table} AS (
            SELECT
                bg.geoid::TEXT,
                bg.sld_natwalkind_ord::TEXT,
                bg.census_region::TEXT,
                bg.census_division::TEXT,
                bg.assumed_csa_name::TEXT,
                bg.state_abbr_fac::TEXT,
                bg.state_fac::TEXT,
                bg.final_pred_type::TEXT,
                bg.n_ped_fatal_int::INTEGER,
                bg.final_pred_ped::NUMERIC,
                bg.final_se_ped::NUMERIC,
                bg.n_bike_fatal_int::INTEGER,
                bg.final_pred_bike::NUMERIC,
                bg.final_se_bike::NUMERIC,
                bg.n_mv_fatal_int::INTEGER,
                bg.final_pred_mv::NUMERIC,
                bg.final_se_mv::NUMERIC,
                bg.area_sq_m::NUMERIC,
                ST_MakeValid(
                    ST_Multi(
                        ST_CollectionExtract(
                            ST_Transform(ST_Intersection(bg.geom, sa.geom), {utm_epsg}),
                            3
                        )
                    )
                ) AS geom
            FROM
                static.block_group_fatals bg,
                _tmp_sa_{username}_{study_name} sa
            WHERE
                ST_Intersects(bg.geom, sa.geom)
        );

        -- for anything that is a geometry collection, extract the polygon parts
        UPDATE {block_group_clip_table}
        SET geom = ST_Multi(ST_CollectionExtract(geom, 3))
        WHERE ST_GeometryType(geom) = 'ST_GeometryCollection'
        ;

        UPDATE {block_group_clip_table}
        SET
            final_pred_ped = final_pred_ped*(ST_Area(geom) / area_sq_m),
            final_se_ped = final_se_ped*(ST_Area(geom) / area_sq_m),
            final_pred_bike = final_pred_bike*(ST_Area(geom) / area_sq_m),
            final_se_bike = final_se_bike*(ST_Area(geom) / area_sq_m),
            final_pred_mv = final_pred_mv*(ST_Area(geom) / area_sq_m),
            final_se_mv = final_se_mv*(ST_Area(geom) / area_sq_m)
        WHERE area_sq_m > 0
        ;

        CREATE INDEX ON {block_group_clip_table} USING GIST(geom);
        ANALYZE {block_group_clip_table};
    """
    with eng.begin() as conn:
        conn.execute(sqlalchemy.text(q))
    print(f"Completed block group clipping for {username}_{study_name} at {pd.Timestamp.now()}")

def gdf_to_table(gdf, username, study_name, table_name_prefix, schema):
    """
    Upload raw uploaded files to postgis table as is.
    """
    # rename set geometry column to be called geom so that it shows up correctly in postgis
    gdf = gdf.rename_geometry("geom")
    table_name = f"{table_name_prefix}_{username}_{study_name}"
    eng=db.get_database_engine()
    with eng.begin() as conn:
        gdf.to_postgis(table_name, conn, schema=schema, if_exists="replace", index=False)

def recode_case_stmt(col, recode_dict, null_key="Null Values", empty_string_key="Empty String Values", else_value=None):
    """
    Create a SQL CASE statement for recoding values in a column based on a dictionary
    recode_dict: {original_value: new_value, ...}
    null_key: key in recode_dict to use for NULL values
    empty_string_key: key in recode_dict to use for empty string values
    else_value: value to use if original value not in recode_dict
    """
    case_stmt = sql.SQL("CASE \n")
    for orig, new in recode_dict.items():
        if orig == null_key:
            case_stmt += sql.SQL("    WHEN {col} IS NULL THEN {new} \n").format(col=sql.Identifier(col), new=sql.Literal(new))
        elif orig == empty_string_key:
            case_stmt += sql.SQL("    WHEN TRIM({col}::TEXT) = '' THEN {new} \n").format(col=sql.Identifier(col), new=sql.Literal(new))
        else:
            case_stmt += sql.SQL("    WHEN {col} = {orig} THEN {new} \n").format(col=sql.Identifier(col), orig=sql.Literal(orig), new=sql.Literal(new))
    if else_value is not None:
        case_stmt += sql.SQL("ELSE {else_val} \n").format(else_val=sql.Literal(else_value))
    case_stmt += sql.SQL("END")

    try:
        with psycopg2.connect(db.get_database_connection_string()) as raw_conn:
            case_stmt = case_stmt.as_string(raw_conn)
    except psycopg2.Error as e:
        logger.error("Database connection failed in recode_case_stmt", exc_info=True)
        raise RuntimeError(f"Database connection failed: {e.pgerror or e.diag.message_primary}") from None

    return case_stmt

def upload_roads_formatted(username, study_name, store, id_col, name_col, fclass_col):
    "Upload formatted roads to postgis inputs table"
    un = username
    sn = study_name

    eng = db.get_database_engine()
    df = pd.read_sql(f"SELECT crs FROM gen_management.studies WHERE username = '{un}' AND study_name = '{sn}'", eng)
    crs = df["crs"].iloc[0]

    source = store.get("source")

    if source == "builtin":
        source_table = "static.osm_centerlines"
        source_crs = 4326
        id_col = "osm_id"
        name_col = "name"
        fclass_col = "highway"
    elif source == "upload":
        source_table = store.get("staging_table")
        source_crs = store.get("staging_crs")
        id_col = id_col
        name_col = name_col
        fclass_col = fclass_col
        # check for ID uniqueness
    
    fclass_map = store.get("fclass_map")
    fclass_map_case_stmt = recode_case_stmt(col=fclass_col, 
                                            recode_dict=fclass_map,
                                            null_key="Null Values",
                                            empty_string_key="Empty String Values",
                                            else_value="unknown fclass value")

    q = f"""
        DROP TABLE IF EXISTS _tmp_study_area_{un}_{sn};
        CREATE TEMP TABLE _tmp_study_area_{un}_{sn} AS (
            SELECT ST_Transform(geom, {source_crs}) AS geom FROM inputs.study_area_{un}_{sn}
        );
        CREATE INDEX ON _tmp_study_area_{un}_{sn} USING GIST(geom);
        ANALYZE _tmp_study_area_{un}_{sn};

        DROP TABLE IF EXISTS inputs.roads_{un}_{sn};
        CREATE TABLE inputs.roads_{un}_{sn} AS (
            SELECT
                "{id_col}" AS road_id,
                "{name_col}" AS road_name,
                {fclass_map_case_stmt} AS functional_class,
                (ST_Dump(ST_Transform(ST_LineMerge(ST_Intersection(r.geom, sa.geom)), {crs}))).geom::GEOMETRY(LineString, {crs}) AS geom
            FROM
                {source_table} r,
                _tmp_study_area_{un}_{sn} sa
            WHERE
                ST_Intersects(r.geom, sa.geom)
        );

        ALTER TABLE inputs.roads_{un}_{sn} ADD COLUMN pkey SERIAL PRIMARY KEY;
        CREATE INDEX IF NOT EXISTS idx_roads_{un}_{sn}_geom ON inputs.roads_{un}_{sn} USING GIST(geom);
        ANALYZE inputs.roads_{un}_{sn};

        DROP TABLE IF EXISTS _tmp_study_area_{un}_{sn};

        -- update studies table with roads info
        UPDATE gen_management.studies
        SET
            roads_data_source = '{source}',
            roads_id_col = '{id_col}',
            roads_name_col = '{name_col}',
            roads_fclass_col = '{fclass_col}',
            roads_fclass_map = '{json.dumps(fclass_map)}'::jsonb,
            roads_updated = NOW()
        WHERE 
            study_name = '{sn}'
            AND 
            username = '{un}'
        ;

        DROP TABLE IF EXISTS received.roads_{un}_{sn};
    """
    # if it is a user upload, move the staging table to received and clear staging
    if source == "upload":
        q += f"""
            CREATE TABLE received.roads_{un}_{sn} AS (
                SELECT * FROM {source_table}
            );
            DROP TABLE IF EXISTS {source_table};
        """

    # print time 
    start_time = pd.Timestamp.now()
    print(f"Starting roads upload at {start_time}")
    try:
        with eng.begin() as conn:
            conn.execute(sqlalchemy.text(q))
        load_success = True
    except Exception as e:
        load_success = False
        error_message = str(e)
        print(f"Error occurred during roads upload: {e}")

    if load_success:
        message = f"Roads data successfully loaded into study '{username}_{study_name}'."
    else:
        message = f"Roads data failed to load into study '{username}_{study_name}': {error_message}"

    # some post-load checks
    if load_success:
        df = pd.read_sql(f"SELECT COUNT(*) AS count, SUM(ST_Length(geom))/1609.34 AS total_miles FROM inputs.roads_{un}_{sn};", eng)
        n_total = df["count"].iloc[0]
        total_miles = df["total_miles"].iloc[0]
        if n_total == 0:
            load_success = False
            message = "Roads data failed to load into study. No roads found in the study area."
        elif total_miles > 45000:
            load_success = False
            message = f"Roads data failed to load into study. Total roadway length {total_miles:,.0f} miles exceeds the maximum allowed of 45,000 miles."
            # drop the loaded roads table since it is too large to analyze
            with eng.begin() as conn:
                conn.execute(sqlalchemy.text(f"DROP TABLE IF EXISTS inputs.roads_{un}_{sn};"))

    end_time = pd.Timestamp.now()
    if load_success:
        print(f"Roads upload completed successfully at {end_time} (Duration: {end_time - start_time})")
    else:
        print(f"Roads upload failed at {end_time} (Duration: {end_time - start_time})")

    return load_success, message

def upload_crashes_formatted(username, study_name, store, id_col, year_col, mode_col, severity_col):
    "Upload formatted crashes to postgis inputs table"
    un = username
    sn = study_name
    max_total_crashes = 250000

    eng = db.get_database_engine()
    df = pd.read_sql(f"SELECT crs FROM gen_management.studies WHERE username = '{un}' AND study_name = '{sn}'", eng)
    crs = df["crs"].iloc[0]

    source = store.get("source")

    if source == "builtin":
        source_table = "static.fars_crashes"
        source_crs = 4326
        id_col = "year_st_case"
        year_col = "year"
        mode_col = "crash_mode"
        mode_recode = f"{recode_case_stmt(col=mode_col, recode_dict=FARS_CRASH_MODE_MAP, else_value='unknown mode')} AS crash_mode"
        severity_recode = "'K' AS crash_severity"
        severity_col = None  # all crashes are fatalities
        studies_crash_attr_sql = f"""
            crash_id_col = NULL,
            crash_year_col = NULL,
            crash_mode_col = NULL,
            crash_sev_col = NULL,
            crash_mode_map = '{json.dumps(FARS_CRASH_MODE_MAP)}'::jsonb,
            crash_sev_map = NULL,
        """
    elif source == "upload":
        source_table = store.get("staging_table")
        source_crs = store.get("staging_crs")
        id_col = id_col
        year_col = year_col
        mode_recode = f"{recode_case_stmt(col=mode_col, recode_dict=store.get('crash_mode_map'), else_value='unknown mode')} AS crash_mode"
        severity_recode = f"{recode_case_stmt(col=severity_col, recode_dict=store.get('crash_severity_map'), else_value='unknown severity')} AS crash_severity"
        studies_crash_attr_sql = f"""
            crash_id_col = '{id_col}',
            crash_year_col = '{year_col}',
            crash_mode_col = '{mode_col}',
            crash_sev_col = '{severity_col}',
            crash_mode_map = '{json.dumps(store.get("crash_mode_map"))}'::jsonb,
            crash_sev_map = '{json.dumps(store.get("crash_severity_map"))}'::jsonb,
        """
        
        # check for ID uniqueness
        is_unique, no_nulls = check_uniqueness(table=source_table, col=id_col)
        if not is_unique:
            return False, f"Crashes ID column '{id_col}' contains duplicate or NULL values. Please ensure all values are unique and non-null or select a different column."

    q = f"""
        DROP TABLE IF EXISTS _tmp_study_area_{un}_{sn};
        CREATE TEMP TABLE _tmp_study_area_{un}_{sn} AS (
            SELECT ST_Transform(geom, {source_crs}) AS geom FROM inputs.study_area_{un}_{sn}
        );
        CREATE INDEX ON _tmp_study_area_{un}_{sn} USING GIST(geom);
        ANALYZE _tmp_study_area_{un}_{sn};

        DROP TABLE IF EXISTS inputs.crashes_{un}_{sn};
        CREATE TABLE inputs.crashes_{un}_{sn} AS (
            SELECT
                "{id_col}" AS crash_id,
                "{year_col}"::INTEGER AS crash_year,
                {mode_recode},
                {severity_recode},
                ST_Transform(c.geom, {crs})::GEOMETRY(Point, {crs}) AS geom
            FROM
                {source_table} c
            WHERE
                EXISTS (
                    SELECT 1 
                    FROM _tmp_study_area_{un}_{sn} sa 
                    WHERE ST_Intersects(c.geom, sa.geom)
                )
        );

        ALTER TABLE inputs.crashes_{un}_{sn} ADD COLUMN pkey SERIAL PRIMARY KEY;
        CREATE INDEX IF NOT EXISTS idx_crashes_{un}_{sn}_geom ON inputs.crashes_{un}_{sn} USING GIST(geom);
        ANALYZE inputs.crashes_{un}_{sn};

        DROP TABLE IF EXISTS _tmp_study_area_{un}_{sn};

        -- update studies table with crashes info
        UPDATE gen_management.studies
        SET
            crash_data_source = '{source}',
            {studies_crash_attr_sql}
            crash_updated = NOW()
        WHERE 
            study_name = '{sn}' AND username = '{un}'
        ;

        DROP TABLE IF EXISTS received.crashes_{un}_{sn};
    """
    # if it is a user upload, move the staging table to received and clear staging
    if source == "upload":
        q += f"""
            CREATE TABLE received.crashes_{un}_{sn} AS (
                SELECT * FROM {source_table}
            );
            DROP TABLE IF EXISTS {source_table};
        """
    
    # update a flag to check if crash data does not have any non-fatal crashes
    non_fatal_sevs = [k for k in CRASH_SEVERITIES.keys() if k not in ['K', 'Omit From Analysis']]
    q += f"""
        UPDATE gen_management.studies
        SET crash_missing_non_fatal = NOT EXISTS (
            SELECT 1 
            FROM inputs.crashes_{un}_{sn} 
            WHERE crash_severity IN ({', '.join([f"'{sev}'" for sev in non_fatal_sevs])})
        )
        WHERE study_name = '{sn}' AND username = '{un}';
    """

    # print time 
    start_time = pd.Timestamp.now()
    print(f"Starting crashes upload for {un}_{sn} at {start_time}")
    try:
        with eng.begin() as conn:
            conn.execute(sqlalchemy.text(q))
        load_success = True
    except Exception as e:
        load_success = False
        error_message = str(e)
        print(f"Error occurred during crashes upload for {un}_{sn}: {e}")

    if load_success:
        message = f"Crashes data successfully loaded into study '{un}_{sn}'."
    else:
        message = f"Crashes data failed to load into study '{un}_{sn}' with the error: {error_message}"

    # some post-load checks
    if load_success:
        df = pd.read_sql(f"SELECT COUNT(*) AS count FROM inputs.crashes_{un}_{sn};", eng)
        n_total = df["count"].iloc[0]
        if n_total == 0:
            load_success = False
            message = "Crashes data failed to load into study. No crashes found in the study area."
        elif n_total > max_total_crashes:
            load_success = False
            message = (
                "Crashes data failed to load into study. "
                f"Total crashes {n_total:,} exceeds the maximum allowed of {max_total_crashes:,}."
            )
            # drop the loaded crashes table since it is too large to analyze
            with eng.begin() as conn:
                conn.execute(sqlalchemy.text(f"DROP TABLE IF EXISTS inputs.crashes_{un}_{sn};"))

    end_time = pd.Timestamp.now()
    if load_success:
        print(f"Crashes upload for {un}_{sn} completed successfully at {end_time} (Duration: {end_time - start_time})")
    else:
        print(f"Crashes upload for {un}_{sn} failed at {end_time} (Duration: {end_time - start_time})")

    return load_success, message

def get_crash_missing_non_fatal(username, study_name):
    "Check if the crashes data is missing non-fatal crashes"
    eng = db.get_database_engine()
    crash_missing_non_fatal = pd.read_sql(f"SELECT crash_missing_non_fatal FROM gen_management.studies \
                                           WHERE username = '{username}' AND study_name = '{study_name}';", eng)["crash_missing_non_fatal"].iloc[0]
    return crash_missing_non_fatal

def perform_data_checks(username, study_name):
    "Perform data validation checks on the loaded study area, roads, and crashes data."
    critical_issues = []
    info_msgs = []
    success_msgs = []

    un, sn = username, study_name
    
    # check if input tables exist
    table_names = {
        "study area": f"study_area_{un}_{sn}",
        "roads": f"roads_{un}_{sn}",
        "crashes": f"crashes_{un}_{sn}",
    }
    for input_type, tbl in table_names.items():
        if db.table_exists(table_name=tbl, schema="inputs"):
            success_msgs.append(f"Input data exists for {input_type}.")
        else:
            critical_issues.append(f"{input_type} does not exist. Please load the {input_type} data.")

    passed = True if len(critical_issues) == 0 else False

    if not passed:
        return passed, critical_issues, info_msgs, success_msgs

    eng = db.get_database_engine()
    # check intersection with study area
    # input data is already selected to include only roads in the study area, just checking counts here
    for input_type, tbl in table_names.items():
        if input_type == "study area":
            continue
        if pd.read_sql(f"SELECT COUNT(*) AS count FROM inputs.{tbl};", eng).iloc[0]["count"] == 0:
            critical_issues.append(f"None of your {input_type} data features intersect the study area. Please make sure you are using the study area, roads, and crashes for the same location.")
            passed = False
        else:
            success_msgs.append(f"Your {input_type} data intersects the study area.")

    if not passed:
        return passed, critical_issues, info_msgs, success_msgs
    
    # check crash years
    crash_years_df = pd.read_sql(f"SELECT crash_year::INT, count(*) FROM inputs.crashes_{un}_{sn} GROUP BY crash_year ORDER BY crash_year", eng)
    # if crash_year has a NULL value, get the count
    if crash_years_df[crash_years_df["crash_year"].isnull()].empty:
        null_year_count = 0
    else:
        null_year_count = crash_years_df[crash_years_df["crash_year"].isnull()]["count"].iloc[0]
    total_crashes = crash_years_df["count"].sum()
    max_year = int(crash_years_df["crash_year"].max())
    min_year = int(crash_years_df["crash_year"].min())
    crash_years = crash_years_df[crash_years_df["crash_year"].notnull()]["crash_year"].astype(int).tolist()
    # check to sure at least one year is present
    if len(crash_years) == 0:
        critical_issues.append("No valid crash years found. Please ensure the crash year column is correctly mapped and contains valid year values.")
        passed = False
    else:
        # check if there are any NULL years
        if null_year_count > 0:
            info_msgs.append(f"{null_year_count:,} out of {total_crashes:,} crashes have NULL crash year values and will be excluded from analysis.")
        # check if there are any crashes more than 5 years older than the most recent year
        if min_year <= max_year - 4:
            success_msgs.append(f"Your crash data spans from {min_year} to {max_year}, which meets the 5-year crash data required for analysis.")
        if min_year < max_year - 4:
            old_crash_count = crash_years_df[crash_years_df["crash_year"] < max_year - 4]["count"].sum()
            info_msgs.append(f"{old_crash_count:,} out of {total_crashes:,} crashes are more than 5 years older than the most recent crash year ({max_year}). \
                             Anything older than {max_year - 4} will be excluded from analysis.")
        # check if there is at least 5 years of data from the most recent year
        if min_year > max_year - 4:
            info_msgs.append(f"The analysis needs 5 years of crash data. Your data only has data from {min_year} to {max_year}. \
                             If you have additional years of data, please include them and re-upload. \
                             Otherwise, the analysis will proceed with the available years which may impact the results.")

    # warn if crash dataset is large enough to cause map visualization slowdowns
    if total_crashes > LARGE_CRASH_DATASET_THRESHOLD:
        info_msgs.append(
            f"Your crash dataset contains {total_crashes:,} crashes. "
            "The analysis will work normally, but map visualization in this app may be slow with large datasets."
        )

    # check if there are anny fclass, crash mode, or crash severity values that are not in the standardized set
    # shouldn't happen but just in case user tries multipel uploads and mappings are not updated correctly
    fclass_miscoded = pd.read_sql(f"SELECT COUNT(*) FROM inputs.roads_{un}_{sn} \
                                  WHERE functional_class NOT IN ({', '.join([f"'{fc}'" for fc in FUNCTIONAL_CLASSES])});", eng)["count"].iloc[0]
    if fclass_miscoded > 0:
        critical_issues.append(f"{fclass_miscoded:,} roads have functional class values that are not in the standardized set. \
                               Please re-upload the roads data and ensure the functional class mapping is correct.")
        passed = False
    else:
        success_msgs.append("All roads functional class values are valid.")

    crash_attr_miscoded = pd.read_sql(f"SELECT \
                                    COUNT(*) FILTER (WHERE crash_mode NOT IN ({', '.join([f"'{cm}'" for cm in CRASH_MODES.keys()])})) AS mode_miscoded, \
                                    COUNT(*) FILTER (WHERE crash_severity NOT IN ({', '.join([f"'{cs}'" for cs in CRASH_SEVERITIES.keys()])})) AS severity_miscoded \
                                FROM inputs.crashes_{un}_{sn};", eng)

    if crash_attr_miscoded["mode_miscoded"].iloc[0] > 0:
        critical_issues.append(f"{crash_attr_miscoded['mode_miscoded'].iloc[0]:,} crashes have crash mode values that are not in the standardized set. \
                               Please re-upload the crashes data and ensure the crash mode mapping is correct.")
        passed = False
    else:
        success_msgs.append("All crashes crash mode values are valid.")

    if crash_attr_miscoded["severity_miscoded"].iloc[0] > 0:
        critical_issues.append(f"{crash_attr_miscoded['severity_miscoded'].iloc[0]:,} crashes have crash severity values that are not in the standardized set. \
                               Please re-upload the crashes data and ensure the crash severity mapping is correct.")
        passed = False
    else:
        success_msgs.append("All crashes crash severity values are valid.")

    # if crash data is missing non-fatal crashes, add info message
    crash_missing_non_fatal = get_crash_missing_non_fatal(username=un, study_name=sn)
    if crash_missing_non_fatal:
        info_msgs.append("Your crash data does not contain any non-fatal crashes. \
                        The bayesian model results will not be calculated for non-fatal crashes. \
                        If you wish to estimate non-fatal crash results with this input, you can use the fatal severity model results and apply severity ratios. \
                        See the tool documentation for more details on the ratios.")

    return passed, critical_issues, info_msgs, success_msgs

def load_inputs_review_map(username, study_name,
                           fclass_color_dict=FCLASS_COLORS,
                           severity_color_dict=SEVERITY_COLORS,
                           max_crash_markers=MAX_MAP_CRASH_MARKERS):
    "Load the study area, roads, and crashes into the review map"
    un, sn = username, study_name
    # The review map keeps dedicated panes so crash points stay visually above
    # roads under canvas rendering. The main map visualization intentionally
    # avoids pane separation because separate canvas panes block road clicks.
    review_study_pane = "study_pane"
    review_roads_pane = "roads_pane"
    review_crashes_pane = "crashes_pane"

    # study layer (no symbology required other than outline)
    style = dict(color="#fcba03", weight=2, fillColor="#a6bddb", fillOpacity=0.35)
    study_area = {
        "type": "Feature",
        "properties": {},
        "geometry": get_dissolved_geojson(
            table=f"study_area_{un}_{sn}",
            schema="inputs"
        ),
    }
    study_layer = [dl.GeoJSON(
        data=study_area,
        id="study-geojson-layer",
        zoomToBounds=True,
        options=dict(
            pane=review_study_pane,
            style=style,
        ),
    )]

    eng = db.get_database_engine()
    # roads layer
    fclasses = [fc for fc in FUNCTIONAL_CLASSES if fc != "Omit From Analysis"]
    roads_gdf = pd.read_sql(f"""
                            SELECT road_id, road_name, functional_class, ST_AsGeoJSON(ST_Transform(geom, 4326)) AS geom_geojson
                            FROM inputs.roads_{un}_{sn}
                            WHERE functional_class IN ({', '.join([f"'{fc}'" for fc in fclasses])});
                            """, eng)
    ts_print(f"Loaded roads data for map review with {len(roads_gdf)} features")
    # Group features by functional class using vectorized dict construction
    fc_features = {}
    records = roads_gdf[["road_id", "road_name", "functional_class"]].to_dict("records")
    geom_jsons = roads_gdf["geom_geojson"].tolist()
    for rec, gj in zip(records, geom_jsons):
        fc = rec["functional_class"]
        fc_features.setdefault(fc, []).append({
            "type": "Feature",
            "properties": rec,
            "geometry": json.loads(gj),
        })
    ts_print(f"Processed roads data for map review into {len(fc_features)} functional class layers")
    roads_layer = []
    for fc, features in fc_features.items():
        roads_layer.append(
            dl.GeoJSON(
                data={"type": "FeatureCollection", "features": features},
                options=dict(
                    pane=review_roads_pane,
                    style=dict(
                        color=fclass_color_dict.get(fc, "#000000"),
                        weight=2,
                    ),
                ),
                hoverStyle=dict(weight=5, color="#666666", dashArray=""),
                zoomToBounds=False
            )
        )

    # crashes
    severity_levels = [sv for sv in CRASH_SEVERITIES.keys() if sv != "Omit From Analysis"]
    # need crashes as lat/lon to use with leaflet
    crashes_where = f"""
        crash_severity IN ({', '.join([f"'{sv}'" for sv in severity_levels])})
        AND
        crash_mode IN ({', '.join([f"'{cm}'" for cm in CRASH_MODES.keys() if cm != "Omit From Analysis"])})
    """
    crash_count = int(pd.read_sql(f"""
                            SELECT COUNT(*) AS count
                            FROM inputs.crashes_{un}_{sn}
                            WHERE {crashes_where};
                            """, eng)["count"].iloc[0])

    limit_clause = ""
    truncation_meta = None
    if max_crash_markers is not None and crash_count > max_crash_markers:
        ts_print(
            f"Crash map review: {crash_count} crashes found, rendering latest {max_crash_markers} markers to avoid browser stack overflow"
        )
        limit_clause = f"ORDER BY crash_year DESC NULLS LAST, crash_id DESC LIMIT {max_crash_markers}"
        truncation_meta = {
            "total": crash_count,
            "rendered": max_crash_markers,
            "max": max_crash_markers,
        }

    crashes_gdf = pd.read_sql(f"""
                            SELECT 
                                crash_id, crash_year, crash_mode, crash_severity, 
                                ST_Y(ST_Transform(geom, 4326)) AS lat, ST_X(ST_Transform(geom, 4326)) AS lon
                            FROM inputs.crashes_{un}_{sn}
                            WHERE {crashes_where}
                            {limit_clause}
                            """, eng)
    ts_print(f"Loaded crashes data for map review with {len(crashes_gdf)} features")
    # Build GeoJSON FeatureCollection via vectorized dict construction
    crashes_gdf["fillColor"] = crashes_gdf["crash_severity"].map(severity_color_dict).fillna("#000000")
    # Sort so lower severities draw first and K (fatal) draws on top
    severity_draw_order = {s: i for i, s in enumerate(reversed(SEVERITY_ORDER))}
    crashes_gdf = crashes_gdf.sort_values(
        by="crash_severity",
        key=lambda col: col.map(severity_draw_order),
        na_position="first",
    )
    props = crashes_gdf[["crash_id", "crash_year", "crash_mode", "crash_severity", "fillColor"]].to_dict("records")
    for rec in props:
        rec["_pane"] = review_crashes_pane
    lons = crashes_gdf["lon"].to_numpy()
    lats = crashes_gdf["lat"].to_numpy()
    crash_features = [
        {
            "type": "Feature",
            "geometry": {"type": "Point", "coordinates": [float(lon), float(lat)]},
            "properties": rec,
        }
        for rec, lon, lat in zip(props, lons, lats)
    ]

    crashes_layer = [dl.GeoJSON(
        data={"type": "FeatureCollection", "features": crash_features},
        id="crashes-geojson",
        options=dict(
            pane=review_crashes_pane,
            pointToLayer={"variable": "sspf_crashPointToLayer"},
            onEachFeature={"variable": "sspf_onEachFeatureCrashPopup"},
        ),
    )]

    return study_layer, roads_layer, crashes_layer, truncation_meta
