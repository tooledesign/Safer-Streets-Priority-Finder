from . import db, load as ld, gen
import sqlalchemy
import dash_leaflet as dl
import dash_bootstrap_components as dbc
from dash import html, dcc
import time
from datetime import timedelta
import pandas as pd
import geopandas as gpd
from ..global_vars import *
from .crash_risk_estimation.config import Config
from .mapping import build_color_scheme

analysis_cols = [
            # (col_name, pandas_data_type, default_values)
            ("sliding_window_length", "float", WINDOW_LENGTHS["Sliding"]),
            ("window_length_ratio", "float", WINDOW_LENGTH_RATIO),
            ("short_window_length", "float", WINDOW_LENGTHS["Sliding"] / WINDOW_LENGTH_RATIO),
            ("crash_weights", "dict", CRASH_WEIGHTS),
            ("crash_costs", "dict", CRASH_COSTS),
            ("crash_join_distance", "float", CRASH_JOIN_DIST),
            ("top_streets_threshold", "int", NUM_STREETS),
        ]

# object to retrieve analysis parameters from the database and provide defaults
def get_analysis_params(study_id=None, cols=analysis_cols):
    """
    Retrieve analysis parameters from the database for the given study_id.
    Also retrieve default values from global variables.
    """
    
    params = {"default": {c[0]: c[2] for c in cols}}
    if study_id:
        q = f"""
        SELECT {', '.join([c[0] for c in cols])}
        FROM gen_management.studies
        WHERE study_id = {study_id}
        """
        eng = db.get_database_engine()
        df = pd.read_sql(sql=q, con=eng)
        if df.empty:
            return None
        params["current"] = {c[0]: (df.iloc[0][c[0]] if pd.notnull(df.iloc[0][c[0]]) else None if c[1] != "dict" else {}) for c in cols}
    else:
        params["current"] = {c[0]: None if c[1] != "dict" else {} for c in cols}
    return params

def get_pdo_crash_cost(study_id):
    "Get the cost of a Property Damage Only (PDO) crash for the given study_id."
    pdo_crash_cost = get_analysis_params(study_id)["current"].get("crash_costs", CRASH_COSTS)["O"]
    return pdo_crash_cost

def create_short_windows(study_id, output_schema="scratch", create_model_0p05mi_segments=False):
    """
    Create short windows for the given study_id. Returns the name of the schema qualified output table.
    Optionally also creates a 0.05 mile short segments used in the model.
    """
    un, sn = db.get_user_and_study_names(study_id)

    # Study Parameters
    analysis_params = get_analysis_params(study_id=study_id)["current"]
    sliding_window_length = analysis_params.get("sliding_window_length", WINDOW_LENGTHS["Sliding"]) * 1609.34  # in meters
    window_length_ratio = analysis_params.get("window_length_ratio", WINDOW_LENGTH_RATIO)
    short_window_length = sliding_window_length / window_length_ratio
    step_length = short_window_length

    # Table names
    input_roads_table = f"inputs.roads_{un}_{sn}"
    dissolved_table = f"scratch.ssm_dissolved_roads_{un}_{sn}"
    output_short_windows_table = f"{output_schema}.ssm_short_windows_{un}_{sn}"
    query_parts = {}
    query_parts["1. Dissolve Roads"] = _dissolve_roads_query(input_roads_table=input_roads_table, 
                                                             dissolved_table=dissolved_table,
                                                             sliding_window_length=sliding_window_length,
                                                             step_length=step_length)
    query_parts["2. Create Short Windows"] = _create_short_windows_query(dissolved_table=dissolved_table,
                                                                          output_short_windows_table=output_short_windows_table,
                                                                          short_window_length=short_window_length)
    
    for step in ["1. Dissolve Roads", "2. Create Short Windows"]:
        db.execute_query(query_parts[step])
    if create_model_0p05mi_segments:
        # create 0.05mi short segments for model processing (not used in the sliding windows analysis but used in the model itself)
        db.execute_query(
            _create_short_windows_query(dissolved_table=dissolved_table,
                                        output_short_windows_table=f"{output_schema}.ssm_0p05mi_segments_{un}_{sn}",
                                        short_window_length=0.05 * 1609.34,
                                        window_pkey_col="short_0p05mi_window_id")
        )
        return (f"{output_short_windows_table}", f"{output_schema}.ssm_0p05mi_segments_{un}_{sn}")
    else:
        return f"{output_short_windows_table}"


def _ensure_study_centroid(study_id):
    """
    Ensure the study_centroid_4326 column is populated in the studies table.
    If not, calculate it from the study area table.
    """
    eng = db.get_database_engine()
    # Check if centroid exists
    check_q = f"SELECT study_centroid_4326 FROM gen_management.studies WHERE study_id = {study_id}"
    df = pd.read_sql(check_q, eng)
    
    if df.empty:
            raise ValueError(f"Study {study_id} not found.")
            
    if pd.isnull(df.iloc[0]['study_centroid_4326']):
            un, sn = db.get_user_and_study_names(study_id)
            table_name = f"inputs.study_area_{un}_{sn}"
            
            # Calculate and update
            update_q = f"""
            UPDATE gen_management.studies
            SET study_centroid_4326 = (
                SELECT ST_SetSRID(ST_Centroid(ST_Union(ST_Transform(geom, 4326))), 4326)
                FROM {table_name}
            )
            WHERE study_id = {study_id}
            """
            with eng.begin() as conn:
                conn.execute(sqlalchemy.text(update_q))


def _get_study_state(study_id) -> str:
    """
    Determine the state of the study area based on its centroid.
    """
    _ensure_study_centroid(study_id)

    un, sn = db.get_user_and_study_names(study_id)

    q = f"""
    SELECT s.stusps
    FROM static.us_states s
    JOIN gen_management.studies st ON ST_Intersects(s.geom, st.study_centroid_4326)
    WHERE st.study_id = {study_id}
    """
    eng = db.get_database_engine()
    df = pd.read_sql(sql=q, con=eng)

    if df.empty:
        # fallback: try to get the state by area intersection
        q = f"""  
        SELECT s.stusps 
        FROM static.us_states s  
        JOIN inputs.study_area_{un}_{sn} st ON ST_Intersects(s.geom ,ST_Transform(st.geom, 4326))  
        ORDER BY ST_Area(ST_Intersection(s.geom, ST_Transform(st.geom, 4326))) DESC  
        LIMIT 1  
        """

    if df.empty:
        # if still empty, raise error
        raise ValueError(f"Study {study_id} centroid does not fall within any US state.")

    return df.iloc[0]['stusps']


def create_ssma_config(study_id: str, analysis_params: dict, short_windows_table: str, segments_0p05mi_table: str = None) -> Config:
    """
    Create a Config object for the SSPF crash risk estimation pipeline.
    """
    eng = db.get_database_engine()
    user_name, study_name = db.get_user_and_study_names(study_id)
    study_crs = db.get_study_crs(study_id)
    bayesian_decay, bayesian_max_prior_weight = db.get_bayesian_params(study_id)

    # build input table names
    input_roadway_table = short_windows_table
    input_bg_fatal_table = f"inputs.block_group_fatals_{user_name}_{study_name}"
    input_crash_table = f"inputs.crashes_{user_name}_{study_name}"
    study_area_table = f"inputs.study_area_{user_name}_{study_name}"
    dissolved_roads_table = f"scratch.ssm_dissolved_roads_{user_name}_{study_name}"

    # build debug table names
    roadway_assignment_debug_table = f"roadway_assignment_debug_{user_name}_{study_name}"
    crash_sjoin_debug_table = f"crash_sjoin_debug_{user_name}_{study_name}"
    bayesian_update_debug_table = f"bayesian_update_debug_{user_name}_{study_name}"

    # build output table names
    output_schema = "outputs"
    fatal_estimates_table = f"fatal_estimates_{user_name}_{study_name}"
    all_severity_estimates_table = f"all_severity_estimates_{user_name}_{study_name}"
    output_observed_crash_counts_table = f"observed_crash_counts_{user_name}_{study_name}"
    bayesian_estimates_table = f"bayesian_estimates_{user_name}_{study_name}"

    return Config(
        db_engine=eng,
        user_name=user_name,
        study_name=study_name,
        study_crs=study_crs,
        state_name=_get_study_state(study_id),
        study_area_table=study_area_table,
        input_roadway_table=input_roadway_table,
        input_bg_fatal_table=input_bg_fatal_table,
        input_crash_table=input_crash_table,
        short_window_pk="short_window_id",
        mode_col_name="crash_mode",
        mode_mapping={
            'ped': ['ped'],
            'bike': ['bike'],
            'mv': ['mv'],
        },
        severity_col_name="crash_severity",
        severity_mapping={
            'k': 'K',
            'a': 'A',
            'b': 'B',
            'c': 'C',
            'o': 'O',
        },
        sjoin_dist=analysis_params["crash_join_distance"],
        local_crash_study_period=5,
        max_est_weight=bayesian_max_prior_weight if bayesian_max_prior_weight is not None else 0.8,
        decay=bayesian_decay if bayesian_decay is not None else 1.0,
        output_schema=output_schema,
        fatal_estimates_table=fatal_estimates_table,
        all_severity_estimates_table=all_severity_estimates_table,
        output_observed_crash_counts_table=output_observed_crash_counts_table,
        bayesian_estimates_table=bayesian_estimates_table,
        static_schema="static",
        fatal_rate_table="national_fatal_rates_by_state",
        severity_ratio_table="severity_ratio_by_state",
        roadway_assignment_debug_table=roadway_assignment_debug_table,
        crash_sjoin_debug_table=crash_sjoin_debug_table,
        bayesian_update_debug_table=bayesian_update_debug_table,
        crash_missing_non_fatal=ld.get_crash_missing_non_fatal(user_name, study_name),
        input_segments_0p05mi_table=segments_0p05mi_table,
    )

def run_sliding_windows_analysis(study_id):
    """
    Run the sliding windows analysis for the given study_id. 
    Returns the name of the schema qualified output table.
    """
    output_message = ""
    run_success = False

    start_time = time.time()
    un, sn = db.get_user_and_study_names(study_id)

    # Study Parameters
    analysis_params = get_analysis_params(study_id=study_id)["current"]
    sliding_window_length = analysis_params.get("sliding_window_length", WINDOW_LENGTHS["Sliding"]) * 1609.34  # in meters
    window_length_ratio = analysis_params.get("window_length_ratio", WINDOW_LENGTH_RATIO)
    short_window_length = sliding_window_length / window_length_ratio
    step_length = short_window_length

    crash_join_distance = analysis_params.get("crash_join_distance", CRASH_JOIN_DIST) * 0.3048  # convert to meters
    crash_weights = analysis_params.get("crash_weights", CRASH_WEIGHTS)
    crs = db.get_study_crs(study_id)

    # Table names
    standard_table_names = db.get_study_table_names(username=un, study_name=sn)
    input_roads_table = standard_table_names["input_roads"]
    input_crashes_table = standard_table_names["input_crashes"]
    sliding_windows_table = standard_table_names["sliding_windows_table"]
    output_short_windows_table = standard_table_names["output_swa_short_windows"]
    scratch_crashes_table = f"scratch.swa_crashes_{un}_{sn}"
    scratch_roads_dissolve_table = f"scratch.swa_dissolved_roads_{un}_{sn}"
    
    query_parts = {}

    # Step 1: Dissolve roads by name and functional class
    query_parts["1. Dissolve Roads"] = _dissolve_roads_query(input_roads_table=input_roads_table, 
                                                            dissolved_table=scratch_roads_dissolve_table,
                                                            sliding_window_length=sliding_window_length,
                                                            step_length=step_length)
    
    # Step 2: Create sliding windows
    query_parts["2. Create Sliding Windows"] = _create_sliding_windows_query(dissolved_table=scratch_roads_dissolve_table,
                                                                             sliding_windows_table=sliding_windows_table,
                                                                             sliding_window_length=sliding_window_length,
                                                                             step_length=step_length,
                                                                             crs=crs)

    # Step 3: Create scratch crashes table
    query_parts["3. Create Scratch Crashes"] = f"""
        DROP TABLE IF EXISTS {scratch_crashes_table};
        CREATE TABLE {scratch_crashes_table} AS (
            SELECT
                *
            FROM
                {input_crashes_table}
            WHERE
                crash_severity != 'Omit From Analysis'
                AND
                crash_mode != 'Omit From Analysis'
                AND
                crash_year >= (SELECT MAX(crash_year) - 4 FROM {input_crashes_table})
        );

        CREATE INDEX ON {scratch_crashes_table} USING GIST (geom);
        CREATE INDEX ON {scratch_crashes_table} (crash_severity);
        CREATE INDEX ON {scratch_crashes_table} (crash_mode);
        ANALYZE {scratch_crashes_table};
    """

    # Step 4: Assign crash counts to sliding windows (anything within crash_join_distance)
    modes = [m.lower() for m in CRASH_MODES.keys() if m != "Omit From Analysis"]
    severities = [s.lower() for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"]

    # build sql
    cols = [f"{m}_{s}" for m in modes for s in severities]

    # add mode_sev columns
    add_cols_sql = ",\n".join([f"ADD COLUMN {col} INT DEFAULT 0" for col in cols])
    # add crash score cols
    add_crash_score_cols_sql = ",\n".join([f"ADD COLUMN {m}_crash_score INT DEFAULT 0" for m in modes])

    set_count_sql = ",\n".join([f"{m}_{s.lower()} = COALESCE(sub.{m}_{s}, 0)" for m in modes for s in severities])
    set_score_sql = ",\n".join([f"{m}_crash_score = " + " + ".join([f"(COALESCE(sub.{m}_{s}, 0) * {crash_weights[s.upper()]})" for s in severities]) for m in modes])
    count_sql = ",\n".join([f"COUNT(c.*) FILTER (WHERE c.crash_mode = '{m}' AND c.crash_severity = '{s.upper()}') AS {m}_{s}" for m in modes for s in severities])

    query_parts["4. Assign Crash Counts to Sliding Windows"] = f"""
        ALTER TABLE {sliding_windows_table}
        {add_cols_sql},
        {add_crash_score_cols_sql}
        ;

        UPDATE
            {sliding_windows_table} sw
        SET
            {set_count_sql},
            {set_score_sql}
        FROM 
            (
                SELECT
                    sw.sliding_window_id,
                    {count_sql}
                FROM
                    {sliding_windows_table} sw,
                    {scratch_crashes_table} c
                WHERE
                    ST_DWithin(sw.geom, c.geom, {crash_join_distance})
                GROUP BY
                    sw.sliding_window_id
            ) AS sub
        WHERE
            sw.sliding_window_id = sub.sliding_window_id
        ;
    """

    # Step 5 Create short windows table (subset of roads)
    query_parts["5. Create Short Windows"] = _create_short_windows_query(dissolved_table=scratch_roads_dissolve_table,
                                                                         output_short_windows_table=output_short_windows_table,
                                                                         short_window_length=short_window_length)

    # Step 6 Add crash scores to the short windows table based on the max overlapping sliding windows
    set_score_sql = ",\n".join([f"{m}_crash_score = COALESCE(sub.max_{m}_score, 0)" for m in modes])
    max_score_sql = ",\n".join([f"MAX(sw.{m}_crash_score) AS max_{m}_score" for m in modes])
    query_parts["6. Assign Crash Scores to Short Windows"] = f"""
        ALTER TABLE {output_short_windows_table}
        {add_crash_score_cols_sql}
        ;

        -- index on dissolved_pkey for faster joins
        CREATE INDEX ON {sliding_windows_table} USING BTREE(dissolved_pkey);
        ANALYZE {sliding_windows_table};
        CREATE INDEX ON {output_short_windows_table} USING BTREE(dissolved_pkey);
        ANALYZE {output_short_windows_table};

        UPDATE
            {output_short_windows_table} shw
        SET
            {set_score_sql}
        FROM
            (
                SELECT
                    shw.short_window_id,
                    {max_score_sql}
                FROM
                    {output_short_windows_table} shw,
                    {sliding_windows_table} sw
                WHERE
                    shw.dissolved_pkey = sw.dissolved_pkey
                    AND
                    -- at least one meter of overlap between the short window and sliding window
                    -- this might skip some end slivers which are shorter than a meter, that's ok for the analysis.
                    (LEAST(shw.end_distance_m, sw.end_distance_m) - GREATEST(shw.start_distance_m, sw.start_distance_m)) >= 1
                GROUP BY
                    shw.short_window_id
            ) AS sub
        WHERE
            shw.short_window_id = sub.short_window_id
        ;
    """
    
    for step in ["1. Dissolve Roads", 
                 "2. Create Sliding Windows", 
                 "3. Create Scratch Crashes", 
                 "4. Assign Crash Counts to Sliding Windows",
                 "5. Create Short Windows",
                 "6. Assign Crash Scores to Short Windows"]:
        print(f"Running step: {step}...")
        run_success, error_msg, _ = db.execute_query(query=query_parts[step])
        if not run_success:
            print(f"Error in step '{step}': {error_msg}")
            return False, f"Error in step '{step}': {error_msg}"
        else:
            output_message += f"{step} completed successfully.\n"
    
    elapsed = time.time() - start_time
    output_message += f"Sliding windows analysis completed in {timedelta(seconds=elapsed)}.\n"
    print(f"Sliding windows analysis for study name '{un}_{sn}' completed in {timedelta(seconds=elapsed)}.")

    return True, output_message.strip()

def add_crash_costs(study_id):
    """
    Add crash costs to the given estimates table for the given study_id.
    """
    un, sn = db.get_user_and_study_names(study_id)
    model_output_table = f"outputs.bayesian_estimates_{un}_{sn}"
    
    # get crash cost values
    crash_costs=get_analysis_params(study_id)["current"].get("crash_costs", CRASH_COSTS)
    
    # check is non-fatal severity is to be skipped
    crash_missing_non_fatal = pd.read_sql(f"SELECT crash_missing_non_fatal FROM gen_management.studies \
                                           WHERE username = '{un}' AND study_name = '{sn}';", db.get_database_engine())["crash_missing_non_fatal"].iloc[0]
    if crash_missing_non_fatal:
        crash_costs = {k.lower(): v for k, v in crash_costs.items() if k == "K"}
    else:
        crash_costs = {k.lower(): v for k, v in crash_costs.items()}
    
    # build sql to add cost columns for each mode
    crash_modes = [m.lower() for m in CRASH_MODES.keys() if m not in ["Omit From Analysis", "other"]]
    add_cost_cols_sql = ",\n".join([f"ADD COLUMN IF NOT EXISTS total_{m}_cost FLOAT" for m in crash_modes])
    mode_cost_sql_list = []
    for m in crash_modes:
        mode_cost_sql_list.append(f"total_{m}_cost = " + " + ".join([f"(adj_{m}_{s} * {s_cost})" for s, s_cost in crash_costs.items()]))
    set_cost_sql = ",\n".join(mode_cost_sql_list)

    query = f"""
        ALTER TABLE {model_output_table}
        {add_cost_cols_sql};

        UPDATE {model_output_table}
        SET {set_cost_sql};
    """
    db.execute_query(query)

def get_sliding_window_results(study_id):
    """Load SWA results as a GeoDataFrame with all mode scores sharing geometry."""
    un, sn = db.get_user_and_study_names(study_id)
    eng = db.get_database_engine()

    swa_results_table = f"outputs.swa_short_windows_{un}_{sn}"

    if not db.table_exists(table_name=f"swa_short_windows_{un}_{sn}", schema="outputs"):
        return gpd.GeoDataFrame()

    modes = [k for k in CRASH_MODES.keys() if k not in ["Omit From Analysis", "other"]]
    score_cols = ", ".join(f"{m}_crash_score" for m in modes)
    show_cols = ", ".join(f"COALESCE({m}_crash_score, 0) > 0 AS show_{m}" for m in modes)
    where_clause = " OR ".join(f"COALESCE({m}_crash_score, 0) > 0" for m in modes)

    gdf = gpd.read_postgis(f"""
        SELECT
            INITCAP(road_name) AS road_name,
            functional_class,
            {score_cols},
            {show_cols},
            ST_Transform(
                ST_Simplify((ST_Dump(ST_LineMerge(ST_Union(geom)))).geom, 10, true),
                4326
            ) AS geom
        FROM
            {swa_results_table}
        WHERE
            {where_clause}
        GROUP BY
            road_name, functional_class, {score_cols}
    """, eng, geom_col="geom")

    return gdf

def get_safer_streets_model_results(study_id):
    """Load SSM results as a GeoDataFrame with all mode costs sharing geometry."""
    un, sn = db.get_user_and_study_names(study_id)
    eng = db.get_database_engine()

    min_cost_threshold = get_pdo_crash_cost(study_id)
    ssma_results_table = f"outputs.bayesian_estimates_{un}_{sn}"

    if not db.table_exists(table_name=f"bayesian_estimates_{un}_{sn}", schema="outputs"):
        return gpd.GeoDataFrame()

    modes = [k for k in CRASH_MODES.keys() if k not in ["Omit From Analysis", "other"]]
    cost_cols = ", ".join(f"total_{m}_cost::INT AS {m}_cost" for m in modes)
    show_cols = ", ".join(f"COALESCE(total_{m}_cost, 0) > {min_cost_threshold} AS show_{m}" for m in modes)
    where_clause = " OR ".join(f"COALESCE(total_{m}_cost, 0) > {min_cost_threshold}" for m in modes)

    gdf = gpd.read_postgis(f"""
        SELECT
            INITCAP(road_name) AS road_name,
            functional_class,
            {cost_cols},
            {show_cols},
            ST_Transform(ST_Simplify(geom, 10, true), 4326) AS geom
        FROM
            {ssma_results_table}
        WHERE
            {where_clause}
    """, eng, geom_col="geom")

    return gdf


def get_observed_vs_estimated_by_mode(study_id: int, mode: str):
    """Return observed vs modeled crash counts by severity for a given mode."""

    mode = (mode or "").lower()
    valid_modes = [m for m in CRASH_MODES.keys() if m not in ["Omit From Analysis", "other"]]
    if mode not in valid_modes:
        raise ValueError(f"Unsupported mode '{mode}'.")

    un, sn = db.get_user_and_study_names(study_id)
    if not un or not sn:
        return pd.DataFrame(columns=["Severity", "Observed Crashes", "Modeled Adjusted Crashes"])

    table_exists = db.table_exists(f"bayesian_estimates_{un}_{sn}", "outputs")
    if not table_exists:
        return pd.DataFrame(columns=["Severity", "Observed Crashes", "Modeled Adjusted Crashes"])

    eng = db.get_database_engine()
    inspector = sqlalchemy.inspect(eng)
    try:
        table_columns = {
            col["name"]
            for col in inspector.get_columns(
                f"bayesian_estimates_{un}_{sn}", schema="outputs"
            )
        }
    except sqlalchemy.exc.NoSuchTableError:
        return pd.DataFrame(columns=["Severity", "Observed Crashes", "Modeled Adjusted Crashes"])

    severity_codes = [s for s in SEVERITY_ORDER if s in CRASH_SEVERITIES]
    select_parts: list[str] = []
    available_severities: list[str] = []
    for sev in severity_codes:
        sev_lower = sev.lower()
        obs_col = f"{mode}_{sev_lower}_obs"
        adj_col = f"adj_{mode}_{sev_lower}"
        if obs_col not in table_columns or adj_col not in table_columns:
            continue
        select_parts.append(
            f"SUM(COALESCE({obs_col}, 0)) AS observed_{sev_lower}"
        )
        select_parts.append(
            f"SUM(COALESCE({adj_col}, 0)) AS adjusted_{sev_lower}"
        )
        available_severities.append(sev)

    if not select_parts:
        return pd.DataFrame(columns=["Severity", "Observed Crashes", "Modeled Adjusted Crashes"])

    sql = f"""
        SELECT {', '.join(select_parts)}
        FROM outputs.bayesian_estimates_{un}_{sn}
    """

    summary = pd.read_sql(sqlalchemy.text(sql), eng)
    if summary.empty:
        return pd.DataFrame(columns=["Severity", "Observed Crashes", "Modeled Adjusted Crashes"])

    records = []
    row = summary.iloc[0]
    for sev in available_severities:
        sev_lower = sev.lower()
        records.append(
            {
                "Severity": CRASH_SEVERITIES.get(sev, sev),
                "Observed Crashes": float(row.get(f"observed_{sev_lower}", 0)) or 0,
                "Modeled Adjusted Crashes": float(row.get(f"adjusted_{sev_lower}", 0)) or 0,
            }
        )

    return pd.DataFrame.from_records(records)

def _dissolve_roads_query(input_roads_table: str, dissolved_table: str,
                          sliding_window_length: float, step_length: float) -> str:
    """
    Dissolve roads by name and functional class. Returns SQL query string.
    The names must be schema qualified.
    """
    return f"""
        DROP TABLE IF EXISTS {dissolved_table};
        CREATE TABLE {dissolved_table} AS (
            SELECT
                LOWER(a.road_name) AS road_name,
            functional_class,
            (ST_DUMP(ST_LINEMERGE(ST_UnaryUnion(UNNEST(ST_ClusterWithin(a.geom, 5)))))).geom as geom
        FROM
            {input_roads_table} a
        WHERE
            functional_class != 'Omit From Analysis'
        GROUP BY
            road_name, functional_class
        );

        ALTER TABLE {dissolved_table}
            ADD COLUMN dissolved_pkey SERIAL PRIMARY KEY,
            ADD COLUMN IF NOT EXISTS num_windows INT,
            ADD COLUMN IF NOT EXISTS length_meters FLOAT;

        UPDATE {dissolved_table}
        SET
            num_windows = 
                CASE 
                    WHEN ST_Length(geom) <= {sliding_window_length} THEN 1
                    ELSE CEIL((ST_Length(geom) - {sliding_window_length}) / {step_length}) + 1
                END,
            length_meters = ST_Length(geom)
        ;

        DELETE FROM {dissolved_table} WHERE ST_Length(geom) = 0;

        CREATE INDEX ON {dissolved_table} USING GIST (geom);
        ANALYZE {dissolved_table};
    """

def _create_sliding_windows_query(dissolved_table: str, sliding_windows_table: str, sliding_window_length: float, step_length: float, crs: int) -> str:
    """
    Create sliding windows from dissolved roads table. Returns SQL query string.
    The names must be schema qualified.
    """
    return f"""
        DROP TABLE IF EXISTS {sliding_windows_table};
        CREATE TABLE {sliding_windows_table} (
            sliding_window_id SERIAL PRIMARY KEY,
            geom GEOMETRY(LINESTRING, {crs}),
            road_name TEXT,
            functional_class TEXT,
        start_distance_m FLOAT,
        end_distance_m FLOAT,
        length FLOAT,
        dissolved_pkey INT NOT NULL
        );

        INSERT INTO {sliding_windows_table} (
            road_name,
            functional_class,
            geom,
            start_distance_m,
            end_distance_m,
            dissolved_pkey
        )
            SELECT
                road_name,
                functional_class,
                ST_LineMerge(
                    ST_LineSubstring(
                        geom,
                        {step_length} * n / length_meters,
                        CASE
                            WHEN ({sliding_window_length} + ({step_length} * n)) < length_meters
                            THEN ({sliding_window_length} + ({step_length} * n)) / length_meters
                            ELSE 1
                        END
                    )
                ) AS geom,
                {step_length} * n AS start_distance_m,
                CASE
                    WHEN ({sliding_window_length} + ({step_length} * n)) < length_meters
                    THEN ({sliding_window_length} + ({step_length} * n))
                    ELSE length_meters
                END AS end_distance_m,
                dissolved_pkey
            FROM
                {dissolved_table} CROSS JOIN LATERAL generate_series(0, num_windows-1) AS n   
        ;

        UPDATE {sliding_windows_table} SET length = ST_Length(geom);

        CREATE INDEX ON {sliding_windows_table} USING GIST(geom);
        ANALYZE {sliding_windows_table};
    """

def _create_short_windows_query(dissolved_table: str, 
                                output_short_windows_table: str, 
                                short_window_length: float,
                                window_pkey_col="short_window_id") -> str:
    """
    Create short windows from sliding windows table. Returns SQL query string.
    The names must be schema qualified.
    """
    return f"""
        DROP TABLE IF EXISTS {output_short_windows_table};
        CREATE TABLE {output_short_windows_table} AS (
            SELECT
                dissolved_pkey,
                road_name,
                functional_class,
                n AS segment_sequence,
                ROUND(({short_window_length} * n)::numeric, 1) AS start_distance_m,
                ROUND((
                    CASE
                        WHEN {short_window_length} * (n + 1) < length
                        THEN {short_window_length} * (n + 1)
                        ELSE length
                    END
                )::numeric, 1) AS end_distance_m,
                ST_LineSubstring(input_geom, {short_window_length} * n/length,
                    CASE
                            WHEN {short_window_length} * (n + 1) < length
                            THEN {short_window_length} * (n + 1) / length
                        ELSE 1
                    END
                ) AS geom
            FROM (
                SELECT
                    *,
                    (ST_DUMP(ST_LineMerge(geom))).geom AS input_geom,
                    ST_Length((ST_DUMP(ST_LineMerge(geom))).geom) AS length
                FROM
                    {dissolved_table}
                ) t
                CROSS JOIN
                generate_series(0, (SELECT ((MAX(ST_Length(geom)) / {short_window_length}) + 1)::INT FROM {dissolved_table})) AS n
                WHERE
                    n * {short_window_length}/length < 1
                    AND
                    t.length > 0
        );

        ALTER TABLE {output_short_windows_table}
            ALTER COLUMN dissolved_pkey SET NOT NULL,
            ADD COLUMN {window_pkey_col} SERIAL PRIMARY KEY;
        CREATE INDEX ON {output_short_windows_table} USING GIST (geom);
        ANALYZE {output_short_windows_table};
    """

def analysis_results_layers(study_id, analysis_type):
    """
    Create a Leaflet map layers for the analysis results.
    study_id: int, the study id to load the analysis results for (if initialize is False)
    Returns: (study_layer, bike_layer, ped_layer, mv_layer, legend_data)
    where legend_data is a dict with mode -> {bins, colors, labels}
    """
    if analysis_type not in ["sliding-windows", "safer-streets-model"]:
        raise ValueError("Invalid analysis_type. Must be 'sliding-windows' or 'safer-streets-model'.")

    study_layer_cfg = SWA_MAP_CONFIG.layers.get("study_area")
    study_style = study_layer_cfg.feature_style.to_dict() if study_layer_cfg else {}
    un, sn = db.get_user_and_study_names(study_id=study_id)
    study_area_geojson = ld.get_dissolved_geojson(table=f"study_area_{un}_{sn}", schema="inputs")
    study_area = {
        "type": "Feature",
        "properties": {},
        "geometry": study_area_geojson,
    }
    study_layer = [dl.GeoJSON(data=study_area, zoomToBounds=True, options=dict(style=study_style))]

    if analysis_type == "sliding-windows":
        results_gdf = get_sliding_window_results(study_id)
        score_col_suffix = "crash_score"
    elif analysis_type == "safer-streets-model":
        results_gdf = get_safer_streets_model_results(study_id)
        score_col_suffix = "cost"

    mode_style_config = {}
    legend_data = {}

    if results_gdf.empty:
        return study_layer, {"type": "FeatureCollection", "features": []}, mode_style_config, legend_data

    modes = [k for k in CRASH_MODES.keys() if k not in ["Omit From Analysis", "other"]]

    results_gdf = results_gdf.copy()
    results_gdf["Road Name"] = results_gdf["road_name"]
    results_gdf["Functional Class"] = results_gdf["functional_class"]
    prop_cols = ["Road Name", "Functional Class"]

    for mode in modes:
        score_col = f"{mode}_{score_col_suffix}"
        if score_col not in results_gdf.columns:
            continue

        show_col = f"show_{mode}"
        mode_gdf = results_gdf[results_gdf[show_col]]
        if mode_gdf.empty:
            continue

        layer_cfg = SWA_MAP_CONFIG.layers.get(mode)
        if layer_cfg is None:
            continue

        feature_style_cfg = layer_cfg.feature_style

        if analysis_type == "safer-streets-model":
            label_fn = lambda s, e: f"${s:,.0f} - ${e:,.0f}"
        else:
            label_fn = None

        scheme = build_color_scheme(
            mode_gdf[score_col],
            max_bins=feature_style_cfg.max_bins,
            colorscale=feature_style_cfg.colorscale,
            label_formatter=label_fn,
        )
        if scheme is None:
            continue

        display_label = f"{CRASH_MODES[mode]} {score_col_suffix.replace('_', ' ').title()}"
        if score_col_suffix == "cost":
            results_gdf[display_label] = results_gdf[score_col].apply(
                lambda v: f"${v:,.2f}" if pd.notnull(v) and v > 0 else ""
            )
        else:
            results_gdf[display_label] = results_gdf[score_col].apply(
                lambda v: int(v) if pd.notnull(v) and v > 0 else ""
            )
        prop_cols.append(display_label)

        prefix = "Estimated " if analysis_type == "safer-streets-model" else ""
        suffix = "Crash Cost" if score_col_suffix == "cost" else "Score"
        mode_style_config[mode] = {
            "breaks": scheme.breaks,
            "colors": scheme.colors,
            "base_style": feature_style_cfg.to_dict(),
            "score_prop": score_col,
            "show_prop": f"show_{mode}",
            "display_label": display_label,
            "legend_title": f"{prefix}{CRASH_MODES[mode]} {suffix}",
        }
        legend_data[mode] = scheme.legend

    score_cols = [f"{m}_{score_col_suffix}" for m in modes if f"{m}_{score_col_suffix}" in results_gdf.columns]
    show_cols = [f"show_{m}" for m in modes if f"show_{m}" in results_gdf.columns]
    export_gdf = results_gdf[prop_cols + score_cols + show_cols + ["geom"]].set_geometry("geom")
    export_gdf = export_gdf.sort_values(by=score_cols, key=lambda s: s.fillna(0), ascending=True)
    geojson_data = export_gdf.__geo_interface__

    return study_layer, geojson_data, mode_style_config, legend_data

def placeholder_analysis_results_map(analysis_type: str):
    """Generate an empty Leaflet map displaying analysis results for the specified analysis type.

    Args:
        analysis_type (str): The type of analysis ("sliding-windows" or "safer-streets-model").

    Returns:
        dl.Map: A Leaflet map object with the analysis results.
    """
    if analysis_type not in ["sliding-windows", "safer-streets-model"]:
        raise ValueError("Invalid analysis type. Must be 'sliding-windows' or 'safer-streets-model'.")
    id_ = f"{analysis_type}-results-map"
    analysis_type_readable = "Sliding Windows Analysis" if analysis_type == "sliding-windows" else "Safer Streets Model"
    header = f"{analysis_type_readable} Results Map"

    basemap_config = SWA_MAP_CONFIG
    defaults_present = any(b.default for b in basemap_config.basemaps)
    base_layers = []
    for idx, bm in enumerate(basemap_config.basemaps):
        base_layers.append(
            dl.BaseLayer(
                dl.TileLayer(url=bm.tile_url, attribution=bm.attribution),
                name=bm.name,
                checked=bm.default or (not defaults_present and idx == 0),
            )
        )

    map_children = [
        dl.Pane(name="pop_pane", style={"zIndex": 1000}),
        dl.LayersControl(
            base_layers
            + [
                dl.Overlay(
                    dl.LayerGroup(id=f"study-area-{analysis_type}-layer"),
                    name="Study Area Boundary",
                    checked=True,
                )
            ]
        ),
    ]

    map_children.append(dl.LayerGroup(id=f"{analysis_type}-results-layer", children=[]))
    
    # Add legend overlay container
    legend_div = html.Div(
        id=f"{analysis_type}-legend",
        style={
            "display": "none",
            "position": "absolute",
            "bottom": "12px",
            "right": "12px",
            "zIndex": 1000,
            "pointerEvents": "auto",
            "backgroundColor": "white",
            "padding": "12px",
            "borderRadius": "4px",
            "boxShadow": "0 0 15px rgba(0, 0, 0, 0.2)",
            "maxHeight": "300px",
            "overflowY": "auto",
        },
        children=[]
    )
    map_children.append(legend_div)
    
    # Add mode selector overlay
    mode_selector_div = html.Div(
        [
            html.Label(html.B("Select Mode:"), style={"marginRight": "12px"}),
            dcc.Dropdown(
                id=f"{analysis_type}-mode-selector",
                options=[{"label": ml, "value": m} for m, ml in MODE_LABELS.items()],
                style={"width": "200px"},
                clearable=False
            )
        ], 
        id=f"{analysis_type}-mode-selector-div",
        style={"display": "none"}
    )
    
    map_card = dbc.Card(
        [
            dbc.CardHeader(html.Strong(header)),
            dbc.CardBody(
                [
                    mode_selector_div,
                    dl.Map(
                        id=id_,
                        center=(39.5, -98.35),  # CONUS
                        zoom=4,
                        children=map_children,
                        style={"height": "60vh", "width": "100%", "borderRadius": "12px"},
                        preferCanvas=True,
                    )
                ]
            ),
        ],
        className="mb-3 shadow-sm",
    )

    return map_card
