# The script takes crashes, roads, and study area as an input
# and generates an output road network with predicted crash values
# estimated using a bayesian model for each mode and severity

library(DBI)


# Connect to postgres database
drv <- RPostgreSQL::PostgreSQL()
con <- dbConnect(
  drv,
      dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
      host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
      user = Sys.getenv("SSPF_AMAZON_USERNAME"),
      password = Sys.getenv("SSPF_AMAZON_PASSWORD")
)

# user_id <- '1614643032'
# run_id <- 'gis'

make_expression <- function(col, txt, joiner){
  # col = iterable with items to insert into text
  # text = string into which to insert item sin col. Insert location is specified by '%s'
  # joiner = character/s to join the text with items inserted from col
  expression <- ''
  for (x in col) {
    expression <- paste(expression, gsub('%s', x, txt), sep=joiner)
  }
  expression <- sub(joiner, '', expression, fixed = TRUE)
  return(expression)
}

#------hin_0_update_inputs------
hin_0_update_inputs <- function(
  user_id = NULL,
  run_id = NULL,
  con = NULL,
  crash_geom = 'geom',
  crash_mode = 'usdot_mode_mapped',
  crash_sev = 'severity_mapped',
  crash_year = 'crash_year',
  min_yr = 2016,
  max_yr = 2019,
  
  crash_mode_codes = c('Bicycle Crash', 'Pedestrian Crash'),
  modes = c('bike', 'ped'),
  crash_sev_codes = c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)",  "Possible Injury (C)", "Property Damage Only (O)"),
  sevs = c('k', 'a', 'b', 'c', 'o'),
  wgt = c(3, 3, 1, 1, 0),
  output_sevs = c('k', 'a'),
  road_geom = 'geom',
  road_name = 'fullname',
  road_fclass = 'usdot_fun_class_mapped',
  fclass_values = c("Expressway", "Major Arterial", "Minor Arterial", "Major Collector",  "Minor Collector", "Local Road"),
  exclude_highways = TRUE,
  step_len = 0.1 * 1609, #one-tenth of a mile
  window = 0.5 * 1609, #half of a mile
  buff_dist = 15, #15m ~ 50 ft
  
  national_tracts_table_name = "automated.national_tracts",
  national_fclass_priors_table = "automated.national_fclass_priors",
  fars_mode_codes = c('bicyclist', 'pedestrian'), # must be in the order of the 'modes' vector specified above
  # fclass_deflate_factor = 1000,  #this will be replaced by crashes/mile or something similar
  low_severity_deflate_factor = 5, #this is the value that pbm values are divided by for severities 'b', 'c', 'o'
  stan_model_file = "//Users//Bikingman//Documents//GitHub//a0137_vulnerable_user_risk_network_tool//99x_main_tool_dev//safer_streets_priority_finder//stanfile.stan", #needs updating based on where the stan files lives on the server
  automated_schema = "automated"
     
) {
  user_run_id <- paste(user_id, run_id, sep="_")
 
  e = list()
  e$user_run_id <- user_run_id
  
  e$crash_table <- as.character(glue::glue('app_testing.crashes_{user_run_id}'))
  e$crash_id <- as.character(glue::glue('tdg_id_{user_run_id}'))
  e$crash_geom <- crash_geom
  e$crash_mode <- crash_mode
  e$crash_sev <- crash_sev
  e$crash_year <- crash_year
  e$min_yr <- min_yr
  e$max_yr <- max_yr
  names(crash_mode_codes) <- modes
  e$crash_mode_codes <- crash_mode_codes
  e$modes <- modes
  e$sevs <- sevs
  names(crash_sev_codes) <- e$sevs
  e$crash_sev_codes <- crash_sev_codes
  e$mode_scores <- paste0(e$modes, "_score")
  
  names(wgt) <- sevs
  e$wgt <- wgt
  e$output_sevs <- output_sevs
  
  e$roads_table <- as.character(glue::glue('app_testing.roads_{user_run_id}'))
  e$road_id <- as.character(glue::glue('tdg_id_{user_run_id}'))
  e$road_geom <- road_geom
  e$road_name <- road_name
  e$road_fclass <- road_fclass
  e$fclass_values <- fclass_values
  if (exclude_highways) {
    e$road_clause <- "road_fclass NOT IN ('Expressway', 'Omit From Analysis') AND road_fclass IS NOT NULL"
  } else {
    e$road_clause <- "road_fclass NOT IN ('Omit From Analysis') AND road_fclass IS NOT NULL"
  }
  
  e$study_area <- as.character(glue::glue('app_testing.study_area_{user_run_id}'))
  
  e$step_len <- step_len
  e$window <- window
  e$buff_dist <- buff_dist
  
  e$scratch_study_area_table <- as.character(glue::glue("scratch.hin_study_area_{user_run_id}"))
  e$scratch_crashes_table <- as.character(glue::glue("scratch.hin_crashes_{user_run_id}"))
  e$scratch_roads_table <- as.character(glue::glue("scratch.hin_roads_{user_run_id}"))
  e$scratch_excluded_crashes_table <- as.character(glue::glue("scratch.hin_crashes_excluded_{user_run_id}"))
  e$scratch_roads_dissolve_table <- as.character(glue::glue("scratch.hin_roads_dissolve_{user_run_id}"))
  e$scratch_sliding_windows_table <- as.character(glue::glue("scratch.hin_sliding_windows_{user_run_id}"))
  e$scratch_short_windows_table <- as.character(glue::glue("scratch.hin_short_windows_{user_run_id}"))
  e$crosswalk_table_name <- as.character(glue::glue("scratch.hin_xwalk_table_{user_run_id}"))
  e$weighting_table_name <- as.character(glue::glue("scratch.hin_weight_table_{user_run_id}"))
  e$window_crash_raw <- as.character(glue::glue("scratch.window_crash_raw_{user_run_id}"))
  
  e$scratch_tracts_table <- as.character((glue::glue("scratch.hin_tracts_{user_run_id}")))
  e$scratch_modeling_table <- as.character(glue::glue("scratch.hin_modeling_{user_run_id}"))
  e$scratch_modeling_agg_table <- as.character(glue::glue("scratch.hin_modeling_agg_{user_run_id}"))
  e$scratch_modeling_reduced_table <- as.character(glue::glue("scratch.hin_modeling_reduced_{user_run_id}"))
  
  e$out_road_table_name <- as.character(glue::glue("automated.hin_output_roads_{user_run_id}"))
  e$crosstab_table_name <- as.character(glue::glue("automated.hin_crosstab_table_{user_run_id}"))
  
  e$con <- con
  e$crs <- dbGetQuery(con, glue::glue("SELECT DISTINCT ST_SRID(geom) FROM {e$study_area};"))[1,1]
  
  e$scratch_national_fclass_priors <- as.character((glue::glue("scratch.hin_national_fclass_priors_{user_run_id}")))
  e$national_tracts_table_name <- national_tracts_table_name
  e$national_fclass_priors_table <- national_fclass_priors_table
  names(fars_mode_codes) <- modes
  e$fars_mode_codes <- fars_mode_codes
  e$alpha_g_year_equivalents <- max_yr - min_yr + 1
  e$beta_g_years_of_crash_data <- max_yr - min_yr + 1
  # e$fclass_deflate_factor <- fclass_deflate_factor
  e$low_severity_deflate_factor <- low_severity_deflate_factor
  e$stan_model_file <- stan_model_file
  e$stan_outputs_table <- as.character(glue::glue("hin_stan_outputs_{user_run_id}"))
  e$automated_schema <- automated_schema
  
  return(e)
}

#------hin_1_create_initial_scratch_tables------
hin_1_create_initial_scratch_tables = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    
    #----Create scratch study area----
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_study_area_table};
                               CREATE TABLE {scratch_study_area_table} AS (
                               SELECT geom
                               FROM {study_area}
                               );
                               "))
    dbGetQuery(con, glue::glue("
                               CREATE INDEX IF NOT EXISTS sidx_scratch_study_area ON {scratch_study_area_table} USING GIST(geom);
                               ANALYSE {scratch_study_area_table}(geom);
                               "))
    print("Scratch study area table made")
    
    #----Create scratch crashes----
    sev_reclass <- ""
    for (x in names(crash_sev_codes)){sev_reclass <- paste0(sev_reclass, glue::glue(paste0("WHEN {crash_sev} = '{crash_sev_codes[x]}' THEN '{x}' " )))}
    sev_reclass <- paste0(sev_reclass, "ELSE 'exclude'")
    
    mode_reclass <- ""
    for (x in names(crash_mode_codes)){mode_reclass <- paste0(mode_reclass, glue::glue(paste0("WHEN {crash_mode} = '{crash_mode_codes[x]}' THEN '{x}' " )))}
    mode_reclass <- paste0(mode_reclass, "ELSE 'exclude'")
    
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_crashes_table};
                               CREATE TABLE {scratch_crashes_table} AS (
                               SELECT {crash_id},
                               {crash_geom} as geom,
                               CAST({crash_year} AS int) as crash_year,
                               CASE {mode_reclass} END AS mode,
                               CASE {sev_reclass} END AS severity
                               FROM {crash_table}
                               );
                               
                               ALTER TABLE {scratch_crashes_table}
                               ADD mode_severity TEXT,
                               ADD id_flag BOOLEAN,
                               ADD geom_flag BOOLEAN,
                               ADD year_flag BOOLEAN,
                               ADD mode_flag BOOLEAN,
                               ADD sev_flag BOOLEAN,
                               ADD study_area_flag BOOLEAN
                               ;
                               
                               UPDATE
                               {scratch_crashes_table} a
                               SET
                               mode_severity = a.mode||'_'||a.severity,
                               id_flag = CASE
                               WHEN a.{crash_id} IS NULL
                               THEN TRUE
                               ELSE FALSE
                               END,
                               geom_flag = CASE
                               WHEN a.geom IS NULL
                               THEN TRUE
                               ELSE FALSE
                               END,
                               year_flag = CASE
                               WHEN a.crash_year < {min_yr} OR a.crash_year > {max_yr}
                               THEN TRUE
                               ELSE FALSE
                               END,
                               mode_flag = CASE
                               WHEN a.mode = 'exclude'
                               THEN TRUE
                               ELSE FALSE
                               END,
                               sev_flag = CASE
                               WHEN a.severity = 'exclude'
                               THEN TRUE
                               ELSE FALSE
                               END,
                               study_area_flag = CASE
                               WHEN ST_INTERSECTS(a.geom, b.geom)
                               THEN FALSE
                               ELSE TRUE
                               END
                               FROM
                               {scratch_study_area_table} b
                               ;
                               "))
    dbGetQuery(con, glue::glue("
                               CREATE INDEX IF NOT EXISTS sidx_scratch_crashes ON {scratch_crashes_table} USING GIST(geom);
                               ANALYSE {scratch_crashes_table}(geom);
                               "))
    print("Initial crashes table made. Cleaning crashes...")
    
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_excluded_crashes_table};
                               CREATE TABLE {scratch_excluded_crashes_table} AS (
                               SELECT
                               *
                               FROM
                               {scratch_crashes_table}
                               WHERE
                               id_flag OR geom_flag OR year_flag OR mode_flag OR sev_flag OR study_area_flag
                               );
                               
                               DELETE FROM {scratch_crashes_table}
                               WHERE
                               id_flag OR geom_flag OR year_flag OR mode_flag OR sev_flag OR study_area_flag;
                               "))
    print("Cleaned crashes. Excluded crashes added to the scratch schema.")
    
    #----Create scratch roads----
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_roads_table};
                               CREATE TABLE {scratch_roads_table} AS (
                               SELECT {road_id}, {road_name} as road_name, {road_fclass} as road_fclass, {road_geom} as geom
                               FROM {roads_table}
                               );
                               
                               CREATE INDEX IF NOT EXISTS sidx_scratch_roads ON {scratch_roads_table} USING GIST(geom);
                               ANALYSE {scratch_roads_table}(geom);
                               "))
    print("Scratch roads table made")
    
    #----Create scratch national fclass priors----
    mode_reclass <- ""
    for (x in names(fars_mode_codes)){mode_reclass <- paste0(mode_reclass, glue::glue(paste0("WHEN mode = '{fars_mode_codes[x]}' THEN '{x}' " )))
    # where_clause <- paste0(where_clause, fars_mode_codes[x], ',')
    }
    where_clause <- make_expression(fars_mode_codes, "'%s'", ",")
    where_clause <- as.character(glue::glue("b.mode in ({where_clause})"))
    
    
    # mode_reclass <- paste0(mode_reclass, "ELSE 'exclude'")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_national_fclass_priors};
                               CREATE TABLE {scratch_national_fclass_priors} AS (
                               SELECT CASE {mode_reclass} END AS mode,
                               severity,
                               road_fclass,
                               mileage,
                               n_crashes,
                               beta_crashes
                               FROM {national_fclass_priors_table} b
                               WHERE {where_clause}
                               );
                               "))
    print("Scratch national fclass priors table made")
    
    mem_use <- 0
    for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    print(glue::glue("Memory used in function = {mem_use} bytes"))
  }, error = function(cond){
    print(cond)
  })
}

#------hin_2_join_crashes_to_roads------
hin_2_join_crashes_to_roads = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    
    #----Make output roads----
    print("Making output roads")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {out_road_table_name};
                               CREATE TABLE {out_road_table_name} AS (
                               SELECT
                               a.*
                               FROM
                               {scratch_roads_table} a,
                               {scratch_study_area_table} b
                               WHERE
                               {road_clause}
                               AND
                               ST_INTERSECTS(a.geom, b.geom)
                               );
                               
                               ALTER TABLE {out_road_table_name} ADD length FLOAT;
                               UPDATE {out_road_table_name} SET length = ST_Length(geom);
                               
                               CREATE INDEX IF NOT EXISTS sidx_geom_out_road_table_name ON {out_road_table_name} USING GIST(geom);
                               ANALYSE {out_road_table_name}(geom);
                               
                               ALTER TABLE {out_road_table_name}
                               ADD PRIMARY KEY ({road_id})
                               ;
                               "))
    print("Output roads table made")
    
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {crosswalk_table_name};
                               CREATE TABLE {crosswalk_table_name} AS (
                               SELECT
                               crash_id,
                               road_id
                               FROM (
                               SELECT
                               a.{crash_id} AS crash_id,
                               b.{road_id} AS road_id,
                               RANK() OVER (PARTITION BY a.{crash_id} ORDER BY ST_DISTANCE(a.geom, b.geom))
                               FROM
                               {scratch_crashes_table} a,
                               {out_road_table_name} b
                               WHERE
                               ST_DWITHIN(a.geom, b.geom, {buff_dist})
                               ) x
                               WHERE
                               x.rank = 1
                               );
                               
                               CREATE INDEX idx_crosswalk_table_crash_id ON {crosswalk_table_name}(crash_id);
                               ANALYSE {crosswalk_table_name}(crash_id);
                               
                               CREATE INDEX idx_crosswalk_table_name_road_id ON {crosswalk_table_name}(road_id);
                               ANALYSE {crosswalk_table_name}(road_id);
                               "))
    
    
    count_expression <- "COUNT(*) AS tot_all"
    sum_expression <- "SUM(a.tot_all) AS tot_all"
    update_expression <- "tot_all = x.tot_all"
    sum_cols <- c("tot_all")
    for (x in modes) {
      count_expression <- paste(count_expression, glue::glue("COUNT(*) FILTER(WHERE mode = '{x}') AS tot_{x}_all"), sep=",")
      sum_expression <- paste(sum_expression, glue::glue("SUM(a.tot_{x}_all) AS tot_{x}_all"), sep=",")
      update_expression <- paste(update_expression, glue::glue("tot_{x}_all = x.tot_{x}_all"), sep=",")
      sum_cols <- c(sum_cols, c(glue::glue("tot_{x}_all")))
      for (y in output_sevs) {
        count_expression <- paste(count_expression, glue::glue("COUNT(*) FILTER(WHERE mode = '{x}' AND severity = '{y}') AS tot_{x}_{y}"), sep=",")
        sum_expression <- paste(sum_expression, glue::glue("SUM(a.tot_{x}_{y}) AS tot_{x}_{y}"), sep=",")
        update_expression <- paste(update_expression, glue::glue("tot_{x}_{y} = x.tot_{x}_{y}"), sep=",")
        sum_cols <- c(sum_cols, c(glue::glue("tot_{x}_{y}")))
      }
    }
    add_cols <- make_expression(sum_cols, 'ADD IF NOT EXISTS %s INT DEFAULT 0', ',')
    
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {out_road_table_name}
                               {add_cols};
                               "))
    print("Columns added to output road table")
    
    dbGetQuery(con, glue::glue("
                               UPDATE {out_road_table_name} a
                               SET
                               {update_expression}
                               FROM (
                               SELECT
                               b.road_id,
                               {sum_expression}
                               FROM (
                               SELECT
                               {crash_id},
                               {count_expression}
                               FROM
                               {scratch_crashes_table}
                               GROUP BY
                               {crash_id}
                               )a
                               INNER JOIN
                               {crosswalk_table_name} b
                               ON
                               a.{crash_id} = b.crash_id
                               GROUP BY
                               b.road_id
                               ) x
                               WHERE
                               a.{road_id} = x.road_id;
                               "))
    print("Crash totals added to the output roads table")
    
    #----Sum by roadclass and create crosstab----
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {crosstab_table_name};
                               CREATE TABLE {crosstab_table_name} AS (
                               SELECT
                               COUNT(a.*),
                               a.mode_severity,
                               c.road_fclass
                               FROM
                               {scratch_crashes_table} a
                               INNER JOIN
                               {crosswalk_table_name} b
                               ON
                               a.{crash_id} = b.crash_id
                               INNER JOIN
                               {out_road_table_name} c
                               ON
                               b.road_id = c.{road_id}
                               GROUP BY
                               a.mode_severity,
                               c.road_fclass
                               ORDER BY
                               a.mode_severity,
                               c.road_fclass
                               );
                               
                               ALTER TABLE {crosstab_table_name}
                               ADD IF NOT EXISTS length FLOAT DEFAULT 0;
                               
                               UPDATE {crosstab_table_name} a
                               SET
                               length = b.length
                               FROM (
                               SELECT
                               c.road_fclass,
                               SUM(ST_Length(c.geom)) as length
                               FROM
                               {out_road_table_name} c
                               GROUP BY
                               c.road_fclass
                               ) b
                               WHERE
                               a.road_fclass = b.road_fclass;
                               "))
    
    
    print("Crosstable of road class and mode_crash_sev made")
    
    mem_use <- 0
    for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    print(glue::glue("Memory used in function = {mem_use} bytes"))
  }, error = function(cond){
    print(cond)
  })
}

#------hin_3_create_sliding_windows------
hin_3_create_sliding_windows = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    #----Dissolve roads by name, functional class, and proximity----
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_roads_dissolve_table};
                               CREATE TABLE {scratch_roads_dissolve_table} AS (
                               SELECT
                               LOWER(a.road_name) AS road_name,
                               road_fclass,
                               (ST_DUMP(ST_LINEMERGE(ST_UnaryUnion(unnest(ST_ClusterWithin(a.geom, 5)))))).geom as geom
                               FROM
                               {scratch_roads_table} a,
                               {scratch_study_area_table} b
                               WHERE
                               {road_clause}
                               AND
                               ST_INTERSECTS(a.geom, b.geom)
                               GROUP BY
                               road_name, road_fclass
                               )
                               ;"))
    
    dbGetQuery(con, glue::glue("
                               CREATE INDEX IF NOT EXISTS idx_road_name_dissolve ON {scratch_roads_dissolve_table}(road_name);
                               ANALYSE {scratch_roads_dissolve_table}(road_name);
                               
                               CREATE INDEX IF NOT EXISTS idx_road_fclass_dissolve ON {scratch_roads_dissolve_table}(road_fclass);
                               ANALYSE {scratch_roads_dissolve_table}(road_fclass);
                               
                               CREATE INDEX IF NOT EXISTS sidx_dissolve ON {scratch_roads_dissolve_table} USING GIST(geom);
                               ANALYSE {scratch_roads_dissolve_table}(geom);
                               
                               ALTER TABLE {scratch_roads_dissolve_table} ADD length FLOAT;
                               
                               UPDATE {scratch_roads_dissolve_table} SET length = ST_Length(geom);
                               
                               DELETE FROM {scratch_roads_dissolve_table}
                               WHERE length = 0;
                               
                               CREATE INDEX IF NOT EXISTS idx_length_dissolve ON {scratch_roads_dissolve_table}(length);
                               ANALYSE {scratch_roads_dissolve_table}(length);
                               "))
    print("Roads dissolved by name, functional class, and proximity.")
    
    
    
    #------Make sliding windows------
    
    # Get max number of iterations on the longest window segment
    max_iter <- dbGetQuery(con, glue::glue("SELECT CEILING(MAX(length / {step_len}))::INT FROM {scratch_roads_dissolve_table};"))[1,1]
    
    #----Make long windows----
    print("Making sliding windows")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_sliding_windows_table};
                               CREATE TABLE {scratch_sliding_windows_table}(
                               id SERIAL PRIMARY KEY,
                               geom GEOMETRY(LINESTRING, {crs}),
                               road_name TEXT,
                               road_fclass TEXT,
                               length FLOAT
                               );
                               
                               INSERT INTO {scratch_sliding_windows_table}(
                               road_name,
                               road_fclass,
                               geom
                               )
                               SELECT
                               road_name,
                               road_fclass,
                               ST_LineMerge(
                               ST_LineSubstring(
                               geom,
                               {step_len} * n / length,
                               CASE
                               WHEN ({window} + ({step_len} * n)) < length
                               THEN ({window} + ({step_len} * n)) / length
                               ELSE 1
                               END
                               )
                               ) AS geom
                               FROM
                               {scratch_roads_dissolve_table}
                               CROSS JOIN
                               generate_series(0, {max_iter}) AS n
                               WHERE
                               n < 1 + CEILING((length - {window})/{step_len})
                               UNION
                               SELECT
                               road_name,
                               road_fclass,
                               ST_LineMerge(geom)
                               FROM
                               {scratch_roads_dissolve_table}
                               WHERE
                               length < {window};
                               
                               CREATE INDEX IF NOT EXISTS sidx_window_geom ON {scratch_sliding_windows_table} USING GIST(geom);
                               ANALYSE {scratch_sliding_windows_table}(geom);
                               
                               UPDATE
                               {scratch_sliding_windows_table}
                               SET
                               length = ST_Length(geom);
                               "))
    
    #----Make Short Windows----
    print("Making short windows")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_short_windows_table};
                               CREATE TABLE {scratch_short_windows_table} (
                               short_window_id SERIAL PRIMARY KEY,
                               tdg_road_id INT,
                               geom GEOMETRY(LINESTRING, {crs}),
                               road_name TEXT,
                               road_fclass TEXT
                               );
                               
                               INSERT INTO {scratch_short_windows_table}(
                               tdg_road_id,
                               geom,
                               road_name,
                               road_fclass
                               )
                               SELECT
                               {road_id} as tdg_road_id,
                               ST_LineMerge(
                               ST_LineSubstring(
                               geom,
                               {step_len} * n / length,
                               CASE
                               WHEN {step_len} * (n + 1) < length
                               THEN {step_len} * (n + 1) / length
                               ELSE 1
                               END
                               )
                               ) AS geom,
                               road_name,
                               road_fclass
                               FROM
                               {out_road_table_name}
                               CROSS JOIN
                               generate_series(0, {max_iter}) AS n
                               WHERE
                               ({step_len} * n) / length < 1
                               UNION
                               SELECT
                               {road_id} as tdg_road_id,
                               ST_LineMerge(geom),
                               road_name,
                               road_fclass
                               FROM
                               {out_road_table_name}
                               WHERE
                               length < {step_len}
                               ;
                               
                               CREATE INDEX IF NOT EXISTS sidx_short_window_geom ON {scratch_short_windows_table} USING GIST(geom);
                               ANALYSE {scratch_short_windows_table}(geom);
                               "))
    
    
    
    #----Joining crashes to windows----
    print("Adding crash columns to window tables")
    mode_sev_cols <- paste(rep(modes, each = length(sevs)), sevs, sep = "_")
    add_cols <- make_expression(mode_sev_cols, 'ADD IF NOT EXISTS %s FLOAT DEFAULT 0', ',')
    
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_sliding_windows_table}
                               {add_cols};
                               
                               ALTER TABLE {scratch_short_windows_table}
                               {add_cols};
                               "))
    
    print("Performing window density analysis")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {window_crash_raw};
                               CREATE TABLE {window_crash_raw} AS (
                               SELECT
                               a.id,
                               COUNT(b.*),
                               LOWER(b.mode_severity) AS mode_severity
                               FROM
                               {scratch_sliding_windows_table} a,
                               {scratch_crashes_table} b
                               WHERE
                               ST_DWITHIN (a.geom, b.geom, {buff_dist})
                               GROUP BY
                               a.id,
                               b.mode_severity
                               ORDER BY
                               a.id,
                               b.mode_severity
                               );
                               "))
    print("Crashes joined to windows")
    
    crashes_present <- dbGetQuery(con, glue::glue("
                                                  SELECT DISTINCT mode_severity
                                                  FROM {window_crash_raw}
                                                  WHERE mode_severity IS NOT NULL;
                                                  "))[,1]
    for (c in crashes_present) {
      dbGetQuery(con, glue::glue("
                                 UPDATE
                                 {scratch_sliding_windows_table} a
                                 SET
                                 {c} = b.count
                                 FROM
                                 {window_crash_raw} b
                                 WHERE
                                 a.id = b.id
                                 AND
                                 b.mode_severity = '{c}';
                                 "))
    }
    print("Finished adding counts to windows")
    
    #----Calculating mode scores----
    add_cols <- make_expression(mode_scores, 'ADD IF NOT EXISTS %s FLOAT DEFAULT 0', ',')
    
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_sliding_windows_table}
                               {add_cols};
                               
                               ALTER TABLE {scratch_short_windows_table}
                               {add_cols};
                               "))
    
    for (m in modes) {
      score_expression <- make_expression(paste0(m, "_", names(wgt), "*", wgt), "%s", "+")
      dbGetQuery(con, glue::glue("UPDATE {scratch_sliding_windows_table} SET {m}_score = {score_expression};"))
    }
    print("All scores added to sliding windows")
    
    mem_use <- 0
    for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    print(glue::glue("Memory used in function = {mem_use} bytes"))
    }, error = function(cond){
      print(cond)
    })
  }

#------hin_4_calculate_priors------
hin_4_calculate_priors = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    #------Create tracts table and join crashes-----
    print("Creating tracts table with necessary fields")
    n_mode_sev_cols <- paste('n', rep(modes, each = length(sevs)), sevs, sep = "_")
    mode_sev_cols <- paste(rep(modes, each = length(sevs)), sevs, sep = "_")
    add_cols <- make_expression(n_mode_sev_cols, '0::INTEGER AS %s', ',')
    
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_tracts_table};
                               CREATE TABLE {scratch_tracts_table} AS 
                               SELECT a.ogc_fid,
                               a.gisjoin,
                               a.geoid,
                               a.pbm_value,
                               ST_Transform(a.geom, {crs}) AS geom,
                               {add_cols}
                               FROM {national_tracts_table_name} a, 
                               {scratch_study_area_table} b
                               WHERE ST_INTERSECTS(a.geom, ST_Transform(b.geom, 4326))
                               ;

                               CREATE INDEX IF NOT EXISTS sidx_scratch_tracts_area ON {scratch_tracts_table} USING GIST(geom);
                               ANALYSE {scratch_tracts_table}(geom);
                               "))
    
    
    print("Joining crashes to the tracts")
    sum_crashes_expression <- make_expression(mode_sev_cols, glue::glue("SUM(CASE WHEN mode_severity = '%s' THEN 1 ELSE 0 END) AS n_%s"), ',')
    update_sum_crashes_expression <- make_expression(n_mode_sev_cols, "%s = y.%s", ',')
    
    dbGetQuery(con, glue::glue("
                               WITH tract_crash_xwalk AS (
                               SELECT a.geoid, 
                               b.mode_severity 
                               FROM {scratch_tracts_table} a, 
                               {scratch_crashes_table} b 
                               WHERE ST_INTERSECTS(a.geom, b.geom)
                               ), tract_crash_agg AS (
                               SELECT geoid, 
                               {sum_crashes_expression}
                               FROM tract_crash_xwalk
                               GROUP BY geoid 
                               ORDER BY geoid )
                               UPDATE {scratch_tracts_table} x SET 
                               {update_sum_crashes_expression}
                               FROM tract_crash_agg y 
                               WHERE x.geoid = y.geoid 
                               ;
                               "))
    
    #------Create modeling table------
    print("Creating modeling table")
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_modeling_table};
                               CREATE TABLE {scratch_modeling_table} AS
                               SELECT
                               a.id as window_id,
                               a.road_name,
                               b.geoid,
                               b.gisjoin,
                               NULL::TEXT AS road_fclass,
                               NULL::TEXT AS severity,
                               NULL::TEXT AS mode,
                               NULL::TEXT AS mode_severity,
                               0::INTEGER AS window_crashes,
                               0::INTEGER AS tract_crashes,
                               b.pbm_value AS pbm_mean,
                               1.0 AS pbm_stdev,
                               a.length AS window_length
                               FROM {scratch_sliding_windows_table} a, 
                               {scratch_tracts_table} b 
                               WHERE 1=2;
                               "))
    
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_modeling_table} 
                               ADD COLUMN id SERIAL PRIMARY KEY;
                               "))
    
    for (f in fclass_values) {
      for (m in modes) {
        # m_code <- crash_mode_codes[m]
        for (s in sevs) {
          # s_code <- crash_sev_codes[s]
          dbGetQuery(con, glue::glue("
                                     INSERT INTO {scratch_modeling_table}
                                     SELECT
                                     a.id AS window_id,
                                     a.road_name,
                                     b.geoid,
                                     b.gisjoin,
                                     '{f}' AS road_fclass,
                                     '{s}' AS severity,
                                     '{m}' AS mode,
                                     '{m}_{s}' AS mode_severity,
                                     a.{m}_{s} AS window_crashes,
                                     b.n_{m}_{s} AS tract_crashes,
                                     b.pbm_value AS pbm_mean,
                                     1.0 AS pbm_stdev,
                                     a.length AS window_length  
                                     FROM {scratch_sliding_windows_table} a, 
                                     {scratch_tracts_table} b 
                                     WHERE ST_DWITHIN(a.geom, b.geom, 10)
                                     AND a.road_fclass = '{f}'
                                     ;
                                     "))
        }
        }
        }
    
    
    #------Calculate priors------
    print("Calculating priors")
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_modeling_table}
                               ADD COLUMN IF NOT EXISTS fclass_alpha_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS fclass_beta_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS alpha_b_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS beta_b_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS alpha_b NUMERIC(10, 4),
                               ADD COLUMN IF NOT EXISTS beta_b NUMERIC(10, 4),
                               ADD COLUMN IF NOT EXISTS alpha_g_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS beta_g_0 DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS alpha_g NUMERIC(10, 2),
                               ADD COLUMN IF NOT EXISTS beta_g NUMERIC(10, 1),
                               ADD COLUMN IF NOT EXISTS fclass_mileage INT;
                               "))
    
    #Update alpha_b and beta_b in modeling table
    dbGetQuery(con, glue::glue("
                               UPDATE {scratch_modeling_table} a
                               SET 
                               fclass_alpha_0 = b.n_crashes,
                               fclass_beta_0 = b.beta_crashes,
                               fclass_mileage = b.mileage
                               FROM {scratch_national_fclass_priors} b 
                               WHERE a.road_fclass = b.road_fclass
                               AND a.mode = b.mode
                               ;
                               
                               UPDATE {scratch_modeling_table} SET 
                               alpha_b_0 = fclass_alpha_0 / fclass_mileage,
                               beta_b_0 = fclass_beta_0 / fclass_mileage
                               ;
                               

                               UPDATE {scratch_modeling_table} SET 
                               alpha_b = window_crashes + alpha_b_0,
                               beta_b = (tract_crashes - window_crashes) + beta_b_0
                               ;
                               "))
    
    #Update alpha_g and beta_g in modeling table
    dbGetQuery(con, glue::glue("
                               UPDATE {scratch_modeling_table} a SET 
                               alpha_g_0 = CASE 
                               WHEN a.severity IN ('k', 'a') 
                               THEN a.pbm_mean * {alpha_g_year_equivalents}
                               ELSE a.pbm_mean * {alpha_g_year_equivalents}/{low_severity_deflate_factor}
                               END,
                               beta_g_0 = {alpha_g_year_equivalents}
                               ;
                               
                               UPDATE {scratch_modeling_table} a SET 
                               alpha_g = a.alpha_g_0 + a.tract_crashes,
                               beta_g = a.beta_g_0 + {beta_g_years_of_crash_data}
                               ;
                               "))
    
    #Aggregate modeling table to have one row per window segment per mode_severity
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_modeling_agg_table};
                               SELECT window_id, road_name, road_fclass, 
                               severity, mode, mode_severity, 
                               COUNT(*) AS n_tracts,
                               MIN(window_crashes) AS window_crashes,
                               SUM(tract_crashes) AS tract_crashes,
                               SUM(pbm_mean) AS pbm_mean,
                               MIN(window_length) AS window_length,
                               MIN(alpha_b) AS alpha_b,
                               (SUM(tract_crashes) - MIN(window_crashes) + MIN(beta_b_0)) AS beta_b,
                               SUM(alpha_g) AS alpha_g,
                               MIN(beta_g) AS beta_g
                               INTO {scratch_modeling_agg_table}
                               FROM {scratch_modeling_table}
                               GROUP BY window_id, road_name, road_fclass, 
                               severity, mode, mode_severity 
                               ORDER BY window_id, road_name, road_fclass, 
                               severity, mode, mode_severity
                               ;
                               
                               ALTER TABLE {scratch_modeling_agg_table}
                               ADD COLUMN id SERIAL PRIMARY KEY
                               ;
                               "))
    
    #Reduce the dataset for unique parameter combination
    dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_modeling_reduced_table};
                               SELECT DISTINCT 
                               alpha_b,
                               beta_b,
                               alpha_g,
                               beta_g 
                               INTO {scratch_modeling_reduced_table}
                               FROM {scratch_modeling_agg_table}
                               ;
                               "))
    print("Priors for the model written to a table")
    
    mem_use <- 0
    for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    print(glue::glue("Memory used in function = {mem_use} bytes"))
        }, error = function(cond){
          print(cond)
        })
      }

#------hin_5_run_stan_model------
hin_5_run_stan_model = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    #------Run stan model------
    print("Running stan model. This could take some time")
    #Read the priors table into a dataframe
    pbm_test <- dbGetQuery(con, glue::glue("SELECT * FROM {scratch_modeling_reduced_table}"))
    pbm_outs <- data.frame()
    
    
    total_n <- nrow(pbm_test)
    chunk_size <- 100
    start_n <- 1
    end_n <- min(chunk_size, total_n)
    while (start_n <= total_n) {
      print(glue::glue("Start_N = {start_n}, End_N = {end_n}, Total_N = {total_n}"))
      pbm_clip <- pbm_test[start_n:end_n,]
      base_modeling_data <- list(
        N = nrow(pbm_clip),
        alpha_B = pbm_clip$alpha_b,
        beta_B = pbm_clip$beta_b,
        alpha_G = pbm_clip$alpha_g,
        beta_G = pbm_clip$beta_g
      )
      
      test_model <- rstan::stan(stan_model_file, data=base_modeling_data, pars = "crashes")
      rm(base_modeling_data) # clears up some memory
      posterior <- rstan::extract(test_model)
      
      # This is where the most memory is used. Logging memory usage
      # mem_use <- 0
      # for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
      # print(glue::glue("Memory used in function = {mem_use} bytes"))
      
      rm(test_model) # clears up some memory
      crashes <- as.data.frame(posterior$crashes)
      rm(posterior) # clears up some memory
      mean_crashes <- sapply(crashes, mean)
      sd_crashes <- sapply(crashes, sd)
      rm(crashes) # clears up some memory
      df_tmp <- cbind(pbm_clip, mean_crashes, sd_crashes)
      pbm_outs <- rbind(pbm_outs, df_tmp)
      rm(pbm_clip, mean_crashes, sd_crashes, df_tmp) # clears up some memory
      
      gc() # returns memory back to system
      start_n <- end_n + 1
      end_n <- min(end_n + chunk_size, total_n)
    }
    
    pbm_outs$cv_crashes <- pbm_outs$mean_crashes/pbm_outs$sd_crashes
    
    # #Run stan model and get estimates
    # base_modeling_data <- list(
    #   N = nrow(pbm_test),
    #   alpha_B = pbm_test$alpha_b,
    #   beta_B = pbm_test$beta_b,
    #   alpha_G = pbm_test$alpha_g,
    #   beta_G = pbm_test$beta_g
    # )
    # 
    # test_model <- rstan::stan(stan_model_file, data=base_modeling_data)
    
    # # Warning messages:
    # #   1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
    # #   '-E' not found
    # # 2: There were 255 divergent transitions after warmup. See
    # # http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
    # # to find out why this is a problem and how to eliminate them. 
    # # 3: Examine the pairs() plot to diagnose sampling problems
    # # 
    # # 4: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
    # # Running the chains for more iterations may help. See
    # # http://mc-stan.org/misc/warnings.html#tail-ess 
    # 
    # # print(get_elapsed_time(test_model))
    # # 1762 rows
    # #         warmup  sample
    # # chain:1 419.109 167.171
    # # chain:2 411.590 394.254
    # # chain:3 419.677 318.153
    # # chain:4 379.725 404.998

    # rm(base_modeling_data) # clears up some memory
    # posterior <- extract(test_model)
    # 
    # # This is where the most memory is used. Logging memory usage
    # mem_use <- 0
    # for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    # print(glue::glue("Memory used in function = {mem_use} bytes"))
    # 
    # rm(test_model) # clears up some memory
    # crashes <- as.data.frame(posterior$crashes)
    # rm(posterior) # clears up some memory
    # mean_crashes <- sapply(crashes, mean)
    # sd_crashes <- sapply(crashes, sd)
    # rm(crashes) # clears up some memory
    # pbm_outs <- cbind(pbm_test, mean_crashes, sd_crashes)
    # pbm_outs$cv_crashes <- pbm_outs$mean_crashes/pbm_outs$sd_crashes
    # 
    # rm(pbm_test, mean_crashes, sd_crashes) # may not be necessary if the variables are cleared when function exits
    
    dbWriteTable(con, c(automated_schema, stan_outputs_table), pbm_outs, overwrite = TRUE, row.names=FALSE)
    print("Stan outputs written to the database")
    
  }, error = function(cond){
    print(cond)
  })
}

#------hin_6_join_model_outputs_to_roads------
hin_6_join_model_outputs_to_roads = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())
    
    stan_outputs_table <- paste0(automated_schema,'.',stan_outputs_table)
    print("Joining stan outputs to the windows")
    #---- Join stan outputs to modeling_agg_table ----
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_modeling_agg_table} 
                               ADD COLUMN IF NOT EXISTS stan_crashes_mean DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS stan_crashes_sd DOUBLE PRECISION,
                               ADD COLUMN IF NOT EXISTS stan_crashes_cv DOUBLE PRECISION
                               ;
                               
                               UPDATE {scratch_modeling_agg_table} a SET 
                               stan_crashes_mean = b.mean_crashes,
                               stan_crashes_sd = b.sd_crashes,
                               stan_crashes_cv = b.cv_crashes 
                               FROM {stan_outputs_table} b 
                               WHERE ROUND(a.alpha_b::NUMERIC,10) = ROUND(b.alpha_b::NUMERIC,10) 
                               AND ROUND(a.beta_b::NUMERIC,10) = ROUND(b.beta_b::NUMERIC,10) 
                               AND ROUND(a.alpha_g::NUMERIC,10) = ROUND(b.alpha_g::NUMERIC,10) 
                               AND ROUND(a.beta_g::NUMERIC,10) = ROUND(b.beta_g::NUMERIC,10) 
                               ;
                               "))
    
    # Join stan outputs to sliding window table
    stan_mode_sev_cols <- paste('stan', rep(modes, each = length(sevs)), sevs, sep = "_")
    add_cols <- make_expression(stan_mode_sev_cols, "ADD COLUMN IF NOT EXISTS %s DOUBLE PRECISION", ',')
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_sliding_windows_table}
                               {add_cols};
                               
                               ALTER TABLE {scratch_short_windows_table}
                               {add_cols};
                               "))
    
    
    for (m in modes) {
      for (s in sevs) {
        dbGetQuery(con, glue::glue("
                                   UPDATE {scratch_sliding_windows_table} a SET 
                                   stan_{m}_{s} = b.stan_crashes_mean / (a.length / 1609.34)
                                   FROM {scratch_modeling_agg_table}  b 
                                   WHERE a.id = b.window_id
                                   AND b.mode_severity = '{m}_{s}'
                                   ;
                                   "))
      }
      }
    
    #----Assign values to short windows----
    print("Adding scores to short windows")
    mode_sev_cols <- paste(rep(modes, each = length(sevs)), sevs, sep = "_")
    update_expression <- make_expression(c(mode_scores, mode_sev_cols, stan_mode_sev_cols), '%s = c.%s', ',')
    max_expression = make_expression(c(mode_scores, mode_sev_cols, stan_mode_sev_cols), 'MAX(b.%s) AS %s', ',')
    dbGetQuery(con, glue::glue("
                               UPDATE {scratch_short_windows_table} a
                               SET {update_expression}
                               FROM (
                               SELECT
                               a.short_window_id,
                               {max_expression}
                               FROM
                               {scratch_short_windows_table} a,
                               {scratch_sliding_windows_table} b
                               WHERE
                               COALESCE(LOWER(a.road_name),'none') = COALESCE(b.road_name,'none')
                               AND
                               ST_DWITHIN(a.geom, b.geom, 2)
                               GROUP BY
                               a.short_window_id
                               ) c
                               WHERE a.short_window_id = c.short_window_id;
                               
                               ALTER TABLE {scratch_short_windows_table} ADD IF NOT EXISTS length FLOAT;        
                               UPDATE {scratch_short_windows_table} SET length = ST_Length(geom);
                               "))
    print("All scores and stan outputs added to short windows")
    
    add_cols <- make_expression(mode_scores, 'ADD IF NOT EXISTS %s FLOAT DEFAULT 0', ',')
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {out_road_table_name}
                               {add_cols};
                               "))
    
    stan_mode_sev_cols <- paste('stan', rep(modes, each = length(sevs)), sevs, sep = "_")
    add_cols <- make_expression(stan_mode_sev_cols, "ADD COLUMN IF NOT EXISTS %s DOUBLE PRECISION", ',')
    dbGetQuery(con, glue::glue("
                               ALTER TABLE {out_road_table_name}
                               {add_cols};
                               "))
    
    #---- Add mode scores and model estimates to the output roads ----
    update_expression <- make_expression(c(mode_scores, stan_mode_sev_cols), "%s = a.%s", ',')
    sum_expression <- make_expression(c(mode_scores, stan_mode_sev_cols), "SUM(%s*length)/SUM(length) as %s", ',')
    dbGetQuery(con, glue::glue("
                               UPDATE {out_road_table_name}
                               SET
                               {update_expression}
                               FROM (
                               SELECT {sum_expression},
                               tdg_road_id
                               FROM {scratch_short_windows_table}
                               GROUP BY tdg_road_id
                               ) a
                               WHERE {road_id} = a.tdg_road_id;
                               "))
    
    print("Scores and stan outputs added to the roads")
    
    mem_use <- 0
    for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    print(glue::glue("Memory used in function = {mem_use} bytes"))
      }, error = function(cond){
        print(cond)
      })
    }


#------hin_run_all------
hin_run_all = function(con=NULL, inputs=NULL) {
  start.time <- Sys.time()
  print(glue::glue("Start time = {start.time}"))
  
  hin_1_create_initial_scratch_tables(con, inputs)
  hin_2_join_crashes_to_roads(con, inputs)
  hin_3_create_sliding_windows(con, inputs)
  hin_4_calculate_priors(con, inputs)
  hin_5_run_stan_model(con, inputs)
  hin_6_join_model_outputs_to_roads(con, inputs)
  
  end.time <- Sys.time()
  print(glue::glue("Start time = {end.time}"))
  print(end.time-start.time)
  
  # print("Dropping scratch tables")
  # for (x in inputs) {
  #   if (is.character(x)) {
  #     if (startsWith(toString(x), "scratch.")) {
  #         DBI::dbGetQuery(con, glue::glue("
  #                               DROP TABLE IF EXISTS {x};
  #                              "))
  #       }}}
}

#------hin_run_sliding_sliding_windows------
hin_run_sliding_windows = function(con=NULL, inputs=NULL) {
  
  hin_1_create_initial_scratch_tables(con, inputs)
  hin_3_create_sliding_windows(con, inputs)
  
}

#----run functions----
run_inputs <- hin_0_update_inputs(con=con, user_id = "1614643032", run_id = "gis")

hin_run_all(con=con, inputs=run_inputs)
