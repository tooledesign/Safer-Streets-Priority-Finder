


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
# returns object of variables for subsequent fucntions  
hin_0_update_inputs <- function(
  user_id = NULL,
  run_id = NULL,
  con = NULL,
  crash_geom = 'geom',
  crash_mode = 'usdot_mode_mapped',
  crash_sev = 'severity_mapped',
  # crash_year = 'crash_year',
  # min_yr = 2016,
  # max_yr = 2019,
  crash_mode_codes = c('Bicycle Crash', 'Pedestrian Crash', 'Other Crash'),
  modes = c('bike', 'ped', 'other'),
  crash_sev_codes = c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)",  "Possible Injury (C)", "Property Damage Only (O)"),
  sevs = c('k', 'a', 'b', 'c', 'o'),
  wgt = c(3, 3, 1, 0, 0),
  output_sevs = c('k', 'a'),
  road_geom = 'geom',
  road_name = 'fullname',
  road_fclass = 'usdot_fun_class_mapped',
  fclass_values = c("Expressway", "Major Arterial", "Minor Arterial", "Major Collector",  "Minor Collector", "Local Road"),
  exclude_highways = FALSE,
  step_len = 0.1 * 1609, #one-tenth of a mile
  window = 0.5 * 1609, #half of a mile
  buff_dist = 15, #15m ~ 50 ft
  fars_mode_codes = c('bicyclist', 'pedestrian', 'other'), # must be in the order of the 'modes' vector specified above
  automated_schema = "sliding_windows_outputs"

) {
  user_run_id <- paste(user_id, run_id, sep="_")

  e = list()
  e$user_run_id <- user_run_id

  e$crash_table <- as.character(glue::glue('local_user_data.crashes_{user_run_id}'))
  e$crash_id <- as.character(glue::glue('tdg_id_{user_run_id}'))
  e$crash_geom <- crash_geom
  e$crash_mode <- crash_mode
  e$crash_sev <- crash_sev
  e$crash_year <- DBI::dbQuoteIdentifier(con, as.character(get_crash_year(con, user_id, run_id)))
  e$max_yr <- get_max_year(con, user_id, run_id)
  e$min_yr <- e$max_yr - 4
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

  e$roads_table <- as.character(glue::glue('local_user_data.roads_{user_run_id}'))
  e$road_id <- as.character(glue::glue('tdg_id_{user_run_id}'))
  e$road_geom <- road_geom
  e$road_name <- DBI::dbQuoteIdentifier(con, as.character(DBI::dbGetQuery(con, glue::glue("SELECT road_o_name FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = \'{run_id}\';"))[1,1]))
  e$road_fclass <- road_fclass
  e$fclass_values <- fclass_values
  if (exclude_highways) {
    e$road_clause <- "road_fclass NOT IN ('Expressway', 'Omit From Analysis') AND road_fclass IS NOT NULL"
  } else {
    e$road_clause <- "road_fclass NOT IN ('Omit From Analysis') AND road_fclass IS NOT NULL"
  }

  e$study_area <- as.character(glue::glue('local_user_data.study_area_{user_run_id}'))

  e$step_len <- step_len
  e$window <- window
  e$buff_dist <- buff_dist

  e$scratch_study_area_table <- as.character(glue::glue("scratch.sw_study_area_{user_run_id}"))
  e$scratch_crashes_table <- as.character(glue::glue("scratch.sw_crashes_{user_run_id}"))
  e$scratch_roads_table <- as.character(glue::glue("scratch.sw_roads_{user_run_id}"))
  e$scratch_excluded_crashes_table <- as.character(glue::glue("scratch.sw_crashes_excluded_{user_run_id}"))
  e$scratch_roads_dissolve_table <- as.character(glue::glue("scratch.sw_roads_dissolve_{user_run_id}"))
  e$scratch_sliding_windows_table <- as.character(glue::glue("sliding_windows_outputs.sw_sliding_windows_{user_run_id}"))
  e$scratch_short_windows_table <- as.character(glue::glue("scratch.sw_short_windows_{user_run_id}"))
  e$crosswalk_table_name <- as.character(glue::glue("scratch.sw_xwalk_table_{user_run_id}"))
  e$window_crash_raw <- as.character(glue::glue("scratch.sw_window_crash_raw_{user_run_id}"))
  e$out_road_table_name <- as.character(glue::glue("scratch.sw_output_roads_{user_run_id}"))
  e$crosstab_table_name <- as.character(glue::glue("scratch.sw_crosstab_table_{user_run_id}"))

  e$con <- con
  e$crs <- DBI::dbGetQuery(con, glue::glue("SELECT DISTINCT ST_SRID(geom) FROM {e$study_area};"))[1,1]


  names(fars_mode_codes) <- modes
  e$fars_mode_codes <- fars_mode_codes
  #e$low_severity_deflate_factor <- low_severity_deflate_factor
  # e$stan_model_file <- stan_model_file
  #e$stan_outputs_table <- as.character(glue::glue("sw_stan_outputs_{user_run_id}"))
  e$automated_schema <- automated_schema

  return(e)
}

#------hin_1_create_initial_scratch_tables------
hin_1_create_initial_scratch_tables = function(con=NULL, inputs=NULL) {
  tryCatch({
    # read inputs to the function environment
    list2env(inputs, envir=environment())

    #----Create scratch study area----
    DBI::dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_study_area_table};
                               CREATE TABLE {scratch_study_area_table} AS (
                               SELECT geom
                               FROM {study_area}
                               );
                               "))
    DBI::dbGetQuery(con, glue::glue("
                               DROP INDEX IF EXISTS sidx_sw_scratch_study_area_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_scratch_study_area_{user_run_id} ON {scratch_study_area_table} USING GIST(geom);
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

    DBI::dbGetQuery(con, glue::glue("
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
                               ADD study_area_flag BOOLEAN DEFAULT TRUE
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
                               END
                               FROM
                               {scratch_study_area_table} b
                               ;

                               UPDATE
                                {scratch_crashes_table} a
                                SET
                                study_area_flag = FALSE
                                FROM {scratch_study_area_table} b
                                WHERE ST_INTERSECTS(a.geom, b.geom)
                                ;
                               "))

    DBI::dbGetQuery(con, glue::glue("
                               DROP INDEX IF EXISTS sidx_sw_scratch_crashes_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_scratch_crashes_{user_run_id} ON {scratch_crashes_table} USING GIST(geom);
                               ANALYSE {scratch_crashes_table}(geom);
                               "))
    print("Initial crashes table made. Cleaning crashes...")

    DBI::dbGetQuery(con, glue::glue("
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
    DBI::dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {scratch_roads_table};
                               CREATE TABLE {scratch_roads_table} AS (
                               SELECT DISTINCT {road_id}, {road_name} as road_name, {road_fclass} as road_fclass, {road_geom} as geom
                               FROM {roads_table}
                               );
                               DROP INDEX IF EXISTS sidx_sw_scratch_roads_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_scratch_roads_{user_run_id} ON {scratch_roads_table} USING GIST(geom);
                               ANALYSE {scratch_roads_table}(geom);
                               "))
    print("Scratch roads table made")

    #----Create scratch national fclass priors----
    mode_reclass <- ""
    for (x in names(fars_mode_codes)){mode_reclass <- paste0(mode_reclass, glue::glue(paste0("WHEN mode = '{fars_mode_codes[x]}' THEN '{x}' " )))
    }
    where_clause <- make_expression(fars_mode_codes, "'%s'", ",")
    where_clause <- as.character(glue::glue("b.mode in ({where_clause})"))
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
    DBI::dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {out_road_table_name};
                               CREATE TABLE {out_road_table_name} AS (
                               SELECT
                               DISTINCT a.{road_id},
                               a.road_name,
                               a.road_fclass,
                               a.geom
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

                               DELETE FROM {out_road_table_name}
                               WHERE length = 0;

                               DROP INDEX IF EXISTS sidx_sw_geom_out_road_table_name_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_geom_out_road_table_name_{user_run_id} ON {out_road_table_name} USING GIST(geom);
                               ANALYSE {out_road_table_name}(geom);

                               ALTER TABLE {out_road_table_name}
                               ADD PRIMARY KEY ({road_id})
                               ;
                               "))
    print("Output roads table made")

    DBI::dbGetQuery(con, glue::glue("
                               DROP TABLE IF EXISTS {crosswalk_table_name};
                               CREATE TABLE {crosswalk_table_name} AS (
                               SELECT
                               crash_id,
                               road_id
                               FROM (
                               SELECT
                               a.{crash_id} AS crash_id,
                               b.{road_id} AS road_id
                               FROM
                               {scratch_crashes_table} a,
                               {out_road_table_name} b
                               WHERE
                               ST_DWITHIN(a.geom, b.geom, {buff_dist})
                               ) x
                               );

                               DROP INDEX IF EXISTS idx_sw_crosswalk_table_crash_id_{user_run_id};
                               CREATE INDEX idx_sw_crosswalk_table_crash_id_{user_run_id} ON {crosswalk_table_name}(crash_id);
                               ANALYSE {crosswalk_table_name}(crash_id);

                               DROP INDEX IF EXISTS idx_sw_crosswalk_table_name_road_id_{user_run_id};
                               CREATE INDEX idx_sw_crosswalk_table_name_road_id_{user_run_id} ON {crosswalk_table_name}(road_id);
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

    DBI::dbGetQuery(con, glue::glue("
                               ALTER TABLE {out_road_table_name}
                               {add_cols};
                               "))
    print("Columns added to output road table")

    DBI::dbGetQuery(con, glue::glue("
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
    DBI::dbGetQuery(con, glue::glue("
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

    # mem_use <- 0
    # for (itm in ls()) {mem_use <- mem_use + object.size(get(itm))}
    # print(glue::glue("Memory used in function = {mem_use} bytes"))
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
    DBI::dbGetQuery(con, glue::glue("
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

    DBI::dbGetQuery(con, glue::glue("
                               DROP INDEX IF EXISTS idx_sw_road_name_dissolve_{user_run_id};
                               CREATE INDEX IF NOT EXISTS idx_sw_road_name_dissolve_{user_run_id} ON {scratch_roads_dissolve_table}(road_name);
                               ANALYSE {scratch_roads_dissolve_table}(road_name);

                               DROP INDEX IF EXISTS idx_sw_road_fclass_dissolve_{user_run_id};
                               CREATE INDEX IF NOT EXISTS idx_sw_road_fclass_dissolve_{user_run_id} ON {scratch_roads_dissolve_table}(road_fclass);
                               ANALYSE {scratch_roads_dissolve_table}(road_fclass);

                               DROP INDEX IF EXISTS sidx_sw_dissolve_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_dissolve_{user_run_id} ON {scratch_roads_dissolve_table} USING GIST(geom);
                               ANALYSE {scratch_roads_dissolve_table}(geom);

                               ALTER TABLE {scratch_roads_dissolve_table} ADD length FLOAT;

                               UPDATE {scratch_roads_dissolve_table} SET length = ST_Length(geom);

                               DELETE FROM {scratch_roads_dissolve_table}
                               WHERE length = 0;

                               DROP INDEX IF EXISTS idx_sw_length_dissolve_{user_run_id};
                               CREATE INDEX IF NOT EXISTS idx_sw_length_dissolve_{user_run_id} ON {scratch_roads_dissolve_table}(length);
                               ANALYSE {scratch_roads_dissolve_table}(length);
                               "))
    print("Roads dissolved by name, functional class, and proximity.")



    #------Make sliding windows------

    # Get max number of iterations on the longest window segment
    max_iter <- DBI::dbGetQuery(con, glue::glue("SELECT CEILING(MAX(length / {step_len}))::INT FROM {scratch_roads_dissolve_table};"))[1,1]

    #----Make long windows----
    print("Making sliding windows")
    DBI::dbGetQuery(con, glue::glue("
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

                               DROP INDEX IF EXISTS sidx_sw_window_geom_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_window_geom_{user_run_id} ON {scratch_sliding_windows_table} USING GIST(geom);
                               ANALYSE {scratch_sliding_windows_table}(geom);

                               UPDATE
                               {scratch_sliding_windows_table}
                               SET
                               length = ST_Length(geom);
                               "))

    #----Make Short Windows----
    print("Making short windows")
    DBI::dbGetQuery(con, glue::glue("
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

                               DROP INDEX IF EXISTS sidx_sw_short_window_geom_{user_run_id};
                               CREATE INDEX IF NOT EXISTS sidx_sw_short_window_geom_{user_run_id} ON {scratch_short_windows_table} USING GIST(geom);
                               ANALYSE {scratch_short_windows_table}(geom);
                               "))



    #----Joining crashes to windows----
    print("Adding crash columns to window tables")
    mode_sev_cols <- paste(rep(modes, each = length(sevs)), sevs, sep = "_")
    add_cols <- make_expression(mode_sev_cols, 'ADD IF NOT EXISTS %s FLOAT DEFAULT 0', ',')

    DBI::dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_sliding_windows_table}
                               {add_cols};

                               ALTER TABLE {scratch_short_windows_table}
                               {add_cols};
                               "))

    print("Performing window density analysis")
    DBI::dbGetQuery(con, glue::glue("
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

    for (c in mode_sev_cols) {
      DBI::dbGetQuery(con, glue::glue("
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

    DBI::dbGetQuery(con, glue::glue("
                               ALTER TABLE {scratch_sliding_windows_table}
                               {add_cols};

                               ALTER TABLE {scratch_short_windows_table}
                               {add_cols};
                               "))

    for (m in modes) {
      score_expression <- make_expression(paste0(m, "_", names(wgt), "*", wgt), "%s", "+")
      DBI::dbGetQuery(con, glue::glue("UPDATE {scratch_sliding_windows_table} SET {m}_score = {score_expression};"))
    }
    print("All scores added to sliding windows")

  }, error = function(cond){
    print(cond)
  })
}


#------hin_run_sliding_windows------
hin_run_sliding_windows = function(con=NULL, inputs=NULL, run_id, user_id) {

  hin_1_create_initial_scratch_tables(con, inputs)
  hin_2_join_crashes_to_roads(con, inputs)
  hin_3_create_sliding_windows(con, inputs)
 
  # runid <- DBI::dbQuoteLiteral(con, run_id)
  # login_names_q <- glue::glue('SELECT o_username FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {runid};')
  # login_name <- DBI::dbGetQuery(con, login_names_q)[1,1]
  # 
  # email_access_q <- glue::glue('SELECT email FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {runid};')
  # email_access <- DBI::dbGetQuery(con, email_access_q)[1,1]
  # 
  # print(email_access)
  # print(paste0('Sending notification to, ', email_access, '.'))
  # email <- emayili::envelope(
  #   to = email_access,
  #   from = Sys.getenv("SSPF_EMAIL_ADDRESS"),
  #   subject = paste0("SSPF - Results Ready, ", login_name),
  #   text = paste0('This is an automated message to notify you that the sliding windows analysis for study, ', run_id,', is ready for your review. Thanks for using the Safer Streets Priority Finder!')
  # )
  # 
  # smtp <- emayili::server(host = "mail.hover.com",
  #                         port = 465,
  #                         username = Sys.getenv("SSPF_EMAIL_ADDRESS"),
  #                         password = Sys.getenv("SSPF_EMAIL_PASSWORD"),
  #                         reuse=F)
  # smtp(email, verbose = TRUE)
  
  
  
  #print("Dropping scratch tables")
  # for (x in inputs) {
  #   if (is.character(x)) {
  #     if (startsWith(toString(x), "scratch.")) {
  #         DBI::dbGetQuery(con, glue::glue("
  #                               DROP TABLE IF EXISTS {x};
  #                              "))
  #       }}}
}
