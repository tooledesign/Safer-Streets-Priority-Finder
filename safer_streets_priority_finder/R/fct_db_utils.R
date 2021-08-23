 
# returns a postgres connection 
create_postgresql_connection <- function(drv=RPostgreSQL::PostgreSQL(max.con = 160), dbname, host, port=5432, user, password){
  # creates a postgresql connection depends on RPostgreSQL and DBI libraries 
  # dbname   = database to connect to 
  # host     = IP address of PostgreSQL database you'd like a connection to 
  # port     = default 5432
  # user     = UN to access DB 
  # password = UN password to use to connect to DB

  tryCatch({
    # create connection 
    connection <- pool::dbPool(
      drv      =  RPostgreSQL::PostgreSQL(),
      dbname   = dbname,
      host     = host,
      port     = port,
      user     = user,
      password = password,
      idleTimeout = 3600000/4  # one hour
    )
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
  #return connection object
  return(connection)
}

# tests if value exists 
test_if_value_exists <- function(
  connection = NULL, 
  schema=NULL,
  table=NULL,
  column=NULL,
  value=NULL
) {
  
  tryCatch({
    schema=DBI::dbQuoteIdentifier(connection, schema)
    table=DBI::dbQuoteIdentifier(connection, table)
    value=DBI::dbQuoteLiteral(connection, value)
    column=DBI::dbQuoteIdentifier(connection, column)
    #query
    q <- glue::glue('SELECT TRUE FROM {schema}.{table} WHERE {column} = {value} LIMIT 1;')
  
    # send query 
    rs <-  DBI::dbGetQuery(connection ,q)
    
    if (rs[1,]) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, 
  error = function(cond){
    return(FALSE)
  })
  
}

# tests if table exists 
test_if_table_exists <- function(
  connection = NULL, 
  schema=NULL,
  table=NULL,
  column=NULL,
  info_schema='tables'
  ) {
 
  tryCatch({
    if (!is.null(column)) {
      column <- glue::glue('column_name = {column}')
    } else {
      column <- 'TRUE'
    }
    
  q  <- glue::glue("
                    SELECT EXISTS (
                     SELECT FROM information_schema.{info_schema} 
                     WHERE  table_schema = {schema}
                     AND    table_name   = {table}
                     AND {column}
                     );
                   ")
 
  # send query 
  rs <-  DBI::dbGetQuery(connection ,q)
  
  if (rs[1,]) {
    return(TRUE)
  } else {
      return(FALSE)
    }
  }, 
  error = function(cond){
    return(FALSE)
  })
}

 
# transforms CRS 
psql_update_epsg <- function(connection = NULL, 
                             table=NULL, 
                             geom='geom', 
                             new_epsg = NULL, 
                             geom_type=NULL,
                             schema='local_user_data') {
  # connection = DBI::dbConnect connection 
  # table_name = schema and table names, an example would be, c('static', 'roads')
  # new_epsg = target epsg integer format 
  tryCatch({

  new_epsg <- as.integer(new_epsg)
  
  if (is.null(geom)){
    geom = 'geom'
  }
  
  if ( is.null(geom_type) ) {
    g_q <- glue::glue("
                    SELECT DISTINCT GeometryType({geom}) 
                    FROM {schema}.{table} WHERE {geom} IS NOT NULL LIMIT 1;
                    ")
    
    geomtype <- toString(DBI::dbGetQuery(connection, g_q))
  }

  q <- glue::glue("ALTER TABLE {schema}.{table} 
                   ALTER COLUMN {geom} TYPE geometry({geomtype}, {new_epsg}) 
                   USING ST_Transform({geom}, {new_epsg});
                  ")
  
  DBI::dbGetQuery(connection, q)  
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}


# creates primary key 
psql_create_pkey <- function(connection = NULL, 
                             table=NULL, 
                             pkey_name = NULL,
                             schema='local_user_data'){
  # connection = DBI::dbConnect connection 
  # table_name = schema and table names, an example would be, c('static', 'roads')
  # pkey_name = name of new column 
  table <- DBI::dbQuoteIdentifier(connection, table)  

  query <- glue::glue("
                      ALTER TABLE {schema}.{table} 
                      ADD COLUMN {pkey_name} SERIAL PRIMARY KEY;
                     ")
  

  tryCatch({
    DBI::dbGetQuery(connection, query)  
  }, 
  error = function(cond){
    c <- paste0(toString(cond), 'It\'s likely that a) the column you\'ve specified already exists, or b) your table already has a primary key assigned.')
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# creates unique spatial index 
psql_add_s_index <- function(connection=NULL, 
                             table, geom='geom', 
                             index_name=NULL,
                             schema='local_user_data'){
  # connection = DBI::dbConnect connection 
  # table_name = schema and table names, an example would be, c('static', 'roads')
  # index_name = name of index_name
  if (is.null(geom)==TRUE){
    geom = 'geom'
  }
  
  index_name <- paste0(index_name, toString(round(as.numeric(Sys.time())), 0))

  query <- glue::glue('
                      DROP INDEX IF EXISTS {index_name};
                      CREATE INDEX {index_name} 
                      ON {schema}.{table}
                      USING GIST ({geom}); 
                      ANALYZE {schema}.{table};
                     ')
  
  tryCatch({
    DBI::dbGetQuery(connection, query)  
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# sends data to postgres database 
to_postgresql <- function(
  # this is a custom function for sending a spatial data frame to a PostgreSQL database. It's real purpose is to reduce the code line count. Importantly, it's setup to take a table that's not sql formatted (i.e., not with DBI::dbQuoteIdentifier(con, table))
  connection, 
  geodata, 
  schema, #schema of target table 
  table, # id of user's run 
  user_id,
  run_id,
  promote_to_multi=FALSE,
  geom='geom',
  srid=4326,
  geom_type,
  sindex=TRUE,
  w=NULL,
  msgs=NULL # takes 3 messages
)
  {
  tryCatch({
    
    print('Sending data as wkt')
    sf_to_postgres_as_wkt(connection=connection, table=table, schema=schema, geodata=geodata, promote_to_multi=promote_to_multi, geom_type=geom_type)
    
    if(!is.null(w) && !is.null(msgs)){
      w$update(html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML(msgs[1]))
      ))
    }
    
    print("Creating geom from WKT")
    create_geom_from_wkt(connection=connection, schema=schema, table=table, srid=srid, geom_type=geom_type)
    if(!is.null(w) && !is.null(msgs)){
      w$update(html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML(msgs[2]))
      ))
    }
    
    print('Creating primary key')
    psql_create_pkey(connection = connection, table=table, pkey_name = paste0("tdg_id_", toString(user_id), '_', toString(run_id)))
    if(!is.null(w) && !is.null(msgs)){
      w$update(html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML(msgs[3]))
      ))
    }
    
    if(sindex) {
      print("Creating spatial index")
      psql_add_s_index(connection = connection, table = table, index_name = paste0('index_', toString(user_id), '_', toString(run_id), '_'))
    }
  }, 
  error = function(cond){
    c <- toString(cond)
    print(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# send spatial data to postgresql database with spatial information in well-known-text format 
sf_to_postgres_as_wkt <- function (connection, 
                                   schema, 
                                   table, 
                                   geodata,
                                   promote_to_multi=F,
                                   geom_type
) {
 
  geodata <- sf::st_as_sf(geodata)
  if (promote_to_multi) {
    geodata <- sf::st_cast(geodata, geom_type)
  }
  geodata$geometry <- sf::st_as_text(geodata$geometry, EWKB=T)
  geodata <- as.data.frame(geodata)
  DBI::dbExecute(connection, glue::glue("SET search_path TO {schema}"))
  
  ggg <- Sys.time()
  DBI::dbWriteTable(connection,
                    table,
                    geodata, 
                    row.names=FALSE,
                    overwrite=TRUE,
                    append=FALSE,
                    temporary=FALSE
  )
  gggg <- Sys.time() 
  print(paste0('dbWriteTable time:', gggg - ggg))
  DBI::dbExecute(connection, glue::glue("SET search_path TO public"))
 

  print('Done with sf_to_postgres_as_wkt()')
}

# Creates geometry on local spatial table from column in well-known-text format
create_geom_from_wkt <- function(connection, 
                                 schema, # name of target table schema 
                                 table,  # name not target table 
                                 geom_type, # type of geometry (e.g., POLYGON, MULTILINESTRING)
                                 srid    # srid of geometry to create (e.g., 4326)
) {
  tryCatch({
  q <- glue::glue('ALTER TABLE {schema}.{table}
                   ADD COLUMN geom geometry ({geom_type}, {srid});
                  
                   UPDATE {schema}.{table} 
                   SET geom = ST_GeometryFromText(geometry, {srid});
                   ')

  DBI::dbGetQuery(connection, q)

  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# downloads spatial table into R console
fetch_spatial_table <- function (
  connection, # db connection object
  columns=NULL, # list of column name to be selected. 
  schema,  # schema of table 
  table,  # table name 
  clauses=NULL,   # where clause 
  geom_type=NULL,  # geometry to cast to
  is_wkt=TRUE
){
  
  tryCatch({

    if ( !is_wkt ) {
      print('geoms must be downloaded in wkt format') 
    } else {
      
      if (is.null(clauses) || is.na(clauses) ){
        clauses = 'WHERE TRUE'
      } 
    
      if (is.null(columns)) {
        schema_literal <- DBI::dbQuoteLiteral(connection, schema)
        table_literal <- DBI::dbQuoteLiteral(connection, table)  
        
        q <- glue::glue('
                        SELECT  array_to_string(ARRAY(SELECT \'"\' || c.column_name || \'"\'
                                                                  FROM information_schema.columns As c
                                                                  WHERE table_name = {table_literal}
                                                                  AND table_schema = {schema_literal}
                                                                  AND  c.column_name NOT IN(\'geom\')
                        ), \',\') || \', ST_ASEWKT(geom) as geom\' As sqlstmt
                  ')
 
        columns <-  DBI::dbGetQuery(connection,q)
      } 
    
      schema <- DBI::dbQuoteIdentifier(connection, schema)
      table <- DBI::dbQuoteIdentifier(connection, table)  
  
      q  <- glue::glue("SELECT {columns} FROM {schema}.{table} {clauses};")

      spatial_data <-  DBI::dbGetQuery(connection,q)

      spatial_data <- sf::st_as_sf(spatial_data, wkt='geom')
  
      if ( !is.null(geom_type) && !is.na(geom_type) ) {
        spatial_data <- sf::st_cast(spatial_data, toString(geom_type))
      } 
      
      if (is.null(spatial_data)) {
        return(NULL)
      } else {
        return(spatial_data)  
      }
    }}, 
    error = function(cond){
      c <- toString(cond)
      Add_Modal(title = 'Something Went Wrong.', body=c)
    })
  }

# deletes table on postgresql database 
delete_tables <- function (
  connection=NULL, 
  list_of_tables=list() #list of schema, table combinations c('schema', 'table')
) {
  tryCatch({
  for (i in list_of_tables) {
      schema <- DBI::dbQuoteIdentifier(connection, i[1])
      table <- DBI::dbQuoteIdentifier(connection, i[2]) 
      q <- glue::glue('DROP TABLE IF EXISTS {schema}.{table};')
 
      DBI::dbGetQuery(connection,q)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns table name with user_id and run_id, commonly used throughout the application 
return_table_name <- function (
  table_name=NULL, # name of table 
  user_id=NULL, # ID of user
  run_id=NULL #ID of run 
) {
  tryCatch({
  table_name <-  toString(table_name)
  user_id    <-  toString(user_id)
  run_id     <-  toString(run_id)
  return(toString(glue::glue('{table_name}_{user_id}_{run_id}')))
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# creates list of table names 
create_list_user_tbls <- function (
  user_id,
  run_id,
  list_of_tbl_prefixes
) {
  tryCatch({
  ts <- c()
  for (i in list_of_tbl_prefixes) {
    t <- paste0(i, '_', user_id, '_', run_id)
    ts <- append(ts, t)
  }
  if(length(ts) > 0){
    return(ts)
  } else {
    return(FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# checks for whether user has uploaded thier data 
check_for_user_inputs <- function ( 
  connection, 
  user_id, 
  run_id,
  list_of_tables, 
  schema='local_user_data'
) {
  tryCatch({
  c <- c()
  for (i in list_of_tables){
    i <- DBI::dbQuoteLiteral(connection, i)
    crs <- test_if_table_exists(connection=connection, schema=schema, table=i)
    c <- append(c, crs)
  } 
  if (all(c) == TRUE){
    return(TRUE)
  } else {
    return(FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns geometry type of a given table 
get_geom_type <- function (
  connection, 
  schema,
  table,
  geom='geom'
) {
  tryCatch({
  query <- glue::glue("SELECT DISTINCT GeometryType({geom})
                       FROM {schema}.{table}
                       ;")
  geometry <- DBI::dbGetQuery(connection, query)
  if (!is.null(geometry) && !is.na(geometry)) {
    return(geometry)
  } else {
    return (FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# test whether object's bounding box overlap 
test_bbox_overlap <- function ( 
  # efficient method for testing overlapping boundary boxes 
  connection, 
  user_id, 
  run_id,
  test_shape # shape to test against user's account
) {
  tryCatch({
  query <- glue::glue("SELECT  sa_bbox_south_4326, sa_bbox_north_4326, sa_bbox_east_4326, sa_bbox_west_4326 
                        FROM   gen_management.accounts 
                       WHERE   user_id = {user_id}
                         AND   run_id  = {run_id}
                         ;"
  )
  account_bbox <- DBI::dbGetQuery(connection, query)
  
  if (!is.null(account_bbox)){
    
    shape_bbox <- sf::st_bbox(test_shape)
    # shapebox[1] xmin or western edge
    # shapebox[2] ymin or southern edge 
    # shapebox[3] xmax or eastern edge 
    # shapebox[4] ymax or northern edge
 
    if( as.numeric(account_bbox$sa_bbox_east_4326[1]) < as.numeric(shape_bbox[1])) {
     sa_west_of <- TRUE
    } else {
     sa_west_of <- FALSE
    }
 
    if (as.numeric(account_bbox$sa_bbox_west_4326[1]) > as.numeric(shape_bbox[3])) {
      sa_east_of <- TRUE
    } else {
      sa_east_of <- FALSE
    }
  
    if( as.numeric(account_bbox$sa_bbox_south_4326[1]) >  as.numeric(shape_bbox[4])) {
      sa_north_of <- TRUE
    } else {
      sa_north_of <- FALSE
    }
  
    if ( as.numeric(account_bbox$sa_bbox_north_4326[1]) <  as.numeric(shape_bbox[2]) ) {
      sa_sourth_of <- TRUE
    } else {
      sa_sourth_of <- FALSE
    }
      
    if ( sa_west_of || sa_east_of || sa_north_of || sa_sourth_of ) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# checks in spatial features intersect, on postgresql db 
intersect_server_side <- function(
  connection, 
  user_id, 
  run_id,
  schema1,
  schema2,
  table1,
  table2
) {
  tryCatch({
    
    query <- glue::glue("SELECT  DISTINCT 1
                        FROM   (SELECT ST_Envelope(ST_Union(r.geom)) as geom FROM {schema1}.{table1} r) t1, 
                               (SELECT ST_Envelope(ST_Union(s.geom)) as geom FROM {schema2}.{table2} s) t2 
                       WHERE   ST_INTERSECTS(t1.geom, t2.geom) 
                         ;"
    )
    
    intersect <- DBI::dbGetQuery(connection, query)
    
    if (length(intersect) == 0) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, error = function(cond){
    return(FALSE)
  }
  )
}

# checks to see if data were selected from nationally available data or not 
sa_from_nationally_aval_data <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
  query <- glue::glue("SELECT sa_o_source
                       FROM gen_management.accounts
                       WHERE user_id = {user_id}
                         AND run_id = {run_id}
                       ;")
 
  sa_source <- DBI::dbGetQuery(connection, query)
  b <-grep('US Census', sa_source ) 
  if ( length(b) > 0) {
    return(TRUE)
  } else {
    return (FALSE)
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}


# downloader, downloads user data and delivers via the browser 
downloader <- function(connection, schema, table, filename, geom_type, file, clauses=NULL, columns=NULL, custom_table=NULL) {
  tryCatch({
  if (is.null(custom_table)) {
    table <- fetch_spatial_table(connection = connection, 
                                 schema = schema,
                                 table =  table,
                                 geom_type = geom_type,
                                 columns = columns,
                                 clauses = clauses
    )
  } else (
    table <- custom_table
  )
  
  if (length(Sys.glob(glue::glue("{filename}.*")))>0){
    file.remove(Sys.glob(glue::glue("{filename}.*")))
  }

  st_write(table, dsn=glue::glue("{filename}.shp"), layer=glue::glue("{filename}"), driver="ESRI Shapefile", delete_layer = T)
  
  if (length(grep('Safer Streets Model', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries','model_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Sliding Windows Analysis', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'sliding_windows_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Top 10 Pedestrian', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'top10_ped_cor_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Top 10 Bicycle', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'top10_bike_cor_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Top 10 Other', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'top10_other_cor_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Crashes', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'crash_data_dictionary.txt')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else if (length(grep('Road Networks', filename)) >= 1) {
    dataPath <- file.path(getwd(), 'data_dictionaries', 'roads_data_dictionary')
    the_files <- c(Sys.glob(glue::glue("{filename}.*")), dataPath)
  } else {
    the_files <-  Sys.glob(glue::glue("{filename}.*")) 
  }

  zip::zip(zipfile=glue::glue('{filename}.zip'), files=the_files, mode="cherry-pick")
  file.copy(glue::glue("{filename}.zip"), file)
  if (length(Sys.glob(glue::glue("{filename}.*")))>0){
    file.remove(Sys.glob(glue::glue("{filename}.*")))
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# creates FARS table on postgresql database 
create_fars_data <- function (
  connection,
  user_id,
  run_id
) {
  tryCatch({
   run_id_sql_formatted <- DBI::dbQuoteLiteral(connection, run_id)
   crashes_utm <- get_account_crs(connection = connection,user_id = user_id,run_id = run_id_sql_formatted)
   crash_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id))
   study_area_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id))
 
      create_county_query <- glue::glue('DROP TABLE IF EXISTS local_user_data.{crash_table}; 
                                             SELECT f.pkey, f.st_case, f.crash_mode, 
                                                    f.crash_severity, f.crash_year, f.state_fp, 
                                                    f.county_fp, f.place_fp, f.functional_class, 
                                                    TRUE::BOOLEAN as in_sa_{user_id}_{run_id},
                                                    (ST_Dump(f.geom)).geom as geom
                                             INTO local_user_data.{crash_table}
                                             FROM static.fars_processed f, (SELECT ST_Transform(geom,4326) as geom 
                                             FROM local_user_data.{study_area_table} WHERE geom IS NOT NULL) c
                                             WHERE ST_INTERSECTS(c.geom, f.geom);
                                             
                                         
                                            ')
      print(create_county_query)
    
      
      DBI::dbExecute(connection, create_county_query)
      create_county_query <- glue::glue('
                                           ALTER TABLE local_user_data.{crash_table}
                                           ALTER COLUMN geom TYPE geometry(POINT,{crashes_utm}) 
                                           USING ST_Transform(geom,{crashes_utm});
                                           
                                           DROP INDEX IF EXISTS index_{user_id}_{run_id}_crashes;
                                           CREATE INDEX index_{user_id}_{run_id}_crashes
                                           ON local_user_data.{crash_table}
                                           USING GIST (geom);
                                           ANALYZE local_user_data.{crash_table};
                                           
                                           UPDATE gen_management.accounts 
                                           SET crs = {crashes_utm}
                                           WHERE user_id = {user_id} 
                                           AND run_id = {run_id_sql_formatted};
                                           
                                           ALTER TABLE local_user_data.{crash_table}
                                           ADD COLUMN crashes_costs_usdot VARCHAR (40),
                                           ADD COLUMN severity_mapped VARCHAR (40),
                                           ADD COLUMN usdot_mode_mapped VARCHAR (40);
                                           
                                           ALTER TABLE local_user_data.{crash_table}
                                           ADD COLUMN tdg_id_{user_id}_{run_id} BIGSERIAL PRIMARY KEY;
                                           
                                           UPDATE local_user_data.{crash_table}
                                           SET usdot_mode_mapped = CASE 
                                            WHEN crash_mode = \'pedestrian\' 
                                              THEN \'Pedestrian Crash\'
                                            WHEN crash_mode = \'bicyclist\'
                                              THEN \'Bicycle Crash\' 
                                           ELSE \'Other Crash\'
                                            END; 
                                           
                                           UPDATE local_user_data.{crash_table}
                                           SET crashes_costs_usdot = \'11326039\';
                                           
                                           UPDATE local_user_data.{crash_table}
                                           SET severity_mapped = \'Fatality (K)\';
                                           
                                           UPDATE gen_management.accounts
                                           SET crash_o_source = \'Nationally Available FARS Data, NHTSA (2015 - 2019). Selected on {Sys.time()}\',
                                               crash_o_rep_id_col = \'Not recorded\',
                                               crash_o_mode_col = \'crash_mode\',
                                               crash_o_year_col = \'crash_year\',
                                               crash_o_serv_col = \'crash_severity\'
                                           WHERE user_id = {user_id} AND run_id = {run_id_sql_formatted};
                                           
                                             ALTER TABLE local_user_data.crashes_{user_id}_{run_id} 
                                             ADD COLUMN fclass_mapped TEXT DEFAULT \'Unknown Functional Classification\';
                        
                                             UPDATE local_user_data.crashes_{user_id}_{run_id} t
                                             SET fclass_mapped = usdot_fun_class_mapped
                                             FROM
                                                 (SELECT 
                                                  DISTINCT ON (c.tdg_id_{user_id}_{run_id}) c.tdg_id_{user_id}_{run_id}, 
                                                  i.usdot_fun_class_mapped
                                                 FROM 
                                                  local_user_data.crashes_{user_id}_{run_id} c, local_user_data.roads_{user_id}_{run_id} i
                                                 WHERE 
                                                  ST_DWithin(c.geom, i.geom, 250)
                                                  
                                                 ORDER BY 
                                                  c.tdg_id_{user_id}_{run_id}, 
                                                  ST_Distance(c.geom, i.geom)
                                                  ) z
                                             WHERE t.tdg_id_{user_id}_{run_id} = z.tdg_id_{user_id}_{run_id};
                                            
                                           
                                          ')
   
      DBI::dbGetQuery(connection, create_county_query)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns fars table from postgresql database 
download_fars <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
  study_area_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id))
  #retrieve FARS data  
  q <- glue::glue('
                  SELECT pkey, CASE WHEN crash_mode = \'pedestrian\' THEN \'Pedestrian Crash\' 
                                    WHEN crash_mode = \'bicyclist\' THEN \'Bicycle Crash\' 
                                    ELSE \'Other Crash\' END as crash_mode, 
                                    ST_AsEWKT((ST_Dump(f.geom)).geom) as geom_wkt 
                  FROM static.fars_processed f, (SELECT ST_Transform(geom,4326) as geom 
                                                    FROM local_user_data.{study_area_table} 
                                                    WHERE geom IS NOT NULL) c
                  WHERE ST_CONTAINS(c.geom, f.geom) ;
                  ')
 
  crashes <-  DBI::dbGetQuery(connection,q)
 
  if (length(crashes) == 0) {
    return(crashes)
  } else {
    crashes <- sf::st_as_sf(as.data.frame(crashes), wkt='geom_wkt')
    crashes <- sf::st_cast(crashes, 'POINT')
    if (is.null(crashes) || is.na(crashes)){
      return(NULL)
    } else {
      return(crashes)
    }
  }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns **OSM** data from postgresql database 
# tiger data were originally used before team made the decision to switch to Open Street Map data 
download_tiger <-   function(
  connection,
user_id,
run_id
) {
 tryCatch({
  roads <- fetch_spatial_table(connection = connection,
                               columns='osm.name as fullname, ST_ASEWKT(osm.geom) as geom',
                                        schema = 'static',
                                        table =  glue::glue('osm_centerlines'),
                                        clauses = glue::glue('osm, local_user_data.study_area_{user_id}_{run_id} st
                                                             WHERE ST_INTERSECTS(ST_Transform(st.geom,4326), osm.geom)
                                                             '),
                                        geom_type = "LINESTRING",
                                        is_wkt = TRUE
  )
  if (length(roads) == 0) {
    return('No Roads')
  } else {
    if (is.null(roads) || is.na(roads)){
      return(NULL)
    } else {
      return(roads)
    }
  }
 }, 
 error = function(cond){
   c <- toString(cond)
   Add_Modal(title = 'Something Went Wrong.', body=c)
 })
}

# creates **OSM** data for user account in psql database 
# tiger data were originally used before team made the decision to switch to Open Street Map data 
create_account_tiger <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
  study_area_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id))
  roads_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id))
  run_id_sql_formatted <- DBI::dbQuoteLiteral(connection, run_id)
  
  roads_utm <- get_account_crs(connection = connection,user_id = user_id,run_id = run_id_sql_formatted)
  run_id_sql_formatted <- DBI::dbQuoteLiteral(connection, run_id)
 
 
  create_road_query <- glue::glue(' DROP TABLE IF EXISTS temp_{user_id}_{run_id};
                                     SELECT * 
                                     INTO TEMP temp_{user_id}_{run_id} 
                                     FROM local_user_data.study_area_{user_id}_{run_id} 
                                    ;
                                     
                                    ALTER TABLE temp_{user_id}_{run_id}
                                     ALTER COLUMN geom TYPE geometry(MultiPolygon,4326) 
                                      USING ST_TRANSFORM(ST_MULTI(geom),4326);
                                    
                                    CREATE INDEX temp_{user_id}_{run_id}_indx
                                      ON temp_{user_id}_{run_id}
                                      USING GIST (geom);
                                    ANALYZE temp_{user_id}_{run_id};
  
                                    DROP TABLE IF EXISTS temp_{user_id}_{run_id}_2;
                                     SELECT osm.way_id, osm.osm_type, osm.name, osm.ref, osm.oneway, osm.tags, osm.state_fp, osm.county_fp,
                                      CASE 
                                        WHEN osm.osm_type in (\'tertiary\', \'tertiary_link\') THEN \'Major Collector\'
                                        WHEN osm.osm_type in (\'secondary_link\', \'secondary\') THEN \'Minor Arterial\'
                                        WHEN osm.osm_type in (\'motorway\', \'motorway_link\') THEN \'Expressway\'
                                        WHEN osm.osm_type in (\'primary\', \'primary_link\', \'trunk\', \'trunk_link\') THEN \'Major Arterial\'
                                        WHEN osm.osm_type in (\'residential\', \'living_street\') THEN \'Local Road\'
                                        ELSE \'Omit From Analysis\'
                                        END as usdot_fun_class_mapped,
                                    osm.geom
                                    INTO TEMP TABLE temp_{user_id}_{run_id}_2
                                    FROM static.osm_centerlines osm, temp_{user_id}_{run_id} sa
                                    WHERE ST_INTERSECTS(osm.geom, sa.geom)
                                    ;
    
                                    DROP TABLE IF EXISTS local_user_data.{roads_table};
                                    SELECT osm.way_id, osm.osm_type, osm.name, osm.ref, 
                                           osm.oneway, osm.tags, osm.state_fp, osm.county_fp,
                                           osm.usdot_fun_class_mapped, 
                                           (ST_Dump(osm.geom)).geom as geom,
                                           ST_AsEWKT((ST_Dump(osm.geom)).geom) as geom_wkt
                                    INTO local_user_data.{roads_table}
                                    FROM temp_{user_id}_{run_id}_2 osm;
                                    
                                    DROP TABLE IF EXISTS temp_{user_id}_{run_id};
                                    DROP TABLE IF EXISTS temp_{user_id}_{run_id}_2;

                                    ALTER TABLE local_user_data.{roads_table}
                                    ADD COLUMN osm_id_{user_id}_{run_id} SERIAL PRIMARY KEY;

                                    ALTER TABLE local_user_data.{roads_table}
                                    ADD COLUMN tdg_id_{user_id}_{run_id} SERIAL;
                                    
                                    ALTER TABLE local_user_data.{roads_table}
                                    ALTER COLUMN geom TYPE geometry(LineString,{roads_utm})
                                    USING ST_Transform(geom,{roads_utm});

                                    DROP INDEX IF EXISTS index_{user_id}_{run_id}_roads_osm;
                                    CREATE INDEX index_{user_id}_{run_id}_roads_osm
                                    ON local_user_data.{roads_table}
                                    USING GIST (geom);
                                    ANALYZE local_user_data.{roads_table};
                                    
                                    ')
 
  print(create_road_query)
  
  DBI::dbGetQuery(connection, create_road_query)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# Updates attributes of accounts table with osm road attributes
update_osm_attributes <- function(  connection,
                                    user_id,
                                    run_id
                                    ){
  tryCatch({
  run_id_sql_formatted <- DBI::dbQuoteLiteral(connection, run_id)
  update <- glue::glue('UPDATE gen_management.accounts
                        SET road_o_id = \'osm_id_{user_id}_{run_id}\',
                             road_o_name = \'name\',
                             road_o_source = \'Nationally Available OSM Data. Selected on {Sys.time()}\',
                             roads_fun_c_col = \'osm_type\'
                         WHERE user_id = {user_id} AND run_id = {run_id_sql_formatted};
                          ')
  DBI::dbGetQuery(connection, update)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns a count of features given method  
get_count <- function(
  connection, 
  table,
  schema,
  attr, 
  method = 'SUM',
  where_clause = NULL
) {
  tryCatch({
  attr <- DBI::dbQuoteIdentifier(connection, attr)
  table <-  DBI::dbQuoteIdentifier(connection, table)
  schema <-  DBI::dbQuoteIdentifier(connection, schema)
 
  if (is.null(where_clause)){
    where_clause <- toString('TRUE')
  } 
  q <- glue::glue('
                  SELECT {method}({attr}) FROM {schema}.{table} WHERE {where_clause}
                  '
  )

  count <-  DBI::dbGetQuery(connection, q)
  return(as.integer(count))
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns tables associated with user_id 
get_users_tables <- function(
  connection, 
  user_id,
  run_id
) {
  tryCatch({
  q <- glue::glue(
    '
    SELECT table_schema || \'.\' || table_name as t
    FROM INFORMATION_SCHEMA.TABLES
    WHERE TABLE_NAME LIKE \'%_{user_id}_{run_id}\';
    '
  )
    tables <-  DBI::dbGetQuery(connection, q)
    return(tables)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
  }
 
# returns table in long format (crosswalk table)
get_crosswalk_table_points <- function(
  connection, 
  schema,
  table,
  values,
  filter=NULL 
) {
 tryCatch({
  # get string of attributes 
  v <- ""
  for (i in values) { v  <- paste(v, glue::glue('t."{i}"'), sep=",") }
  v <- sub(".", "", v)
  # create filter 
  if (is.null(filter)) {
    filter <- 'TRUE'
  } else {
    filter <- filter
  }
  
  q <- glue::glue(
    '
    WITH the_total AS (
      SELECT COUNT(g.geom) as total FROM {schema}.{table} g
    )
    SELECT 
      DISTINCT {v},
      COUNT(t.geom) as "Total Count",
      ROUND(CAST(COUNT(t.geom)::FLOAT/tt.total AS NUMERIC), 2) as "Proportion" 
    FROM {schema}.{table} t, the_total tt
    WHERE {filter}
    GROUP BY {v}, tt.total;
    '
  )
  table <-  DBI::dbGetQuery(connection, q)
  return(table)
 }, 
 error = function(cond){
   c <- toString(cond)
   Add_Modal(title = 'Something Went Wrong.', body=c)
 })
}

# returns table in long format (crosswalk table)
get_crosswalk_table_lines <- function(
  connection, 
  schema,
  table,
  values,
  filter=NULL 
) {
  tryCatch({
  # get string of attributes 
  v <- ""
  for (i in values) {  v  <- paste(v, glue::glue('t."{i}"'), sep=",") }
  v <- sub(".", "", v)
  # create filter 
  if (is.null(filter)) {
    filter <- 'TRUE'
  } else {
    filter <- filter
  }
  
  q <- glue::glue(
    '
    WITH the_total AS (
      SELECT SUM(ST_LENGTH(g.geom))/1609.344  as total FROM {schema}.{table} g
    )
    SELECT 
      DISTINCT {v},
      SUM(ST_LENGTH(t.geom)/1609.344) as "Total Miles",
      ROUND(CAST(SUM((ST_LENGTH(t.geom)/1609.344))/tt.total AS NUMERIC), 2) as "Proportion" 
    FROM {schema}.{table} t, the_total tt
    WHERE {filter}
    GROUP BY {v}, tt.total;
    '
  )
 
  table <-  DBI::dbGetQuery(connection, q)
  return(table)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns distinct value of column for a given user id and run id 
get_user_account_value <- function(
  connection,
  user_id, 
  run_id, 
  column
) {
  tryCatch({
  q <- glue::glue("SELECT DISTINCT {column} FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {run_id};")
  table <-  DBI::dbGetQuery(connection, q)
  return(table)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}
 
# this is a great function, returns all columns of table with geom in well-known-text format
get_columns <- function(
  connection, 
  run_id, 
  user_id, 
  table,
  columns_to_exclude,
  spatial=T
) {
  tryCatch({
  q <- glue::glue('
                        SELECT  array_to_string(ARRAY(SELECT \'"\' || c.column_name || \'"\'
                                                                  FROM information_schema.columns As c
                                                                  WHERE table_name = {table_literal}
                                                                  AND table_schema = {schema_literal}
                                                                  AND  c.column_name NOT IN ({columns_to_exclude},)
                        ), \',\') || \', ST_ASEWKT(geom) as geom\' As sqlstmt
                  ')
  
  columns <-  DBI::dbGetQuery(connection,q)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}


fetch_report_from_s3 = function(bucket=Sys.getenv("S3_BUCKET"), user_id, run_id) {
  tryCatch({
    print(glue::glue('Retrieving {user_id}_{run_id}_report.pdf from S3 bucket'))
    if (aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), bucket)) {
      #file <- aws.s3::get_object(glue::glue('{user_id}_{run_id}_report.pdf'), bucket)
      #return(file)
      # or save object to disc
      aws.s3::save_object(glue::glue('{user_id}_{run_id}_report.pdf'), bucket, file=glue::glue('{user_id}_{run_id}_report.pdf'))
    } else {
      print(glue::glue('{user_id}_{run_id}_report.pdf not found'))
      return(NULL)
    }
  }, error = function(cond){
    print('There was an error retreiving report.')
    print(cond)
  })
}
