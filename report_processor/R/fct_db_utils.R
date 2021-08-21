
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
      
      # define clauses
      if (is.null(clauses) || is.na(clauses) ){
        clauses = 'WHERE TRUE'
      } 
    
      # define columns 
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
    
      # cleanse schema and table names 
      schema <- DBI::dbQuoteIdentifier(connection, schema)
      table <- DBI::dbQuoteIdentifier(connection, table)  
  
      #glue query 
      q  <- glue::glue("SELECT {columns} FROM {schema}.{table} {clauses};")

      # send query 
      spatial_data <-  DBI::dbGetQuery(connection,q)

      spatial_data <- sf::st_as_sf(spatial_data, wkt='geom')
  
      # cast geom type if identified 
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
      print("There was an error with, fetch_spatial_table()")
      print(cond)
    })
  }
   
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
    print("There was an error with, return_table_name()")
    print(cond)
  })
}

 
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
   print("There was an error with, get_crosswalk_table_points()")
   print(cond)
 })
}

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
    print("There was an error with, get_crosswalk_table_lines()")
    print(cond)
  })
}

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
    print("There was an error with, get_user_account_value()")
    print(cond)
  })
}
 
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
    print("There was an error with, get_columns()")
    print(cond)
  })
}