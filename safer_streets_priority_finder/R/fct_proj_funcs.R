# returns UTM code from table in WGS84 format 
get_utm_code <- function(lng, lat){
  tryCatch({
  utm_band = toString((floor((lng + 180 ) / 6 ) %% 60) + 1)
  if (nchar(utm_band) == 1) {
    utm_band <- paste0('0', utm_band)
  } 
  if (lat >= 0 ) { 
    epsg_code <- paste0('326', utm_band)
  } else {
    epsg_code <- paste0('327', utm_band)
  }
  return(epsg_code)
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# locally transforms sf table 
transform_with_epsg <- function(spatial_data, epsg){
  transformed <- sf::st_transform(spatial_data, epsg)  
  return(transformed)
}

# returns medroid (like centroid, but instead returns the median of all lay/longs)
get_medroid <- function(spatial_data){
  tryCatch({
  coors <- as.data.frame(sf::st_coordinates(spatial_data)[,1:2])
  median <- data.frame(x=median(coors$X), y=median(coors$Y))
  c <- sf::st_as_sf(median, coords = c('x','y'), remove = FALSE)
  return(c)
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# returns CRS of spatial table 
get_crs <-function ( 
  connection, 
  user_id, 
  run_id,
  table,
  geom='geom',
  schema='local_user_data'
) {
  tryCatch({
  crs <- NULL
  g_q <- glue::glue("
                    SELECT DISTINCT ST_SRID({geom}) 
                    FROM {schema}.{table} WHERE {geom} IS NOT NULL LIMIT 1;
                    ")
  crs <- toString(DBI::dbGetQuery(connection, g_q))
  if(length(crs) > 0){
    return(crs) 
  } else {
    return(FALSE)
  }
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
  }

# returns CRS recorded on accounts table 
get_account_crs <- function (
  connection, 
  user_id=NULL, 
  run_id=NULL
){
  tryCatch({
  crs <- NULL
  q <- glue::glue("
                    SELECT DISTINCT crs 
                    FROM gen_management.accounts 
                    WHERE run_id = {run_id} 
                    AND user_id = {user_id};
                    ")
  crs <- toString(DBI::dbGetQuery(connection, q))
  return(crs) 
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# checks for CRS on account table 
check_crs_of_tables <- function ( 
  connection, 
  user_id, 
  run_id,
  list_of_tables, 
  schema='local_user_data'
) {
  tryCatch({
  c <- c()
  for (i in list_of_tables){
    crs <- get_crs(connection=connection, user_id=user_id, run_id=run_id, table=i)
    if (crs != FALSE && !is.na(crs) && !is.null(crs) && crs != '' ) {
      c <- append(c, crs)
    } else {
      c <- append(c, -999)
    }
  } 
 
  if (var(c) == 0){
    return(TRUE)
  } else {
     return(FALSE)
  }
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}
