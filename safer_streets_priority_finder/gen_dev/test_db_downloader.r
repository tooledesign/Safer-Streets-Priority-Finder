 create_postgresql_connection <- function(dbname=NULL, host=NULL, port=5432, user=NULL, password=NULL){
  # creates a postgresql connection depends on RPostgreSQL and DBI libraries 
  # dbname   = database to connect to 
  # host     = IP address of PostgreSQL database you'd like a connection to 
  # port     = default 5432
  # user     = UN to access DB 
  # password = UN password to use to connect to DB

    # set driver 
    drv <- RPostgreSQL::PostgreSQL()
    
    # create PostgreSQL connection 
    con <- DBI::dbConnect(drv, 
                          dbname   = dbname,
                          host     = host,
                          port     = port,
                          user     = user,
                          password = password
    )
  #return connection object
  return(con)
}

fetch_spatial_table <- function(
  columns=NULL, # list of column name to be selected. 
  schema=NULL,  # schema of table 
  table=NULL,  # table name 
  clauses=NULL   # where clause 
){
  if (length(clauses) == 0){
      clauses = 'WHERE TRUE'
  } 
  con <- create_postgresql_connection(dbname = "a0137", host = "192.168.60.227", port = 5432, user = "gis", password = "gis")
  q  <- glue::glue("SELECT {columns} FROM {schema}.{table} {clauses};")
  print(q)
  rs <-  DBI::dbGetQuery(con,q)
  df <-  DBI::fetch(rs,n=-1)
  print(df)
  
  DBI::dbDisconnect(con)
  return(df)
}
s <- '30'
c <- '017'

f <- fetch_spatial_table(columns='fullname, mtfcc, ST_AsEWKT(ST_UNION(geometry)) as geom', 
                         schema = 'received',
                         table =  'tl_2019_all_roads',
                         clauses = glue::glue('WHERE state_fp = \'{s}\' AND county_fp = \'{c}\' GROUP BY fullname, mtfcc'))


tmap_mode("view")
mapview::mapview(b)