connection <- pool::dbPool(
  drv =  RPostgreSQL::PostgreSQL(),
      dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
      host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
      user = Sys.getenv("SSPF_AMAZON_USERNAME"),
      password = Sys.getenv("SSPF_AMAZON_PASSWORD")
)

getwd()
lines <- rgdal::readOGR(dsn = "mock_data/roads.shp")
points <- rgdal::readOGR(dsn = "mock_data/crashes.shp")
polygon_multi <- rgdal::readOGR(dsn = "mock_data/sa_multi.shp")
polygon_sing <- rgdal::readOGR(dsn = "mock_data/study_area.shp")

print(points)
class(points)

d <- sf::st_as_sf(lines)
d$geometry <-  sf::st_as_text(d$geometry)
d <- as.data.frame(d)

d2 <- sf::st_as_sf(points)
d2$geometry <-  sf::st_as_text(d2$geometry)
d2 <- as.data.frame(d2)

d3 <- sf::st_as_sf(polygon_sing)
d3$geometry <-  sf::st_as_text(d3$geometry)
d3 <- as.data.frame(d3)

d4 <- sf::st_as_sf(polygon_multi)
d4$geometry <-  sf::st_as_text(d4$geometry)
d4 <- as.data.frame(d4)

 
DBI::dbExecute(connection, "SET search_path TO automated")

# LINES as WKT test 
round(as.numeric(object.size(d))/1000000, 3)
# ~ 2.05 Mbs

system.time(DBI::dbWriteTable(connection,
                              "d1_test",
                              d, 
                              row.names=FALSE,
                              overwrite=TRUE,
                              append=FALSE,
                              temporary=FALSE))
# user  system elapsed 
# 0.028   0.006   3.449 
# user  system elapsed 
# 0.033   0.006   3.020 
# user  system elapsed 
# 0.032   0.007   2.306 

# ~ 3 seconds for ~2 MBs

round(as.numeric(object.size(d2))/1000000, 3)
# ~ 4.2 Mbs
system.time(DBI::dbWriteTable(connection,
                              "d2_test",
                              d2, 
                              row.names=FALSE,
                              overwrite=TRUE,
                              append=FALSE,
                              temporary=FALSE))
# user  system elapsed 
# 0.064   0.015   4.777 
# user  system elapsed 
# 0.069   0.015   5.023
# user  system elapsed 
# 0.071   0.014   5.111 

# ~ 5.5 seconds for ~13 MBs
round(as.numeric(object.size(d4))/1000000, 3)
# ~ 0.024 Mbs
system.time(DBI::dbWriteTable(connection,
              "d4_test",
             d4, 
             row.names=FALSE,
             overwrite=TRUE,
             append=FALSE,
             temporary=FALSE))

# user  system elapsed 
# 0.011   0.001   0.636 
# user  system elapsed 
# 0.023   0.003   0.954
# user  system elapsed 
# 0.017   0.001   0.990 

#~ 1 second for < 1 Mb

d <- sf::st_as_sf(lines)
d$geometry <- sf::st_as_binary(d$geometry, EWKB=T)
d <- as.data.frame(d)

d2 <- sf::st_as_sf(points)
d2$geometry <- sf::st_as_binary(d2$geometry, EWKB=T)
d2 <- as.data.frame(d2)

d3 <- sf::st_as_sf(polygon_sing)
d3$geometry <- sf::st_as_binary(d3$geometry, EWKB=T)
d3 <- as.data.frame(d3)

d4 <- sf::st_as_sf(polygon_multi)
d4$geometry <- sf::st_as_binary(d4$geometry, EWKB=T)
d4 <- as.data.frame(d4)


# LINES as WKT test 
round(as.numeric(object.size(d))/1000000, 3)
# ~ 2 Mbs
system.time(DBI::dbWriteTable(connection,
                              "d1_test_2",
                              d, 
                              row.names=FALSE,
                              overwrite=TRUE,
                              append=FALSE,
                              temporary=FALSE))
# user  system elapsed 
# 0.130   0.014   4.430 
# user  system elapsed 
# 0.132   0.014   5.526 
# user  system elapsed 
# 0.131   0.013   3.916 

# ~ 5 seconds for ~2MBs

round(as.numeric(object.size(d2))/1000000, 3)
# ~ 5 Mbs
system.time(DBI::dbWriteTable(connection,
                              "d2_test_2",
                              d2, 
                              row.names=FALSE,
                              overwrite=TRUE,
                              append=FALSE,
                              temporary=FALSE))
# user  system elapsed 
# 0.255   0.032   8.692 
# user  system elapsed 
# 0.260   0.029  10.306 
# user  system elapsed 
# 0.257   0.030   8.703 

# ~ 9 seconds for ~5 MBs
round(as.numeric(object.size(d4))/1000000, 3)
# ~ 0.02 Mbs
system.time(DBI::dbWriteTable(connection,
                              "d4_test_2",
                              d4, 
                              row.names=FALSE,
                              overwrite=TRUE,
                              append=FALSE,
                              temporary=FALSE))
# user  system elapsed 
# 0.024   0.001   1.054 
# user  system elapsed 
# 0.022   0.001   0.904 
# user  system elapsed 
# 0.025   0.001   1.163 

# ~ 1 second for .02 Mbs 
DBI::dbExecute(connection, "SET search_path TO automated")

q <- 'ALTER TABLE automated.d1_test
      ADD COLUMN geom geometry (LineString, 4326);
      
      UPDATE automated.d1_test 
      SET geom = ST_GeometryFromText(geometry, 4326);
      '
DBI::dbExecute(connection, q)



sf_to_postgres_as_wkt <- function (connection, 
                                   schema, 
                                   table, 
                                   geodata
) {
  geodata <- sf::st_as_sf(geodata)
  geodata$geometry <- sf::st_as_text(geodata$geometry, EWKB=T)
  geodata <- as.data.frame(geodata)
  DBI::dbExecute(connection, glue::glue("SET search_path TO {schema}"))
  DBI::dbWriteTable(connection,
                    table,
                    geodata, 
                    row.names=FALSE,
                    overwrite=TRUE,
                    append=FALSE,
                    temporary=FALSE
  )
  DBI::dbExecute(connection, glue::glue("SET search_path TO public"))
  print('Done with sf_to_postgres_as_wkt()')
}
 
sf_to_postgres_as_wkt(connection=connection, schema='automated', table='d2_test', geodata=points)

create_geom_from_wkt <- function(connection, 
                          schema, # name of target table schema 
                          table,  # name not target table 
                          type,   # type of geometry to create (e.g., LINESTRING, MULTILINESTRING etc) 
                          srid    # srid of geometry to create (e.g., 4326)
                          ) {
  q <- glue::glue('ALTER TABLE {schema}.{table}
                   ADD COLUMN geom geometry ({type}, {srid});
                  
                   UPDATE {schema}.{table} 
                   SET geom = ST_GeometryFromText(geometry, {srid});
                   ')

  DBI::dbGetQuery(connection, q)
  print('Done with create_geom_from_wkt()')
}

create_geom(connection=connection, schema='automated', table='d2_test', type="POINT", srid=4326)
create_geom(connection=connection, schema='automated', table='d4_test', type="MULTIPOLYGON", srid=4326)