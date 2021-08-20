con <- pool::dbPool(
  drv =  RPostgreSQL::PostgreSQL(),
    dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
    host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
    user = Sys.getenv("SSPF_AMAZON_USERNAME"),
    password = Sys.getenv("SSPF_AMAZON_PASSWORD")
)

getwd()
states  <-  DBI::dbGetQuery(con, 'SELECT DISTINCT c.statefp, c.countyfp 
	                                  FROM static.us_county_2018 c;') 

miles <- data.frame()
for (i in states[,1]) {
  counties <- states[ which(states$statefp==glue::glue(i)),]
  for ( j in counties[,2]) {
    q <- glue::glue("SELECT state_fp, county_fp, SUM(ST_LENGTH(geom)) as length_degrees FROM static.osm_centerlines WHERE county_fp = \'{j}\' AND state_fp = \'{i}\' GROUP BY county_fp, state_fp")
    co  <-  DBI::dbGetQuery(con, q)
    print(co)
    miles <<- rbind(miles, co)
  }
}

write.csv(miles, 'county_mileage.csv')

q <- "SELECT state_fp, 
             county_fp, 
             SUM(ST_LENGTH(ST_TRANSFORM(geom, 6423)))/1609.334 as length_miles 
      FROM static.osm_centerlines 
      WHERE county_fp = \'037\' 
      AND state_fp = \'06\' 
      GROUP BY county_fp, state_fp" 

la  <-  DBI::dbGetQuery(con, q)

write.csv(la, 'la_mileage.csv')
