

# create_postgresql_connection function from fct_db_utlis.R
con <- create_postgresql_connection(dbname = "a0137", host = "192.168.60.227", port = 5432, user = "gis", password = "gis")

counties  <- DBI::dbGetQuery(con, 'SELECT c.ogc_fid, c.statefp, c.countyfp, c.name as county_name, c.namelsad as county_namelsad, s.name as state_name 
	FROM received.us_county_2018 c
	LEFT JOIN received.us_state_2018 s ON (c.statefp = s.statefp) ;', stringsAsFactors = FALSE,
                             as.is = c(TRUE, FALSE))  

write.csv(counties, 'data/county_state_fips.csv') 
counties <- read.csv('data/county_state_fips.csv') 
counties$statefp <- sprintf("%03d", counties$statefp)
counties$countyfp <- sprintf("%03d", counties$countyfp)
write.csv(counties, 'data/county_state_fips.csv') 
counties <- read.csv('data/county_state_fips.csv') 
# ugh, why   