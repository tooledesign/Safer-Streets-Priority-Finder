 
connection <- create_postgresql_connection( dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
                                            host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
                                            user = Sys.getenv("SSPF_AMAZON_USERNAME"),
                                            password = Sys.getenv("SSPF_AMAZON_PASSWORD"))
fips <- read.csv("county_state_fips.csv", header=T, sep=',')
fips$statefp <- sprintf("%02d", fips$statefp)
state_fips <- unique(fips$statefp)
for (i in state_fips[1:5]) {
  print(i)
}
s <- NULL
sa <- NULL 



# test
 
for (i in state_fips[41:53]) {
  s <- DBI::dbQuoteString(connection, i)
  sa <- as.integer(i)
  q <- glue::glue("
        DROP TABLE IF EXISTS tiger.tl_2019_all_roads_{sa};
        SELECT linearid,
               fullname, 
               state_fp, 
               county_fp, 
               mtfcc,
               NULL::TEXT as geom_wkt,
               (ST_Dump(geometry)).geom as geom
        INTO tiger.tl_2019_all_roads_{sa}
        FROM received.tl_2019_all_roads
        WHERE state_fp = {s} 
        --AND mtfcc NOT IN ('S1740', 'S1500', 'S1750', 'S1710', 'S1640', 'S1780', 'S1830', 'S1820', 'S1720')
        ; 

        ALTER TABLE tiger.tl_2019_all_roads_{sa} 
        ALTER COLUMN geom TYPE geometry(LineString, 4326)
        USING ST_Transform(geom, 4326);

        UPDATE tiger.tl_2019_all_roads_{sa}
        SET geom_wkt = ST_ASEWKT(geom);
        
        ALTER TABLE tiger.tl_2019_all_roads_{sa}
        ADD COLUMN tdg_id SERIAL PRIMARY KEY;

         ALTER TABLE tiger.tl_2019_all_roads_{sa}
         ADD COLUMN usdot_fun_class_mapped VARCHAR (60);
  
         UPDATE tiger.tl_2019_all_roads_{sa}
         SET usdot_fun_class_mapped = (SELECT CASE
         WHEN mtfcc = \'S1400\' THEN \'Local Road\'
         WHEN mtfcc = \'S1730\' THEN \'Local Road\'
         WHEN mtfcc = \'S1100\'  THEN \'Major Arterial\'
         WHEN mtfcc = \'S1630\'  THEN \'Major Arterial\'
         WHEN mtfcc = \'S1200\'  THEN \'Major Collector\'
         ELSE \'mtfcc\'
         END);

        CREATE INDEX county_fp_indx_{sa} ON tiger.tl_2019_all_roads_{sa} (county_fp) ;
        ANALYZE tiger.tl_2019_all_roads_{sa};
                ")
  DBI::dbExecute(connection, q)
}

# build
for (i in state_fips[16:30]) {
  s <- DBI::dbQuoteString(connection, i)
  sa <- as.integer(i)
  DBI::dbGetQuery(connection, q)
}

# 
# ### fars 
# f <- glue::glue("
# 
#                 DROP TABLE IF EXISTS fars.fars_processed_{sa};
#                 SELECT pkey,
#                 st_case, 
#                 crash_mode, 
#                 crash_severity, 
#                 crash_year,
#                 state_fp,
#                 county_fp,
#                 place_fp,
#                 functional_class,
#                 ST_AsEWKT((ST_Dump(geom)).geom) as geom_wkt
#                 INTO fars.fars_processed_{sa}
#                 FROM automated.fars_processed
#                 WHERE state_fp = {s}; 
#                 
#                 ALTER TABLE fars.fars_processed_{sa}
#                 ADD PRIMARY KEY (pkey);
#                 CREATE INDEX county_fp_indx ON fars.fars_processed_{sa} (county_fp) ;
#                 ANALYZE fars.fars_processed_{sa};
#                 CREATE INDEX crash_mode_indx ON fars.fars_processed_{sa} (crash_mode) ;
#                 ANALYZE fars.fars_processed_{sa};
#                 "
# )
# 
# # test
# for (i in fips$statefp[1]) {
#   s <- DBI::dbQuoteString(connection, i)
#   print(sa)
#   print(s)
#   print(q)
#   DBI::dbExecute(connection, f)
# }
# 
# # build
# for (i in fips$statefp[1]) {
#   s <- DBI::dbQuoteString(connection, i)
#   DBI::dbExecute(connection, f)
# }
# session$onSessionEnded(function(){
#   print('Returned pool connection')
#   pool::poolReturn(connection)
# })
