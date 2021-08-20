create_postgresql_connection <- function(dbname=NULL, host=NULL, port=5432, user=NULL, password=NULL){
  drv <- RPostgreSQL::PostgreSQL()
  con <- DBI::dbConnect(drv, 
                        dbname   = dbname,
                        host     = host,
                        port     = port,
                        user     = user,
                        password = password
  )
  return(con)
}
# con <- create_postgresql_connection(dbname = "a0137", host = "192.168.60.227", port = 5432, user = "gis", password = "gis")
con <- create_postgresql_connection(  dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
                                      host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
                                      user = Sys.getenv("SSPF_AMAZON_USERNAME"),
                                      password = Sys.getenv("SSPF_AMAZON_PASSWORD"))
q  <- glue::glue("DROP TABLE IF EXISTS gen_management.accounts;
                  CREATE TABLE gen_management.accounts (
                  tdg_id                  BIGSERIAL PRIMARY KEY,
                	user_id                 INTEGER NOT NULL,
                	username                TEXT NOT NULL,
                	email                   TEXT NOT NULL,
                	run_id                  TEXT NOT NULL,
                	crs                     INTEGER NULL,
                	crash_o_year_col        TEXT NULL, 
                	crash_o_serv_col        TEXT NULL, 
                	crash_o_rep_id_col      TEXT NULL,
                	crash_o_mode_col        TEXT NULL,
                	roads_o_fun_c_col       TEXT NULL, 
                	road_o_name             TEXT NULL,
                	road_o_source           TEXT NULL,
                	road_o_id               TEXT NULL,
                	roads_fun_c_col         TEXT NULL,
                	crash_o_source          TEXT NULL,
                	sa_o_source             TEXT NULL,
                  move_windows_long_comp  BOOLEAN DEFAULT FALSE,	
                  model_comp              BOOLEAN DEFAULT FALSE,
                  sa_bbox_north_4326      FLOAT,
                  sa_bbox_south_4326      FLOAT,
                  sa_bbox_east_4326       FLOAT,
                  sa_bbox_west_4326       FLOAT,
                  time_since_model_desired TIMESTAMP DEFAULT NULL,
                  time_mode_finished TIMESTAMP DEFAULT NULL,
                  model_process_time INTERVAL DEFAULT NULL,             
                  model_status TEXT DEFAULT 'no_model_desired',
                  sa_storage_opt_out BOOLEAN DEFAULT FALSE,
                  roads_storage_opt_out BOOLEAN DEFAULT FALSE,
                  crash_storage_opt_out BOOLEAN DEFAULT FALSE,
                  sa_data_last_added TIMESTAMP DEFAULT NULL,
                  roads_data_last_added TIMESTAMP DEFAULT NULL,
                  crash_data_last_added TIMESTAMP DEFAULT NULL,
                  last_login TIMESTAMP DEFAULT NOW(),
                  account_created TIMESTAMP DEFAULT NOW(),
                  total_logins INTEGER DEFAULT 1
                  );
                 ")
DBI::dbGetQuery(con,q)
q <- 'SELECT * FROM gen_management.accounts ;'
rs <-  DBI::dbGetQuery(con,q)
df <-  DBI::fetch(rs,n=-1)
print(df)
DBI::dbDisconnect(con)

