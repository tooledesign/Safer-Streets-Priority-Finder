library('RPostgreSQL')
db <-DBI::dbConnect(DBI::dbDriver(drvName = "PostgreSQL"),
                    dbname=SSPF_AMAZON_DATABASE,
                    host=SSPF_AMAZON_HOST_ADDRESS, 
                    port=5432,
                    user = SSPF_AMAZON_USERNAME ,
                    password = SSPF_AMAZON_PASSWORD)

data <- read.csv('//Users//Bikingman//Documents//GitHub//Safer-Streets-Priority-Finder//general/update_pfm//exports//pfm_corrected_predvalyearly_20220113_js.csv')

dbWriteTable(db, c(glue::glue("static"),glue::glue("pfm_corrected_predvalyearly_20220113_js")),
             value = data,
             append = FALSE,
             row.names = FALSE,
             overwrite = TRUE)

