# testing bbox function 
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
connection <- create_postgresql_connection( dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
                                            host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
                                            user = Sys.getenv("SSPF_AMAZON_USERNAME"),
                                            password = Sys.getenv("SSPF_AMAZON_PASSWORD"))


library(rgdal)
library(sf)
library(RCurl)
library(rgeos)
library(ggplot2)
library(leaflet)
getwd()
shape <- readOGR(dsn = "mock_data/roads.shp")
points <- readOGR(dsn = "mock_data/crashes.shp")
plot(shape)
plot(points)

user_id <- "1614356312"
run_id <- 'f'
run_id <- DBI::dbQuoteLiteral(connection, run_id)
shape_bbox <- sf::st_bbox(shape)
shape_bbox
shape_bbox[1]
shape_bbox[2]
shape_bbox <- sf::st_bbox(points)
shape_bbox
shape_bbox[1]
shape_bbox[2]

update_bbox_udpates <- function ( 
  connection, 
  user_id, 
  run_id,
  shape
) {
  shape_bbox <- sf::st_bbox(shape)
  query <- glue::glue("UPDATE app_testing.accounts 
                       SET    sa_bbox_south_4326 = {shape_bbox[2]},
                              sa_bbox_north_4326 = {shape_bbox[4]},
                              sa_bbox_east_4326  = {shape_bbox[3]},
                              sa_bbox_west_4326  = {shape_bbox[1]}
                       WHERE  user_id = {user_id}
                         AND  run_id  = {run_id}
                       ;")
  DBI::dbGetQuery(connection, query)
  print("Done updating account bounding box notes.")
}

update_bbox_udpates(connection=connection, user_id=user_id, run_id=run_id, shape=shape)

query <- glue::glue("SELECT  sa_bbox_south_4326, sa_bbox_north_4326, sa_bbox_east_4326, sa_bbox_west_4326 
                        FROM   app_testing.accounts 
                       WHERE   user_id = {user_id}
                         AND   run_id  = {run_id}
                         ;"
)
account_bbox <- DBI::dbGetQuery(connection, query)
account_bbox 
account_bbox$sa_bbox_south_4326[1]


test_bbox_overlap <- function ( 
  connection, 
  user_id, 
  run_id,
  test_shape # shape to test against user's account
) {
  shape_bbox <- sf::st_bbox(test_shape)
  query <- glue::glue("SELECT  sa_bbox_south_4326, sa_bbox_north_4326, sa_bbox_east_4326, sa_bbox_west_4326 
                        FROM   app_testing.accounts 
                       WHERE   user_id = {user_id}
                         AND   run_id  = {run_id}
                         ;"
                      )
  account_bbox <- DBI::dbGetQuery(connection, query)
  
  if (!is.null(account_bbox)){
    test_shape_bbox <- sf::st_bbox(test_shape)
    
    if (abs(account_bbox$sa_bbox_south_4326[1]) <= abs(shape_bbox[4]) && abs(account_bbox$sa_bbox_north_4326[1]) >= abs(shape_bbox[2])) {
      north_south <- TRUE
      } else {
        north_south <- FALSE
        }
    if (abs(account_bbox$sa_bbox_east_4326[1]) <= abs(shape_bbox[1]) && abs(account_bbox$sa_bbox_west_4326[1]) >= abs(shape_bbox[3])) {
      east_west <- TRUE
      } else {
        east_west <- FALSE
      }
    if ( north_south && east_west ) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}



dat_orig <- read.table(text="    a    b    c    d    e    lat   lng
                                 12   f2   23   dd   2d   15.6  80.9
                                 12   g5   99   NA   hh   20.9  10.9
                                 13   g4   12   aa   3r3  1.2   81.8", header=TRUE, stringsAsFactors=FALSE)


dat_2 <- SpatialPointsDataFrame(dat_orig[,c("lng", "lat")], dat_orig[,1:5])
leaflet(dat_2) %>% addCircleMarkers() %>% addTiles()

test <- test_bbox_overlap(connection=connection, user_id=user_id, run_id=run_id, test_shape=dat_2)
test

test <- test_bbox_overlap(connection=connection, user_id=user_id, run_id=run_id, test_shape=shape)
test

## test get max bbox function 
get_max_bounds <- function(list_of_objects){
  bbox <- c(xmin=NULL, xmin=NULL, ymax=NULL, ymax=NULL)
  for (i in list_of_objects){
    for (k in 1:4){
      if ( is.null(bbox[k]) || is.na(bbox[k]) ) {
        bbox[k] <- i[k] 
      } 
      else {
        if ( k == 1 || k == 2){
          if (bbox[k] > i[k]){
            bbox[k] <- i[k]
          }
        } else {
          if (bbox[k] < i[k]){
            bbox[k] <- i[k]
          }
        }
      }
    }
  }
  return(bbox)
}

c <- list()
c[[1]] <- sf::st_bbox(shape)
c[[2]] <- sf::st_bbox(points)
 
get_max_bounds(c)

