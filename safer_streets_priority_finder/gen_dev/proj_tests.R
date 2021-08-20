
library(rgdal)
library(sf)
library(RCurl)
library(rgeos)
library(ggplot2)
shape <- readOGR(dsn = "mock_data/limited_roads_wgs84.shp")
points <- readOGR(dsn = "mock_data/bike_ped_crashes_dc.shp")
poly <- readOGR(dsn = "mock_data/test_study_area.shp")
leaflet(shape) %>% addPolylines()

get_utm_code <- function(lng, lat){
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
}

centroid <- gCentroid(shape)
epsg <- get_utm_code(lng=centroid@coords[1], lat=centroid@coords[2])

get_proj4string <- function(epsg){
  url <- RCurl::getURL(sprintf('https://spatialreference.org/ref/epsg/%s/proj4/', epsg))
  return(url)
}

p <- get_proj4string(epsg)

transform_with_epsg <- function(spatial_data, epsg){
  proj4string <- get_proj4string(epsg)
  wgs_data <- sp::spTransform(spatial_data, CRS(proj4string)) # To convert it to WGS84
  return(wgs_data)
}

updated_shape <- transform_with_epsg(shape, 32615)
shape <- transform_with_epsg(shape, 4326)
plot(updated_shape) 
plot(shape)

# test data 
con <<- create_postgresql_connection(dbname = "a0137", host = "192.168.60.227", port = 5432, user = "gis", password = "gis")


rpostgis::pgInsert(conn = con, name = c("app_testing", 'test_roads_utm'), data.obj = updated_shape, geom = "geom",
                   df.mode = FALSE, partial.match = FALSE, overwrite = TRUE, new.id = NULL, row.names = FALSE,
                   upsert.using = NULL, alter.names = FALSE, encoding = NULL, return.pgi = FALSE, df.geom = NULL, geog = FALSE)

rpostgis::pgInsert(conn = con, name = c("app_testing", 'test_roads_wgs'), data.obj = shape, geom = "geom",
                   df.mode = FALSE, partial.match = FALSE, overwrite = TRUE, new.id = NULL, row.names = FALSE,
                   upsert.using = NULL, alter.names = FALSE, encoding = NULL, return.pgi = FALSE, df.geom = NULL, geog = FALSE)

as.character(DBI::dbGetQuery(con, 'SELECT DISTINCT ST_SRID(geom) as srid FROM app_testing.test_roads_wgs;'))  
as.character(DBI::dbGetQuery(con, 'SELECT DISTINCT ST_SRID(geom) as srid FROM app_testing.test_roads_utm;'))  
shape_from_psql <- rpostgis::pgGetGeom(con, c("app_testing", 'test_roads_wgs'))
plot(shape_from_psql)
updated_shape_from_psql <- rpostgis::pgGetGeom(con, c("app_testing", 'test_roads_utm'))
plot(updated_shape_from_psql)

#ok, let's get metriod 
get_medroid <- function(spatial_data){
  coors <- as.data.frame(do.call(rbind, lapply(coordinates(spatial_data), as.data.frame)))
  c <- SpatialPoints(cbind(lon = median(coors[,1]), lat = median(coors[,2])), proj4string=spatial_data@proj4string)
  c <- SpatialPointsDataFrame(c, data.frame(ID=1:length(c)))
  return(c)
}

wgs_medriod <- get_medroid(shape)
plot(wgs_medriod)

# plot for comparison
ggplot() + 
  geom_path(data = shape_from_psql, aes(x = long, y = lat, group=group)) + 
  geom_point(data = as.data.frame(wgs_medriod), aes(x = lon, y = lat))

# cool

# create function to handle points or lines in medriod function 
wgs_medriod <- get_medroid(shape)
wgs_medriod <- get_medroid(points)

# lines 
coors <- as.data.frame(do.call(rbind, lapply(sp::coordinates(shape), as.data.frame)))
# points 
coorsp <- as.data.frame(sp::coordinates(points)[,1:2])
# test if lines or points 
class(shape)[1]
class(points)[1]



get_coordinates_table <- function(spatial_data){
  if (class(spatial_data)[1] == "SpatialPointsDataFrame") {
    return(as.data.frame(sp::coordinates(spatial_data)[,1:2]))
  } else if (class(spatial_data)[1] == "SpatialPolygonsDataFrame") {
    return(as.data.frame(sp::coordinates(spatial_data)[,1:2]))
  } else {
    return(as.data.frame(do.call(rbind, lapply(sp::coordinates(spatial_data), as.data.frame))))
  }
}

get_medroid <- function(spatial_data){
  coors <- get_coordinates_table(spatial_data)
  c <- sp::SpatialPoints(cbind(lon = median(coors[,1]), lat = median(coors[,2])), proj4string=spatial_data@proj4string)
  c <- sp::SpatialPointsDataFrame(c, data.frame(ID=1:length(c)))
  return(c)
}

p_medriod <- get_medroid(points)
l_medriod <- get_medroid(shape)
plot(p_medriod)
plot(l_medriod)

 

