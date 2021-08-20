

library(rgdal)
library(sf)
library(RCurl)
library(rgeos)
library(ggplot2)
shape <- readOGR(dsn = "mock_data/limited_roads_wgs84.shp")
points <- readOGR(dsn = "mock_data/bike_ped_crashes_dc.shp")

table <- as.data.frame(unique(na.omit(as.data.frame(shape)['ROADCLASS']))[, 1])
for (i in 1:nrow(table)) {
  table[['Standard Functional Class Value']][i] <- runif(1, 5.0, 7.5)
}
names(table)[1] <-  "Your Dataset's Functional Class Value"
# grab data 
shape@data$ROADCLASS 

# grab data based on value 
shape@data$ROADCLASS[shape@data$ROADCLASS ==  'Local']

# grab data based on index value from another table 
shape@data$ROADCLASS[shape@data$ROADCLASS ==  table[1][2,]] 

# try with double braces
shape@data[['ROADCLASS']][shape@data[['ROADCLASS']] == 'Local']

# test seelction with double braces and indexed table 
shape@data[['ROADCLASS']][shape@data[['ROADCLASS']] ==  table[1,1]]
 
#updated values with indexed table 
shape@data[['usdot_fun_class_mapped']][shape@data[['ROADCLASS']] ==  table[1,1]] <- table[1,2]

# put it all together  
for (i in 1:nrow(table)) {
  shape@data[['usdot_fun_class_mapped']][shape@data[['ROADCLASS']] == table[i,1]] <- table[i,2]
}
#qc
as.data.frame(subset(as.data.frame(shape@data), select = c('ROADCLASS', 'usdot_fun_class_mapped')))

# check nulls/nas 
unique(shape@data$ROADCLASS)

# are NAs filled in? 
as.data.frame(subset(as.data.frame(shape@data), is.na(shape@data$ROADCLASS), select = c('ROADCLASS', 'usdot_fun_class_mapped')))
#no