# This function is called as the user uploads their crash data. 
# Several functions are called to check the data for several unacceptable conditions.
# If any of those conditions are met, the function flags the problem, 
# Otherwise the data are unzipped and loaded for next steps (variable mapping)
preload_local_data_shp_crashes <- function(data, input, session, connection, user_id, run_id){
  tryCatch({
  #check if fileInput is null or not 
  if (is.null(input$crash_data_shp)) {
    shiny_warming_alert(title = 'No Data', 
                        text = 'No data selected.')
    
  # test for zipped file   
  } else if (!grepl("\\.zip$", input$crash_data_shp)) {
    shiny_warming_alert(title = 'Not a zipped file', 
                        text = 'Please upload a shapefile in a single zipped file. The zipped file must include the following file extensions: .shp, .shx, .dbf, and .prj.')
    reset("crash_data_shp")
  }  else {
    
    # process crashes, i.e. creates a spatial data frame 
    data$crashes <- process_zipped_shapefile(file = input$crash_data_shp)
    
    # check for created attributes 
    at_check <- attribute_check(data=data$crashes, attributes=c('fclass_mapped', paste0('in_sa_{user_id}_{run_id}'), paste0('tdg_id_{user_id}_{run_id}'), 'usdot_mode_mapped', 'crashes_costs_usdot', 'severity_mapped'))
    if (at_check) {
      shiny_warming_alert(title = 'Problem', text=glue::glue('Sorry, you cannot have any attibutes on your crash data named, \'fclass_mapped\', \'in_sa_{user_id}_{run_id}\', \'tdg_id_{user_id}_{run_id}\', \'usdot_mode_mapped\', \'crashes_costs_usdot\', or \'severity_mapped\'. Please rename the attributes on your local.'))
      
    } else {
      
      # check data type, stop and inform the user if the data are of the wrong type, continue if data are points
      if (unique(st_geometry_type(data$crashes))[1] != 'MULTIPOINT' && unique(st_geometry_type(data$crashes))[1] != 'POINT') {
        
        # let user know if spatial data are not correct geometry type 
        shiny_warming_alert(title = 'Wrong Data.', text='Your crash data are not points. Please upload your crashes as points.')
 
        reset("crash_data_shp")
      }  else {
        
        # convert data into general format
        data$crashes <- sf::st_cast(data$crashes, 'POINT')
   
        print(paste0('Total crashes: ', nrow(data$crashes)))
        
        # check for road length, largest allowed is equal to total length of osm roads in LA county, or 26709 miles 
        if (nrow(data$crashes) > 150000) {
          shiny_warming_alert(title = 'Can Not Process', text=glue::glue('Sorry, you can only upload up to 150,000 crashes.'), showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
        } else {
          # convert to 4326
          data$crashes <- transform_with_epsg(data$crashes, 4326)
          
          # check for crazy bounding box 
          bb <- sf::st_bbox(data$crashes) 
          data$crashes_bbox <- sf::st_bbox(data$crashes) %>% as.vector()
          
          if ( bb[1] < -180 || bb[1] > 180 || 
               bb[2] < -85  || bb[2] > 85 ||
               bb[3] < -180 || bb[3] > 180 ||                
               bb[4] < -85  || bb[4] > 85                
          )  {
            
            reset('crash_data_shp')
            shiny_warming_alert(title = 'Out of bounds.', 
                                text=paste0('We\'ve discovered an invalid value range from the bounding box of your data. We converted your data to SRID 4326 (WGS84 coordinate reference system), and here are the boundaries we found,  
                                                       Longitude (xmin: ', bb[1], ', xmax: ', bb[3], '), Latitude (ymin: ', bb[2], ', ymax: ', bb[4], '). After converting your data, the longitude values should sit between -180 and 180, and the latitude values should sit between -85 and 85. This problem can occur if the coordinate values for your geometries don\'t fit the same SRID. Please fix this issue before proceeding.'
                                ))
          } else {
            
            
            # test overlap with study area 
            in_study_area <- test_bbox_overlap(connection = connection, user_id = user_id, run_id = data$run_id_sql_formatted, test_shape = data$crashes) 
            
            
            # inform the user that everything loaded correctly 
            if ( !in_study_area )  {
              reset('crash_data_shp')
              shiny_warming_alert(title = 'Out of Bounds', text='From the looks of it, your crash dataset does not intersect with your study area. Please upload a study area that overlaps with your crash dataset.')
              
            } else {
              # enable functional classification selection table 
              updateSelectInput(session, "crash_sev_variable",
                                choices = names(as.data.frame(data$crashes))
              )
              
              updateSelectInput(session, "mode_variable",
                                choices = names(as.data.frame(data$crashes))
              )    
              
              updateSelectInput(session, "reportid_variable",
                                choices = names(as.data.frame(data$crashes))
              )    
              
              updateSelectInput(session, "year_variable",
                                choices = names(as.data.frame(data$crashes))
              )    
              
              shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").removeClass("leaflet_none");'))
              shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").addClass("leaflet_block");'))
              shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides1'), '").removeClass("leaflet_block");'))
              shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides1'), '").addClass("leaflet_none");'))
            }
          }
        }

      }
    }}     
  }, error = function(cond){
    return(FALSE)
  })
}

# This function is called as the user uploads their roads data. 
# Several functions are called to check the data for several unacceptable conditions.
# If any of those conditions are met, the function flags the problem, 
# Otherwise the data are unzipped and loaded for next steps (variable mapping)
preload_local_data_shp_roads <- function(data, input, session, connection, user_id, run_id){
  tryCatch({
  if (is.null(input$roads_shp)) {
    shiny_warming_alert(title = 'No Data', 
                        text = 'No data selected.')
  } else if (!grepl("\\.zip$", input$roads_shp)) {
    reset('roads_shp')
    shiny_warming_alert(title = 'Not a zipped file', 
                        text = 'Please upload a shapefile in a single zipped file. Please include the mandatory file extensions needed for a shapefile: .shp, .shx, .dbf, and .prj.')
  }
  else {
 
      #process unzipped shapefile, i.e. creates a spatial data frame 
      data$the_roads <- process_zipped_shapefile(file = input$roads_shp)
      # check for attributes 
      at_check <- attribute_check(data=data$the_roads, attributes=c('usdot_fun_class_mapped', paste0('tdg_id_{user_id}_{run_id}')))
      if (at_check) {
        shiny_warming_alert(title = 'Problem', text=glue::glue('Sorry, you cannot have any attibutes in your roads data named, \'usdot_fun_class_mapped\' or \'tdg_id_{user_id}_{run_id}\'. Please rename the attributes in your local data.'))
      } else {
        # checks the data type to ensure the user uploaded polylines for roads 
        if (unique(st_geometry_type(data$the_roads))[1] != 'LINESTRING' && unique(st_geometry_type(data$the_roads))[1] != 'MULTILINESTRING'){
          reset('roads_shp')
          waiter::waiter_hide()
          shiny_warming_alert(title = 'Wrong Data.', text='Your roads data are not linestrings or multilinestring. Please upload linestring or multilinestring roads data.')
        } else {
          
          # Cast roads into linestring
          data$the_roads <- sf::st_cast(data$the_roads, 'LINESTRING')

          utm <- get_account_crs(
            connection = connection, 
            user_id = user_id,  
            run_id = data$run_id_sql_formatted
          )
          length_roads <- transform_with_epsg(data$the_roads, as.integer(utm))
          length <- as.numeric(sum(st_length(length_roads))/1609.334) 
          length_roads <- NULL
          print(paste0('Total miles of roadway: ', length))
          # check for road length, largest allowed is equal to total length of osm roads in LA county, or 26709 miles 
          if (length > 26708.43) {
            shiny_warming_alert(title = 'Can Not Process', text=glue::glue('Sorry, the total length of your road network is too long. Please reduce the overall size of your network and try again.'), showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
            
            } else {
            
            # convert to 4326
            data$the_roads <- transform_with_epsg(data$the_roads, 4326)
            
            # check for crazy bounding box 
            bb <- sf::st_bbox(data$the_roads) 
            
            if ( bb[1] < -180 || bb[1] > 180 || 
                 bb[2] < -85  || bb[2] > 85 ||
                 bb[3] < -180 || bb[3] > 180 ||                
                 bb[4] < -85  || bb[4] > 85                
            )  {
              waiter::waiter_hide()
              reset('roads_shp')
              shiny_warming_alert(title = 'Out of bounds.', 
                                  text=paste0('We\'ve discovered an invalid value range from the bounding box of your data. We converted your data to SRID 4326 (WGS84 coordinate reference system), and here are the boundaries we found,  
                                                       Longitude (xmin: ', bb[1], ', xmax: ', bb[3], '), Latitude (ymin: ', bb[2], ', ymax: ', bb[4], '). After converting your data, the longitude values should sit between -180 and 180, and the latitude values should sit between -85 and 85. This problem can occur if the coordinate values for your geometries don\'t fit the same SRID. Please fix this issue before proceeding.'
                                  ))
            } else {
              # test if there's overlap with study area, show an alert if not, go to next screen if all good. 
              in_study_area <- test_bbox_overlap(connection = connection, user_id = user_id, run_id = data$run_id_sql_formatted, test_shape = data$the_roads) 
              
              if ( !in_study_area ) {
                reset('roads_shp')
                # hide waiter 
                waiter::waiter_hide()
                shiny_warming_alert(title = 'Out of Bounds.', text='From the looks of it, your roads dataset does not intersect with your study area. Please upload a study area that overlaps with your roads dataset or upload a new roads dataset.')
              }  else {
                # get bbox as vector for map extents
                data$roads_bbox <- sf::st_bbox(data$the_roads)  %>% as.vector()
                
                # enable functional classification variable selection  
                updateSelectInput(session, "func_class_variable",
                                  choices = names(as.data.frame(data$the_roads))
                )
                updateSelectInput(session, "road_name",
                                  choices = names(as.data.frame(data$the_roads))
                )
                updateSelectInput(session, "pri_key_roads",
                                  choices = names(as.data.frame(data$the_roads))
                )
                shiny_warming_alert(title='Roads Loaded', text="Great, your roads data have been uploaded. Let's continue.", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
                shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides1'), '").removeClass("leaflet_block");'))
                shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides1'), '").addClass("leaflet_none");'))
                shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").removeClass("leaflet_none");'))
                shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").addClass("leaflet_block");'))
                # shinyjs::runjs(code = paste0('$("#', session$ns('roads_map'), '").trigger("shown");'))
                
          }
        }
      }}
  }
    
  }}, error = function(cond){
 
    c <- toString(cond)
    shiny_warming_alert(title = 'Something Went Wrong.', text=c)
  })
}