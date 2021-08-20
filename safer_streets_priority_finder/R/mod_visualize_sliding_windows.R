#' visualize_sliding_windows UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' 

mod_visualize_sliding_windows_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}
    
#' visualize_sliding_windows Server Functions
#' generalized method for visualizing all available sliding windows analyses by mode 
#' @noRd 
mod_visualize_sliding_windows_server <- function(input, output, session, connection, user_id, run_id, data=NULL, leaflet_proxy, n){
    ns <- session$ns
    data <- reactiveValues()
 
    

    data$hin_network <- fetch_spatial_table(connection = connection,
                                            columns= 'id,
                                                      road_name, 
                                                      road_fclass as usdot_fun_class_mapped, 
                                                      bike_score as bicycle_crash_score, 
                                                      ped_score as pedestrian_crash_score,
                                                      other_score,
                                                      ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                            schema = 'sliding_windows_outputs',
                                            table =  paste0('sw_sliding_windows_', user_id, '_', run_id),
                                            geom_type = 'LINESTRING',
                                            is_wkt = T
    )
 
    data$hin_network <- transform_with_epsg(data$hin_network, 4326)
    
    groups <- c()
    blanks <- rep("&#8203;",times=n)
    blanks <- paste(blanks, collapse = "")

    bbox <- as.vector(sf::st_bbox(data$hin_network))
 
    sw_vars = list(names  = c('Bicycle Sliding Windows Results', 'Pedestrian Sliding Windows Results', 'Other Sliding Windows Results'), 
                       data   = list(data$hin_network[ which( data$hin_network$bicycle_crash_score > 0), ] %>% arrange(bicycle_crash_score), data$hin_network[ which( data$hin_network$pedestrian_crash_score > 0), ] %>% arrange(pedestrian_crash_score), data$hin_network[ which( data$hin_network$other_score > 0), ] %>% arrange(other_score)),
                       modes   = c("Bicycle", 'Pedestrian', 'Other'),
                       columns = c("bicycle_crash_score", "pedestrian_crash_score", "other_score"),
                       positions = c("bottomleft", "topleft", "bottomright"),
                       colors =c("YlOrRd", "YlOrBr", "YlGnBu")
    )
    
    study_area_boundary <- fetch_spatial_table(connection = connection,
                                               columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                               schema = 'local_user_data',
                                               table =  return_table_name('study_area', user_id, run_id),
                                               geom_type='POLYGON',
                                               is_wkt=TRUE
    )
    study_area_boundary <- transform_with_epsg(study_area_boundary, 4326)
    
    
    
    leaflet_proxy %>%
      leaflet::clearShapes() %>%
      leaflet::clearControls() %>%
      leaflet::addPolygons(data=study_area_boundary, 
                           weight = 2,
                           fillColor = '#666566', 
                           color = '#666566',
                           stroke=TRUE,
                           fillOpacity = 0,
                           opacity = .8,
                           group = paste0('Study Area', blanks))

    print('Mapping HIN network')

    
    if (nrow(sw_vars$data[[which(sw_vars$names %in% 'Bicycle Sliding Windows Results')]]) > 0) {
      groups <- append(groups, paste0("Bicycle Sliding Windows Results", blanks)) 
    } 
    if (nrow(sw_vars$data[[which(sw_vars$names %in% 'Pedestrian Sliding Windows Results')]]) > 0) {
      groups <- append(groups, paste0("Pedestrian Sliding Windows Results", blanks))  
    }
    if (nrow(sw_vars$data[[which(sw_vars$names %in% 'Other Sliding Windows Results')]]) > 0) {
      groups <- append(groups, paste0("Other Sliding Windows Results", blanks)) 
    }
    
    #  map all available sliding windows results 
    for(i in groups){
          sub <- gsub("&#8203;","" , i ,ignore.case = TRUE)
          the_data <- sw_vars$data[[which(sw_vars$names %in% sub)]]
          name <- sub
          mode <- sw_vars$modes[which(sw_vars$names==sub)]
          column <-   sw_vars$columns[which(sw_vars$names==sub)]
          position <- sw_vars$positions[which(sw_vars$names==sub)]
          color <- sw_vars$colors[which(sw_vars$names==sub)]
          the_data$popup <- paste0('<strong>Road Name</strong>: ', the_data$road_name, '<br>',
                                    '<strong>Functional Classification</strong>: ', the_data$usdot_fun_class_mapped, '<br>',
                                    '<strong>', mode,  ' Sliding Windows Score</strong>: ', the_data[[column]]) 
          color_pal <- colorBin(
            palette = color,
            domain=the_data[[column]],
            pretty = TRUE
          )

          leaflet_proxy %>% 
            leaflet::addPolylines(data=the_data, 
                                  color = color_pal(the_data[[column]]),
                                  opacity = .6,
                                  weight = 2,
                                  popup=~the_data$popup,
                                  group=i
            )  %>% 
            leaflet::addLegend(position = position,
                               pal = color_pal,
                               values = the_data[[column]],
                               title =  paste0(mode, " Sliding <br> Windows Score"),
                               group=i
            )  
        }

    if (length(sw_vars$data[[which(sw_vars$names %in% 'Bicycle Sliding Windows Results')]]) > 0 || length(sw_vars$data[[which(sw_vars$names %in% 'Pedestrian Sliding Windows Results')]]) > 0 || length(sw_vars$data[[which(sw_vars$names %in% 'Other Sliding Windows Results')]]) > 0){
      leaflet_proxy %>%
        leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])  %>%
        leaflet::addLayersControl(baseGroups = c("Grey", "Negative", "OpenStreetMap"), 
                                  overlayGroups = c(groups, paste0("Study Area", blanks)) ,
                                  options = layersControlOptions(collapsed = F),
                                  position = "topright") 
      if (length(groups[-1]) >= 1) { 
        for(i in groups[-1]){
          leaflet_proxy %>% hideGroup(i)
        }
      }
    }
}
    
## To be copied in the UI
# mod_visualize_sliding_windows_ui("visualize_sliding_windows_ui_1")
    
## To be copied in the server
# mod_visualize_sliding_windows_server("visualize_sliding_windows_ui_1")
