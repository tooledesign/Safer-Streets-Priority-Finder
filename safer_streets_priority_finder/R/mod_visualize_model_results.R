#' visualize_model_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualize_model_results_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' visualize_model_results Server Functions
#'
#' @noRd 
mod_visualize_model_results_server <- function(input, output, session, connection, user_id, run_id, data, leaflet_proxy, n){
    ns <- session$ns
    
    
    list <- list(sev_levels=c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)",  "Possible Injury (C)", "Property Damage Only (O)"), 
                 col_options = c('Oranges', 'Greens'), 
                 cols = c('e_cr_ped_k', 'e_cr_ped_a', 'e_cr_ped_b', 'e_cr_ped_c', 'e_cr_ped_o'), 
                 mode = 'Pedestrian'
                 )

    study_area_boundary <- fetch_spatial_table(connection = connection,
                                             columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                             schema = 'local_user_data',
                                             table =  return_table_name('study_area', user_id, run_id),
                                             geom_type='POLYGON',
                                             is_wkt=TRUE
    )
    study_area_boundary <- transform_with_epsg(study_area_boundary, 4326)
 
    
    data$model_results <- transform_with_epsg(data$model_results, 4326)
    bk_data <- data$model_results[ which( data$model_results$rt_bike_cost_1y >= 120563), ] %>% arrange(rt_bike_cost_1y)
    pd_data <- data$model_results[ which( data$model_results$rt_ped_cost_1y >= 120563), ] %>% arrange(rt_ped_cost_1y)
 
    if ( nrow(bk_data) < 1 && nrow(pd_data) < 1 && (nrow(data$model_results ) > 0 || nrow(data$model_results) > 0)) {
      shiny_warming_alert(title='Low Values', text="Your results can't be visualized because no segments have an estimated annual average cost per mile greater than $120,563. Your results are still available for download.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
    } else {
      bbox <- as.vector(sf::st_bbox(data$model_results))
      blanks <- rep("&#8203;",times=n)
      blanks <- paste(blanks, collapse = "")
      
      groups <- c()
      groups <- append(groups, paste0('Study Area', blanks)) 
       
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
                             group = paste0('Study Area', blanks)
        )

      if ( nrow(pd_data) > 0 ) {
        ped_cost <- colorNumeric(
          palette = "YlGnBu", 
          domain=as.numeric(pd_data$rt_ped_cost_1y)
        )
        groups <- append(groups, paste0('Estimated Pedestrian Crash Cost', blanks)) 
        leaflet_proxy %>%
        leaflet::addPolylines(data=pd_data,
                              color = ped_cost(as.numeric(pd_data$rt_ped_cost_1y)),
                              opacity = .75,
                              weight = 2,
                              popup=~paste0('<strong>Road Name: </strong>', pd_data$road_name, '</br>',
                                            '<strong>Functional Classification: </strong>',  pd_data$road_fclass,'</br>',
                                            '<strong>Estimated Average Annual Pedestrian Crash Cost Per Mile: </strong>', '$', prettyNum(round(as.numeric(pd_data$rt_ped_cost_1y), 2), big.mark=",",scientific=FALSE),'</br>',
                                            '<strong>Estimated Total 5-year Pedestrian Crash Cost: </strong>', '$', prettyNum(round(as.numeric(pd_data$stan_ped_cost), 2), big.mark=",",scientific=FALSE),'</br>',
                                            '<strong>Estimated 5-year Pedestrian Fatalities (K): </strong>', round(as.numeric(pd_data$e_cr_ped_k), 2),'</br>',
                                            '<strong>Estimated 5-year Pedestrian Incapacitating Injuries (A): </strong>', round(as.numeric(pd_data$e_cr_ped_a), 2),'</br>',
                                            '<strong>Estimated 5-year Pedestrian Non-Incapacitating Injuries (B): </strong>', round(as.numeric(pd_data$e_cr_ped_b), 2),'</br>',
                                            '<strong>Estimated 5-year Pedestrian Possible Injuries (C): </strong>', round(as.numeric(pd_data$e_cr_ped_c), 2),'</br>',
                                            '<strong>Estimated 5-year Pedestrian Property Damage Only (O): </strong>', round(as.numeric(pd_data$e_cr_ped_o), 2),'</br>',
                                            '<strong>Total Historical Pedestrian Crashes: </strong>', pd_data$tot_ped_all
                              ),
                              group = paste0('Estimated Pedestrian Crash Cost', blanks),
                              popupOptions = popupOptions( maxWidth = 600)
        ) %>% 
          leaflet::addLegend(position = "bottomleft",
                             pal = ped_cost,
                             values = pd_data$rt_ped_cost_1y,    
                             title =  "Estimated Average Annual <br>Pedestrian Crash Costs Per Mile",
                             labFormat = labelFormat(prefix='$'),
                             group = paste0('Estimated Pedestrian Crash Cost', blanks)
          )
      }
      
      if ( nrow(bk_data) > 0 ) {
        bike_cost <- colorNumeric(
          palette = "YlOrRd",   
          domain=as.numeric(bk_data$rt_bike_cost_1y)
        )
        groups <- append(groups, paste0('Estimated Bicycle Crash Cost', blanks))
 
        leaflet_proxy %>%
          leaflet::addPolylines(data=bk_data,
                                color = bike_cost(as.numeric(bk_data$rt_bike_cost_1y)),
                                opacity = .75,
                                weight = 2,
                                popup=~paste0('<strong>Road Name: </strong>', bk_data$road_name, '</br>',
                                              '<strong>Functional Classification: </strong>',  bk_data$road_fclass,'</br>',
                                              '<strong>Estimated Average Annual Bicycle Crash Cost Per Mile: </strong>', '$', prettyNum(round(as.numeric(bk_data$rt_bike_cost_1y), 2), big.mark=",",scientific=FALSE),'</br>', 
                                              '<strong>Estimated Total 5-year Bicycle Crash Cost: </strong>', '$', prettyNum(round(as.numeric(pd_data$stan_bike_cost), 2), big.mark=",",scientific=FALSE),'</br>',
                                              '<strong>Estimated 5-year Bicycle Fatalities (K): </strong>', round(as.numeric(bk_data$e_cr_bike_k), 2),'</br>',
                                              '<strong>Estimated 5-year Bicycle Incapacitating Injuries (A): </strong>', round(as.numeric(bk_data$e_cr_bike_a), 2),'</br>',
                                              '<strong>Estimated 5-year Bicycle Non-Incapacitating Injuries (B): </strong>', round(as.numeric(bk_data$e_cr_bike_b), 2),'</br>',
                                              '<strong>Estimated 5-year Bicycle Possible Injuries (C): </strong>', round(as.numeric(bk_data$e_cr_bike_c), 2),'</br>',
                                              '<strong>Estimated 5-year Bicycle Property Damage Only (O): </strong>', round(as.numeric(bk_data$e_cr_bike_o), 2),'</br>',
                                              '<strong>Total Historical Bicycle Crashes: </strong>', bk_data$tot_bike_all
                                ),
                                group = paste0('Estimated Bicycle Crash Cost', blanks),
                                popupOptions = popupOptions( maxWidth = 600)
          ) %>% 
          leaflet::addLegend(position = "bottomleft",
                             pal = bike_cost,
                             values = bk_data$rt_bike_cost_1y, 
                             title =  "Estimated Average Annual <br>Bicycle Crash Costs Per Mile",
                             labFormat = labelFormat(prefix='$'),
                             group = paste0('Estimated Bicycle Crash Cost', blanks)
          )}

      leaflet_proxy %>%
        leaflet::fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])  %>% 
        leaflet::addLayersControl(baseGroups = c("Grey", "Negative", "OpenStreetMap"), 
                                  overlayGroups = c(groups),
                                  options = layersControlOptions(collapsed = F),
                                  position = "topright")
      
      if (nrow(bk_data) > 0 && nrow(pd_data) > 0)
        leaflet_proxy %>%
        leaflet::hideGroup(paste0('Estimated Bicycle Crash Cost', blanks)) 
      
      if (nrow(bk_data) > 0 && nrow(pd_data) < 1) {
        shiny_warming_alert(title='Low Values', text="Your pedestrian model results can't be visualized because no segments have an estimated annual average cost per mile of greater than $120,563. Your results are still available for download.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
      }
 
      if (nrow(bk_data) < 1 && nrow(pd_data) > 0) {
        shiny_warming_alert(title='Low Values', text="Your bicycle model results can't be visualized because no segments have an estimated annual average cost per mile of greater than $120,563. Your results are still available for download.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
      }
    }
  } 
    
## To be copied in the UI
# mod_visualize_model_results_ui("visualize_model_results_ui_1")
    
## To be copied in the server
# mod_visualize_model_results_server("visualize_model_results_ui_1")
