#' visualize_data_reporter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_visualize_data_reporter_ui <- function(id){
  ns <- NS(id)
  tagList(
      tags$div(id=ns('final_map_container'), 
               leafletOutput(ns("data_explore_map"), width = "100%", height = "950"),
               absolutePanel(id = ns("abs_controls"), class = "panel panel-default viz_p", fixed = F,
                             draggable = F, top = 72, left = "auto", right = 23, bottom = "auto",
                             width = 300,
                             
                             awesomeCheckboxGroup(
                               inputId = ns("checkb_data"),
                               label = "Select Inputs:", 
                               choices = list("Study Area" = 'study_area', 'Schools (OSM)' = 'schools', 'Pilot Tool Model' = 'ptm_model'),
                               selected = "study_area"
                             ),
                             selectInput(
                               inputId = ns("radio_crashes"),
                               label = "Choose a crash dataset:", 
                               choices = c('-- Choose One --', 'Bicycle Crash', 'Pedestrian Crash', 'Other Crash', 'No Crashes'),
                               width='100%',
                               selected = "-- Choose One --"
                             ),
                             selectInput(
                               inputId = ns("radio_roads"),
                               label = "Choose a roads dataset:", 
                               choices = list("-- Choose One --", 
                                              "Pedestrian Sliding Windows Analysis" = 'ped_score', 
                                              "Bicycle Sliding Windows Analysis" = 'bike_score', 
                                              "Other Sliding Windows Analysis" = 'other_score', 
                                              "Model: Estimated Pedestrian Crash Rate" = 'rt_ped_cost_1y', 
                                              "Model: Estimated Bicycle Crash Rate" = 'rt_bike_cost_1y',
                                              "No Roads"
                               ),
                               width='100%',
                               selected = "-- Choose One --"
                             ),
                             
                             fileInput(inputId = ns("user_data"), 
                                       label = "Upload a .zip file containing a shapefile",  
                                       multiple = FALSE,
                                       accept = c('.zip')
                             ),
                             actionButton(ns('select_map_data'), 'Map Data'),
               )))
}
    
#' visualize_data_reporter Server Functions
#'
#' @noRd 
 
mod_visualize_data_reporter_server <- function(input, output, session, connection, user_id, run_id) {
ns <- session$ns
data <- reactiveValues(
  run_id=NULL, 
  user_id=NULL,
  roads = return_table_name('roads', user_id, run_id),
  crashes = return_table_name('crashes', user_id, run_id),
  study_area = return_table_name('study_area', user_id, run_id),
  sliding_windows = return_table_name('hin_sliding_windows', user_id, run_id),
  sql_literal_study_area=DBI::dbQuoteLiteral(connection, return_table_name('study_area', user_id, run_id)),
  sql_literal_roads=DBI::dbQuoteLiteral(connection, return_table_name('roads', user_id, run_id)),
  sql_literal_crashes=DBI::dbQuoteLiteral(connection, return_table_name('crashes', user_id, run_id)),
  sql_literal_crashes_hin=DBI::dbQuoteLiteral(connection, return_table_name('hin_crashes', user_id, run_id)),
  sql_literal_model_results=DBI::dbQuoteLiteral(connection, return_table_name('hin_output_roads', user_id, run_id)),
  sql_literal_sliding_windows=DBI::dbQuoteLiteral(connection, return_table_name('sw_sliding_windows', user_id, run_id)),
  sql_literal_local_user_data_schema=DBI::dbQuoteLiteral(connection, 'local_user_data'),
  sql_literal_model_outputs_schema=DBI::dbQuoteLiteral(connection, 'model_outputs'),
  sql_literal_model_outputs_scratch_schema=DBI::dbQuoteLiteral(connection, 'model_output_scratch'),
  sql_literal_sw_results_schema=DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs'),
  sa_exists = F,
  rd_exists = F,
  ec_exists = F,
  mr_exists = F,
  sw_exists = F,
  cr_exists = F,
  est_exists=F,
  dns_exists=F,
  ped_crashes = 0,
  bike_crashes = 0,
  other_crashes = 0, 
  total_crashes = 0,
  crash_table=NULL,
  bike_est_sev_cols=c('stan_bike_k', 'stan_bike_a', 'stan_bike_b', 'stan_bike_c', 'stan_bike_o'),
  bike_his_sev_cols=c('tot_bike_k', 'tot_bike_a', 'tot_bike_b', 'tot_bike_c', 'tot_bike_o'),
  ped_est_sev_cols=c('stan_ped_k', 'stan_ped_a', 'stan_ped_b', 'stan_ped_c', 'stan_ped_o'),
  ped_his_sev_cols=c('tot_ped_k', 'tot_ped_a', 'tot_ped_b', 'tot_ped_c', 'tot_ped_o'),
  sev_cols=c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'),
  fclass_cols=c('Major Arterial', 'Minor Arterial', 'Major Collector', 'Minor Collector', 'Local Road'),
  sa=NULL,
  rd=NULL,
  cr=NULL,
  es=NULL,
  ds=NULL,
  dns_table=NULL,
  reporter_dropdown=list(values= c('study_area', 'schools', 'ptm_model', 'Bicycle Crash', 'Pedestrian Crash', 'Other Crash', 'roads','ped_score', 'bike_score', 'other_score', 'rt_ped_cost_1y', 'rt_bike_cost_1y'), 
                               names=c("Study Area", 'Schools', 'Pilot Tool Model (PTM)', 'Bicycle Crashes', 'Pedestrian Crashes', 'Other Crashes', "Roads by Functional Classification", "Pedestrian Sliding Windows Results", "Bicycle Sliding Windows Results", "Other Sliding Windows Results", "Model: Estimated Pedestrian Crash Rate", "Model: Estimated Bicycle Crash Rate"),
                               colors=c("#666566", '#03a5fc', "Oranges", "Oranges", "Oranges", "Oranges", "Oranges", "YlOrBr", "YlOrRd",  "YlGnBu", "YlGnBu", "YlOrRd")
                               ), # notice the other between values and names. Also notice the values of values line match following three reactive values.
  sw_columns = c('ped_score', 'bike_score', 'other_score'),
  sw_columns_coded = c('Pedestrian Crash', 'Bicycle Crash', 'Other Crash'),
  crash_colors=c('#e6ab02', '#d95f02', '#66a61e', '#7570b3', '#e7298a'),
  mod_columns = c('rt_bike_cost_1y', 'rt_ped_cost_1y'),
  local_usr_data=c('study_area', 'Bicycle Crash', 'Pedestrian Crash', 'Other Crash', 'roads'),
  crash_col_levs = c('Property Damage Only (O)', 'Possible Injury (C)', 'Non-Incapacitating Injury (B)', 'Incapacitating Injury (A)', 'Fatality (K)'),
  user_d=NULL,
  fclass_perc_tnames=c("Functional Classification","Total Crashes","Percent of Total Crashes", "Total Miles", "Crashes Per Mile"),
  mode_count_tnames=c("Crashes", "Total Crashes", "Percent of Total"),
  roads_length=NULL,
  pie_chart2_table_hold=NULL,
  top_10_segs=NULL,
  n=1
)

shinyjs::runjs(code = paste0('$("#tab-map_results").click(function(){$("#', session$ns('data_explore_map'), '").trigger("shown");})'))
 
# render map 
render_map(output, 'data_explore_map', position='bottomleft') 

outputOptions(output, "data_explore_map", suspendWhenHidden = FALSE)


# This is a larger function that downloads spatial data and renders the information on a map. 
# Selecting is checked for availability before downloading 
# the groups variable handles layer legends / toggleables  
# blacks are used to avoid duplicating legends when the user select a choice twice over two separate visualizations 
# the boxes variable handles the aggregate bounding box of all layers 
observeEvent(input$select_map_data, {
  tryCatch({
  waiter::waiter_show(
    color='rgba(175, 175, 175, 0.85)',
    html = tagList(
      tags$div(waiter::spin_1()),
      tags$br(),
      tags$div(HTML("Loading ..."))
    )
  )
 
  leafletProxy("data_explore_map") %>%
    leaflet::clearShapes() %>%
    leaflet::clearControls()  %>% 
    leaflet::clearGroup(group=groups)
  # setup 
  list <- c(input$checkb_data, input$radio_roads, input$radio_crashes)
  blanks <- rep("&#8203;",times=data$n)
  blanks <- paste(blanks, collapse = "")
  ex <- c()
  groups <- c()
  bboxes <- c()
  
  # check to make sure data exists 
  for (i in list) {
    if (i %in% data$local_usr_data) {
      if (i %in% data$local_usr_data[2:4]) {i2 = 'crashes'} else {i2 = i}
      schema=DBI::dbQuoteLiteral(connection, 'local_user_data')
      table=DBI::dbQuoteLiteral(connection, paste0(i2, glue::glue('_{user_id}_{run_id}')))
    } else if (i %in% data$sw_columns) {
      schema=DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs')
      table=DBI::dbQuoteLiteral(connection, glue::glue('sw_sliding_windows_{user_id}_{run_id}'))
    } else if (i %in% data$mod_columns) {
      schema=DBI::dbQuoteLiteral(connection, 'model_outputs')
      table=DBI::dbQuoteLiteral(connection, glue::glue('hin_output_roads_{user_id}_{run_id}'))
    }  
    if (i %in% c(data$mod_columns, data$sw_columns, data$local_usr_data) ) {
      e <- test_if_table_exists(connection=connection, schema=schema, table=table)
      ex <- c(ex, e)
    }
    if (i %in% data$sw_columns_coded) {
      e <- test_if_value_exists(connection=connection, schema='local_user_data', column='usdot_mode_mapped', table=glue::glue('crashes_{user_id}_{run_id}'), value=i)
      ex <- c(ex, e)
    }
  }
  if (!all(ex)){
    h <- data$reporter_dropdown$names[( data$reporter_dropdown$values %in% list )]
    if (length(h) == 1) {
      j <- h
    }  else {
      j <- paste0(do.call(paste, c(as.list(h[-length(h)]), sep = ", ")), ', and ', h[length(h)])
    }
    shiny_warming_alert(title="Missing Data", text=HTML(paste0("You selected: ", j, ". At least one of these layers does not exist. You may need to upload data or run an analysis to see the results.")), showConfirmButton=TRUE, showCancelButton=FALSE, size="s",  type="warning")
    waiter::waiter_hide()
  } else {
    if (!is.null(input$user_data)) {
      data$user_d <- process_zipped_shapefile(file = input$user_data)
      data$user_d  <- transform_with_epsg(data$user_d , 4326)
      d_type <- unique(st_geometry_type(data$user_d ))[1] 
      bboxes[[i]] <- sf::st_bbox(data$user_d)
      
      if (d_type %in% c('MULTIPOLYGON', 'POLYGON')) { 
        groups <- append(groups, paste0("Uploaded Polygon Features", blanks))
        leafletProxy("data_explore_map", session) %>% 
          leaflet::addPolygons(data=data$user_d, 
                               color = "#a6d854",
                               weight = 1,
                               opacity = .8,
                               fillOpacity = .2,
                               group = paste0("Uploaded Polygon Features", blanks))
      } else if (d_type %in% c('MULTILINESTRING', 'LINESTRING')) {
        groups <- append(groups, paste0("Uploaded Linear Features", blanks))
        leafletProxy("data_explore_map", session) %>%
          leaflet::addPolylines(data=data$user_d,  
                                weight = 2,
                                opacity = .8,
                                color = "#a6d854",
                                group = paste0("Uploaded Linear Features", blanks))
      } else if (d_type %in% c('MULTIPOINT', 'POINT')) {
        data$user_d <- sf::st_cast(data$user_d, 'POINT')
        groups <- append(groups, paste0("Uploaded Point Features", blanks))
        leafletProxy("data_explore_map", session) %>%
          leaflet::addCircles(data=data$user_d, 
                              radius=10, 
                              opacity=.9, 
                              color='#a6d854', 
                              fillColor='#a6d854', 
                              group = paste0("Uploaded Point Features", blanks))
      } else {
        shiny_warming_alert(title="Wrong Type of Data", text=HTML(paste0("The data you selected to upload is not a point, line, or polygon dataset.")), showConfirmButton=FALSE, showCancelButton=FALSE, timer = 2500, size="s",  type="warning")
        waiter::waiter_hide()
      }
    }
    
    # download data 
    for (i in list) {
      groups <- append(groups, paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks))
      if (i == 'study_area') {
        data$sa <- fetch_spatial_table(connection = connection,
                                       columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                       schema = 'local_user_data',
                                       table =  return_table_name('study_area', user_id, run_id),
                                       geom_type='POLYGON',
                                       is_wkt=TRUE
        )
        data$sa <- transform_with_epsg(data$sa, 4326)
        bboxes[[i]] <- sf::st_bbox(data$sa)
 
        leafletProxy("data_explore_map", session) %>%
          leaflet::addPolygons(data=data$sa, 
                               weight = 2,
                               fillColor = data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)], 
                               color = data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)],
                               stroke=TRUE,
                               fillOpacity = 0,
                               opacity = .8,
                               group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          )  
      }
      if (i == 'schools') {
        data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
        if (!data$sa_exists){
          shiny_warming_alert(title="No Study Area", text='A study area is needed to download school polygons. Please upload a study area.', showConfirmButton=FALSE, showCancelButton=FALSE, timer = 2500, size="s",  type="warning")
        } else {
          data$sa <- fetch_spatial_table(connection = connection,
                                         columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                         schema = 'local_user_data',
                                         table =  return_table_name('study_area', user_id, run_id),
                                         geom_type='POLYGON',
                                         is_wkt=TRUE
          )
          data$sa <- transform_with_epsg(data$sa, 4326)
          b <- sf::st_bbox(data$sa)
          osm_data <- osmdata::opq(bbox = c(b[1], b[2], b[3], b[4])) %>%
            osmdata::add_osm_feature(key = 'amenity', value = 'school') %>%
            osmdata::osmdata_sf ()
          
          if (!is.null(osm_data$osm_polygons) && !is.null(osm_data$osm_multipolygons)) {
            osm_data <- st_union(sf::st_buffer(osm_data$osm_polygons, dist = 0), sf::st_buffer(osm_data$osm_multipolygons, dist = 0))
          } else if (!is.null(osm_data$osm_polygons) && is.null(osm_data$osm_multipolygons)) {
            osm_data <- osm_data$osm_polygons
          } else if (is.null(osm_data$osm_polygons) && !is.null(osm_data$osm_multipolygons)) {
            osm_data <- osm_data$osm_multipolygons
          } else {
            osm_data <- NULL
          }
          
          if (is.null(osm_data)) {
            shiny_warming_alert(title="No Schools", text='There are no schools in your study area.', showConfirmButton=FALSE, showCancelButton=FALSE, timer = 2500, size="s",  type="warning")
          } else {
            bboxes[[i]] <- b
 
            leafletProxy("data_explore_map", session) %>%
              leaflet::addPolygons(data=osm_data,
                                   opacity=0,
                                   weight=2,
                                   fillColor=data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)],
                                   color=data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)],
                                   popup=osm_data$name,
                                   group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
              ) %>%
              leaflet::addLegend(position = "topleft",
                                 colors = data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)], 
                                 labels = 'Schools',
                                 group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
              )
            }
          }
      }
      
      if (i == 'ptm_model') {
        data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
        if (!data$sa_exists){

          shiny_warming_alert(title="No Study Area", text='A study area is needed to download the Pilot Tool Model. Please upload a study area.', showConfirmButton=FALSE, showCancelButton=FALSE, timer = 2500, size="s",  type="warning")

        } else {
          ptm_model <- fetch_spatial_table(connection = connection,
                                       columns='nt.pbm_value, ST_ASEWKT(nt.geom) as geom',
                                       schema = 'static',
                                       table =  glue::glue('national_tracts'),
                                       clauses = glue::glue('nt, local_user_data.study_area_{user_id}_{run_id} st
                                                             WHERE ST_INTERSECTS(ST_Transform(st.geom, 4326), nt.geom)
                                                             '),
                                       geom_type = "POLYGON",
                                       is_wkt = TRUE
          )
          b <- sf::st_bbox(ptm_model)
          

          if (is.null(ptm_model)) {
            shiny_warming_alert(title="No Data", text='There are no tracts from the Pilot Tool Model near your study area.', showConfirmButton=FALSE, showCancelButton=FALSE, timer = 2500, size="s",  type="warning")

          } else {
            bboxes[[i]] <- b
            pfm_network_pel <- colorBin(
              palette =  data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)], 
              domain=ptm_model$pbm_value,
              pretty = TRUE
            )
            leafletProxy("data_explore_map", session) %>%
              leaflet::addPolygons(data=ptm_model,
                                   opacity=1,
                                   weight=2,
                                   color = pfm_network_pel(ptm_model$pbm_value),
                                   fillColor = pfm_network_pel(ptm_model$pbm_value),
                                   fillOpacity = .8,
                                   popup=~paste0(HTML('<strong>Estimated Annual Pedestrian Fatalities</strong>: '), round(as.numeric(ptm_model$pbm_value), 2)),
                                   group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
              ) %>%
              leaflet::addLegend(position = "topleft",
                                 pal = pfm_network_pel, 
                                 values = unique(data$pbm_value),
                                 title = 'Estimated Annual Pedestrian Fatalities',
                                 group=paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
              )
          }
        }
      }
      
      if (i == 'roads') {
        data$rd <- fetch_spatial_table(connection = connection,
                                       columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom, usdot_fun_class_mapped', 
                                       schema = 'local_user_data',
                                       table =  return_table_name('roads', user_id, run_id),
                                       geom_type='LINESTRING',
                                       is_wkt=T
        )
        data$rd <- transform_with_epsg(data$rd, 4326)
        bboxes[[i]] <- sf::st_bbox(data$rd)
      }
      if (i %in% data$sw_columns) {
        
        if (i == 'ped_score') {
          mode_to_paste2 = 'Pedestrian'
        } else if (i == 'bike_score') {
          mode_to_paste2 = 'Bicycle'
        } else if (i == 'other_score') {
          mode_to_paste2 = 'Other'
        }
        
        data$ds <- fetch_spatial_table(connection = connection,
                                       columns= NULL, 
                                       schema = 'sliding_windows_outputs',
                                       table =  return_table_name('sw_sliding_windows', user_id, run_id),
                                       geom_type='LINESTRING',
                                       is_wkt=TRUE
        )
        data$ds <- transform_with_epsg(data$ds, 4326)
        bboxes[[i]] <- sf::st_bbox(data$ds)
        
        data$ds <- data$ds[ which( data$ds[[i]] > 0), ] 
        data$ds <- data$ds[order(data$ds[[i]]),]
        
        hin_network_pel <- colorBin(
          palette =  data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)], 
          domain=data$ds[[i]],
          pretty = TRUE
        )
        leafletProxy("data_explore_map", session) %>%
          leaflet::addPolylines(data=data$ds,
                                color = hin_network_pel(data$ds[[i]]),
                                opacity = .6,
                                weight = 2,
                                popup=~paste0(HTML('<strong>Road Name</strong>: '), data$ds$road_name, '<br>',
                                              HTML('<strong>Functional Classification</strong>: '), data$ds$road_fclass, '<br>',
                                              HTML(paste('<strong>Sliding Windows Score</strong>: ')), data$ds[[i]]),
                                group=paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          ) %>% 
          leaflet::addLegend(position = "topleft",
                             pal = hin_network_pel, 
                             values = data$ds[[i]],  
                             title =  paste0(mode_to_paste2, " Sliding <br>Windows Scores"),
                             group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          )
      }
      if (i %in% data$mod_columns)  {
        data$es <- fetch_spatial_table(connection = connection,
                                       columns= NULL, 
                                       schema = 'model_outputs',
                                       table =  return_table_name('hin_output_roads', user_id, run_id),
                                       geom_type='LINESTRING',
                                       is_wkt=TRUE
        )
        data$es <- transform_with_epsg(data$es, 4326)
        bboxes[[i]] <- sf::st_bbox(data$es)
 
        data$es <- data$es[ which( data$es[[i]] >= 120563), ] 
        data$es <- data$es[order(data$es[[i]]),]
        data$es[[i]] <- round(as.numeric(data$es[[i]]), 2)
        hin_network_es <- colorNumeric(
          palette = data$reporter_dropdown$colors[which(data$reporter_dropdown$values==i)], 
          domain=data$es[[i]]
        )
        if (i == 'rt_ped_cost_1y') {
          mode_to_paste = 'Pedestrian'
          cost_to_paste = 'stan_ped_cost'
        } else if (i == 'rt_bike_cost_1y') {
          mode_to_paste = 'Bicycle'
          cost_to_paste = 'stan_bike_cost'
        }  
        leafletProxy("data_explore_map", session) %>%
          leaflet::addPolylines(data=data$es,
                                color = hin_network_es(data$es[[i]]),
                                opacity = .75,
                                weight = 2,
                                popup=~paste0('<strong>Road Name</strong>: ', data$es$road_name, '<br>',
                                              '<strong>Functional Classification</strong>: ', data$es$road_fclass, '<br>',
                                              '<strong>Estimated Average Annual ', mode_to_paste, ' Crash Cost Per Mile: </strong>', '$', prettyNum(round(as.numeric(data$es[[i]]), 2), big.mark=",",scientific=FALSE), '</br>',
                                              '<strong>Estimated Total 5-year ', mode_to_paste, ' Crash Cost: </strong>', '$', prettyNum(round(as.numeric(data$es[[cost_to_paste]]), 2), big.mark=",",scientific=FALSE)),
                                group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks),
                                popupOptions = popupOptions( maxWidth = 600)  
          ) %>% 
          leaflet::addLegend(position = "topleft",
                             pal = hin_network_es, 
                             values = data$es[[i]],  
                             title =  paste0("Estimated Average Annual <br>", mode_to_paste, " Crash Costs Per Mile"),
                             labFormat = labelFormat(prefix='$'),
                             group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          )

        
      }
      if (i %in% data$local_usr_data[2:4]) {
        i2 <- DBI::dbQuoteLiteral(connection, i)
        data$cr <- fetch_spatial_table(connection = connection,
                                       columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom, severity_mapped, crashes_costs_usdot, usdot_mode_mapped', 
                                       schema = 'local_user_data',
                                       table =  return_table_name('crashes', user_id, run_id),
                                       geom_type='POINT',
                                       is_wkt=TRUE,
                                       clauses=glue::glue('WHERE usdot_mode_mapped = {i2} 
                                                          AND  severity_mapped != \'Property Damage Only (O)\'
                                                          AND severity_mapped != \'Omit From Analysis\'
                                                          AND in_sa_{user_id}_{run_id}')
        )
        data$cr <- transform_with_epsg(data$cr, 4326)
        bboxes[[i]] <- sf::st_bbox(data$cr)
        
        pal_crashes <- colorFactor(palette=data$crash_colors, levels=data$crash_col_levs)
 
        leafletProxy("data_explore_map", session) %>%
          leaflet::addCircles(data=data$cr, 
                              radius=7, 
                              opacity=.9, 
                              fillColor=pal_crashes(data$cr$severity_mapped),
                              color=pal_crashes(data$cr$severity_mapped),
                              popup=~paste0('<strong>Crash Severity: </strong>', data$cr$severity_mapped, '<br>', 
                                            '<strong>Crash Cost: </strong>', '$', prettyNum(round(as.numeric(data$cr$crashes_costs_usdot)), big.mark=",",scientific=FALSE)),
                              group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          ) %>% 
          leaflet::addLegend(position = "bottomright",
                             pal = pal_crashes, 
                             values = unique(data$cr$severity_mapped),
                             title =  i,
                             group = paste0(data$reporter_dropdown$names[which(data$reporter_dropdown$values==i)], blanks)
          )
      }
    }

    if (blanks %in% groups) {
      groups = groups[ -which(groups %in% blanks)]
    }
 
    # now fit bounds  
    box <- get_max_bounds(bboxes)
    waiter::waiter_hide()
    leafletProxy("data_explore_map", session) %>%
      leaflet::fitBounds(box[1], box[2], box[3], box[4])  %>% 
      leaflet::addLayersControl(baseGroups = c("Grey", "Negative", "OpenStreetMap"), 
                                overlayGroups = groups,
                                options = layersControlOptions(collapsed = F),
                                position = "bottomleft")
  }
  waiter::waiter_hide()
 data$n <- data$n + 1
}, error = function(cond){
  c <- toString(cond)
  data$w$hide()
  shiny_warming_alert(title = 'Something Went Wrong.', text=c)
})
})
 
}
    
## To be copied in the UI
# mod_visualize_data_reporter_ui("visualize_data_reporter_ui_1")
    
## To be copied in the server
# mod_visualize_data_reporter_server("visualize_data_reporter_ui_1")
