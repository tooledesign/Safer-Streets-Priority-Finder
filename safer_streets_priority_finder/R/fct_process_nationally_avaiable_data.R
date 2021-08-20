# creates crash data for user if the select the nationally available data option 
cr_proceed <- function(data, connection, output, session, input, user_id, run_id){
  data$crashes_utm <- get_account_crs(
    connection = connection, 
    user_id = user_id,  
    run_id = data$run_id_sql_formatted
  )
  tryCatch({
  # test if study area exists 
  data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_data, table=data$sql_literal_sa_table)
  if (!data$sa_exists) {
    shiny_warming_alert(title = 'Upload a study area', text='Please upload a study area before uploading your crash dataset.')
    
    # test if study area overlaps with crashes 
    data$sa <- fetch_spatial_table(connection = connection,
                                   columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                   schema = 'local_user_data',
                                   table =  return_table_name('study_area', user_id, run_id),
                                   geom_type='POLYGON',
                                   is_wkt=TRUE
    )

  } else {
    data$rds_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_data, table=data$sql_literal_roads_table)
    if (!data$rds_exists) {
      shiny_warming_alert(title = 'Upload roads data', text='Please upload a roads dataset before uploading your crash dataset.')
    } else {
      if (input$crash_choice_selector == 'local_crash_data'){
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").addClass("leaflet_block");'))
        
      } 
      else if (input$crash_choice_selector == 'fars_data'){
        
        #count the number of crashes 
        q <- glue::glue('
                  SELECT COUNT(*) as count
                   FROM static.fars_processed f, 
                        (SELECT ST_Transform(geom,4326) as geom  FROM local_user_data.{return_table_name(\'study_area\', user_id, run_id)} WHERE geom IS NOT NULL) c
                   WHERE ST_INTERSECTS(c.geom, f.geom)
                ;
              ')
        count_crashes <- DBI::dbGetQuery(connection, q)
        if (count_crashes[1,1] == 0) {
          shiny_warming_alert(title="No Crashes", text='There are FARS crashes in your study area.', showConfirmButton=TRUE, showCancelButton=FALSE, size="s",  type="warning")
        } else {
          create_fars_data(connection, user_id, run_id)
          shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").removeClass("leaflet_none");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").addClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('crashes_fars'), '").removeClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('crashes_fars'), '").addClass("leaflet_none");'))
          shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(4) > a\').click(); ') 
        }
      }
    }}
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# creates roads data for user if the select the nationally available data option 
rds_proceed <- function(data, connection, output, session, input, user_id, run_id){
  data$roads_utm <- get_account_crs(
    connection = connection, 
    user_id = user_id,  
    run_id = data$run_id_sql_formatted
  )
  tryCatch({
  if (is.null(data$roads_utm) || is.na(data$roads_utm) || data$roads_utm == "NA") {
    shiny_warming_alert(title = 'Upload a study area', text='Please upload a study area before uploading your roads dataset.')
  } else {
    if (input$road_data_choice == 'local_roads_data'){
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").addClass("leaflet_block");'))
      
    } else if (input$road_data_choice == 'tiger_roads_data'){
      #  if (!sa_from_nationally_aval_data(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted)) {
      if (FALSE) {
        
        shiny_warming_alert(title = 'You did not select a US Census county boundary.', text='Currently, OSM data are only available for analysis at the county level using a US census county boundary as the study area. Please select a county boundary in the load study area step.')
      } else {
         waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        
        # creates OSM data 
        create_account_tiger(connection, user_id, run_id)
        roads_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id))
        q <- glue::glue("SELECT  
              SUM(ST_LENGTH(geom))/1609.334 as length_miles 
              FROM local_user_data.{roads_table};")
        
        la  <-  DBI::dbGetQuery(connection, q)[1,1]
        print(paste0('Total miles of roadway from OSM: ', la))
        
        # we currently don't allow roads datasets longer than LA county 
        if (la >26708.43) {
          roads_table <-  DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id))
          DBI::dbGetQuery(connection, glue::glue('DROP TABLE IF EXISTS local_user_data.{roads_table};') )
          waiter::waiter_hide()
          shiny_warming_alert(title = 'Can Not Process', text=glue::glue('Sorry, the total length of your road network is too long. Please reduce the overall size of your network and try again.'), showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
        } else {
          update_osm_attributes(connection, user_id, run_id )
          shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(3) > a\').click(); ')  
          waiter::waiter_hide()
        }
      }
    }}
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
)
}