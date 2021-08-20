#' manage_study_area_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manage_study_area_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    bs4Card(inputId=ns('intro_card'), title='Introduction', width = 12, closable = F,
            fluidRow(
              column(12, 
                     tags$ul(
                       tags$li("You can either choose from a US Census county/parish or upload your own study area."),
                     ),
                     actionButton(ns("model_sa_instructions"), label = 'Instructions', class = 'btn btn-primary'),
              )
            )
    ),
    div(id=ns('study_area_selector'),
        bs4Card(inputId=ns('sa_src_selector'), title='Select a US Census county boundary or upload your own study area', width = 12, closable = F,
        radioGroupButtons(inputId = ns("study_area_choice"), label = "Make a choice :",
                          choices = c(" Nationally Available County Boundary" = "county_area", "Local Study Area Boundary"="local_study_area"),
                          justified = TRUE, 
                          width = '600',
                          selected = 'local_study_area',
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue"))
        ),
        actionButton(ns("study_area_selection"), label = 'Select This Source', status = "primary")
        )
    ),
    
    div(id=ns('study_area_choose_county'),  class='leaflet_none',
        bs4Card(inputId=ns('county_selector_card'), title='Choose a county', width = 12, closable = F,
        fluidRow(
          column(12, 
                div(id = ns("sa_selectors"),
                selectInput(ns('state_selector'), 'Select State', c(counties$state_name)),
                selectInput(ns('county_selector'), 'Select County', c('Select a state'))
                )
                )),
        fluidRow(
          column(12, 
          actionButton(ns("back_county_source_selector_counties"), label = 'Back to Study Area Source Selection', class = "btn btn-primary"),  
          actionButton(ns("submit_county_button"), label = 'Submit this County', class = 'btn btn-success')
        )))), 

    div(id=ns('study_area_choose_local_data'), class='leaflet_none',
        fluidRow(
            col_12(
              bs4Card(inputId='local_study_area_opt', title='Upload your local data', width = 12, closable = F,
                      fluidRow(
                          column(12, 
                                 tags$div(id = ns("local_shapefile_file"), 
                                          p("Please upload a shapefile in a single zipped file. The zipped file must include the following file extensions: .shp, .shx, .dbf, and .prj. The maximum file size allowed is 20 Mbs."),
                                          fluidRow(
                                            column(12,
                                                   fileInput(inputId = ns("load_study_area_shp"), 
                                                             label = "Upload a .zip file containing a shapefile", 
                                                             multiple = FALSE,
                                                             accept = c('.zip')
                                                   )))
                                 ),
                                 fluidRow(
                                   column(12, 
                                          actionButton(ns("back_county_source_selector_local"), width = '200px',
                                                       label = 'Back to Study Area Source Selection',
                                                       class = "btn btn-primary t-2"),
                                          actionButton(ns("submit_local_study_area_data"), class='btn btn-success t-2', , width = '200px',
                                                       label = "Submit Local Study Area Boundary",  
                                          )
                                          ))
                                 ))
                      )
            )
        )),
    hr(),
    fluidRow(
      column(12, 
             leafgl::leafglOutput(ns("study_area_map"),
                           width = '100%',
                           height = '400px')
    ))
  )
}
    
#' manage_study_area_upload Server Function
#'
#' @noRd 
mod_manage_study_area_upload_server <- function(input, output, session, connection, user_id, run_id){
  ns <- session$ns

  ######################################### create local reactives 
  data <- reactiveValues(
    the_study_area=NULL, 
    the_study_area_centroid=NULL, 
    the_study_area_utm=NULL,
    table=return_table_name('study_area', user_id, run_id),
    table_sql_formatted=DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id)),
    run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
    schema_study_area_sql_formatted = DBI::dbQuoteIdentifier(connection, 'local_user_data'),
    sql_literal_sliding_windows_outputs_schema = DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs'),
    sql_literal_model_outputs_schema = DBI::dbQuoteLiteral(connection, 'model_outputs'),
    sql_literal_local_data_schema = DBI::dbQuoteLiteral(connection, 'local_user_data'),
    sql_literal_crashes = DBI::dbQuoteLiteral(connection, glue::glue('crashes_{user_id}_{run_id}')),
    sql_literal_roads = DBI::dbQuoteLiteral(connection, glue::glue('roads_{user_id}_{run_id}')),
    sql_literal_model_results = DBI::dbQuoteLiteral(connection, glue::glue('hin_output_roads_{user_id}_{run_id}')),
    sql_literal_sliding_windows = DBI::dbQuoteLiteral(connection, glue::glue('sw_sliding_windows_{user_id}_{run_id}')),
    sa_bbox=NULL
  )
  
  sa_instructions_modal <- modalDialog(
    title = "Uploading Study Area Data; Instructions",
    easyClose = TRUE,
    next_label = NULL,
    tagList(
      tags$div( 
        p("You may use your local study area or select a US Census County Boundary."),
        p("Using your own study area versus the default does not have a strong bearing on the analysis results. Using your own study area may be helpful if you would like to look at an area smaller than a county. Note that study areas larger than one county cannot be processed at this time. If you are interested in a larger study area, you may create additional scenarios in the tool to run the analysis for each county separately.")
      )
    ),
    footer = tagList(
      actionButton(ns("ok"), "OK")
    )
  )
   
  observeEvent(input$model_sa_instructions, {
    showModal(sa_instructions_modal)
  })
  
  observeEvent(input$ok, {
    removeModal()
  })
  
  # render map 
  render_map(output, 'study_area_map')
 
  shinyjs::runjs(code = paste0('$("#tab-load_data").click(function(){$("#', session$ns('study_area_map'), '").trigger("shown");})'))
  sa_proceed <- function (){
    if (input$study_area_choice == 'county_area'){
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").addClass("leaflet_block");'))
    } else if (input$study_area_choice == 'local_study_area'){
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").addClass("leaflet_block");'))
    }
  }
  
  # There are downstream implications to uploading new study area data. At this time, the best solution is to delete crash, roads, and any analytical results to ensure no errors occur elsewhere (mainly in the dahsboard)
  observeEvent(input$study_area_selection, {
    tryCatch({ 
    model_status <- model_production_status(connection, user_id, run_id)
    if (model_status == 'model_needed_running' || model_status == 'model_currently_running') {
      shiny_warming_alert(title = 'Whoa!', text='Your model is processing! You can\'t upload new data right now.')
    } else if (model_status == 'model_needed') {
      shiny_warming_alert(title = 'Whoa!', text='You\'re in line for model production! You can\'t upload new data right now.')
    } else {
      data$re_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results)
      data$cd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sliding_windows_outputs_schema, table=data$sql_literal_sliding_windows)
      data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_data_schema, table=data$sql_literal_roads)
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_data_schema, table=data$sql_literal_crashes)
      if ( data$re_exists || data$cd_exists || data$rd_exists || data$cr_exists ) {
        showModal(delete_analysis_results_sa)
      } else {
        sa_proceed()
      }}
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })

  # deletes data
  observeEvent(input$delete_data_sa, {
    tryCatch({
      shiny_warming_alert(title='Results Deleted', text="Proceeding", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
      removeModal()
      delete_tables(connection, list_of_tables=list(c('model_outputs', glue::glue('hin_output_roads_{user_id}_{run_id}')), 
                                                    c('model_output_scratch', glue::glue('hin_sliding_windows_{user_id}_{run_id}')), 
                                                    c('sliding_windows_outputs', glue::glue('sw_sliding_windows_{user_id}_{run_id}')), 
                                                    c('model_output_scratch', glue::glue('hin_crashes_{user_id}_{run_id}')),
                                                    c('local_user_data', glue::glue('crashes_{user_id}_{run_id}')),
                                                    c('local_user_data', glue::glue('roads_{user_id}_{run_id}'))
      ))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_since_model_desired'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_status'), new_value=DBI::dbQuoteLiteral(connection, 'no_model_desired'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_process_time'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_mode_finished'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'move_windows_long_comp'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_comp'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_year_col'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_serv_col'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_rep_id_col'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_source'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_mode_col'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_name'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_source'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_id'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'roads_fun_c_col'), new_value='NULL')
      sa_proceed()
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
  
  observeEvent(input$do_not_delete_data_sa, {
    removeModal()
  })
  
  observeEvent(input$back_county_source_selector_local, {
    reset('load_study_area_shp')
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").addClass("leaflet_none");'))
    leafletProxy("study_area_map", session) %>%
      clearShapes()
  })
  
  observeEvent(input$back_county_source_selector_counties, {
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").addClass("leaflet_none");'))
    leafletProxy("study_area_map", session) %>%
      clearShapes()
  })
  
  delete_analysis_results_sa <- mod_delete_model_ui("delete_model_ui_1", confirm_button = ns("delete_data_sa"), cancel_button = ns("do_not_delete_data_sa"), text="To upload a new study area we need to delete your roads and crash data, Sliding Windows Analysis, and the Safer Streets Model where they exist. By uploading new data, you will delete those results. Do you want to proceed with deleting your results and upload new data? Remember, you can always create a new study if you'd like to analyze multiple places.")
  
  # process to upload local study area 
  observeEvent(input$load_study_area_shp, {
    
    if (is.null(input$load_study_area_shp)) {
      shiny_warming_alert(title = 'No Data', 
                          text = 'No data selected.')
    } else if (!grepl("\\.zip$", input$load_study_area_shp)) {
      shiny_warming_alert(title = 'Not a zipped file', 
                          text = 'Please upload a shapefile in a single zipped file. Please include the mandatory file extensions needed for a shapefile: .shp, .shx, .dbf, and .prj.')
      reset('load_study_area_shp')
    } else {
      
      tryCatch({
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
        data$the_study_area <- process_zipped_shapefile(file = input$load_study_area_shp)
        at_check <- attribute_check(data=data$the_study_area, attributes=c( paste0('tdg_id_{user_id}_{run_id}')))
        if (at_check) {
          shiny_warming_alert(title = 'Oh no!', text=glue::glue('Sorry, you cannot have an attibute in your study area data named \'tdg_id_{user_id}_{run_id}\'. Please rename the attribute in your local data.'))
        } else {
        if (unique(st_geometry_type(data$the_study_area))[1] != 'MULTIPOLYGON' && unique(st_geometry_type(data$the_study_area))[1] != 'POLYGON'){
          reset('load_study_area_shp')
          # inform user if the type of data is incorrect
          shiny_warming_alert(title = 'Wrong Data.', text='Your study data is not a polygon. Please upload your data as a polygon data')
          
        }  else {
          
          # convert spatial type
          data$the_study_area <- sf::st_cast(data$the_study_area, 'POLYGON')
          
          # convert to 4346
          data$the_study_area <- transform_with_epsg(data$the_study_area, 4326)
          
          # get bbox 
          bb <- sf::st_bbox(data$the_study_area) 
          
          
          # test for crashes out of bounds of US 
          if ( bb[1] < -180 || bb[1] > 180 || 
               bb[2] < -84  || bb[2] > 84 ||
               bb[3] < -180 || bb[3] > 180 ||                
               bb[4] < -84  || bb[4] > 84                
          )  {
            shiny_warming_alert(title = 'Out of bounds.', 
                                text=paste0('We\'ve discovered an invalid value range from the bounding box of your data. We converted your data to SRID 4326 (WGS84 coordinate reference system), and here are the boundaries we found,  
                                                       Longitude (xmin: ', bb[1], ', xmax: ', bb[3], '), Latitude (ymin: ', bb[2], ', ymax: ', bb[4], '). After converting your data, the longitude values should sit between -180 and 180, and the latitude values should sit between -85 and 85. This problem can occur if the coordinate values for your geometries don\'t fit the same SRID. Please fix this issue before proceeding.'
                                ))
          } else {
            # get median centroid
            data$the_study_area_centroid <- get_medroid(data$the_study_area)
            
            # get bbox as vector for map extents
            data$sa_bbox <- sf::st_bbox(data$the_study_area)  %>% as.vector()
 
            # get utm got from median centroid 
            data$the_study_area_utm <- get_utm_code(data$the_study_area_centroid$x, data$the_study_area_centroid$y)
            
            # map features 
            leafletProxy("study_area_map", session) %>%
              clearShapes() %>%
              addPolygons(data=data$the_study_area, 
                          fillColor = "aliceblue", 
                          color = "#fcba03") %>% 
              leaflet::fitBounds(data$sa_bbox[1], data$sa_bbox[2], data$sa_bbox[3], data$sa_bbox[4])
            waiter::waiter_hide()
            Sys.sleep(.1)
            shiny_warming_alert(title='Review your data', text="Review the map, if the study area looks correct, click the submit button.", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=3000, type="success")
       }}}
      }, error = function(cond){
        c <- toString(cond)
        waiter::waiter_hide()
        reset('load_study_area_shp')
        shiny_warming_alert(title = 'Something Went Wrong.', text=c)
      })
    }
  })
          
  ############################################################################# LOCAL STUDY AREA
  # submit local study area. Several things occur here, including: 
  # user's local study area gets sent to postgresql database 
  # local study area get's transformed to local UTM server side 
  # crs is stored on the accounts table to ensure roads and crashes all use the same CRS
  observeEvent(input$submit_local_study_area_data, {
    tryCatch({
      if ( is.null(data$the_study_area) ) {
        shiny_warming_alert(title = 'No Data', 
                            text = 'Please upload your study area.')
      } else {
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
        DBI::dbGetQuery(connection, glue::glue('DROP TABLE IF EXISTS local_user_data.{data$table_sql_formatted};'))
        to_postgresql(connection=connection, 
                      geodata=data$the_study_area, 
                      table=data$table, 
                      user_id=user_id, 
                      run_id=run_id,
                      schema='local_user_data',
                      geom_type='MULTIPOLYGON',
                      promote_to_multi=TRUE,
                      srid=4326
        )
        account_bbox_udpates(connection=connection, user_id = user_id, run_id = data$run_id_sql_formatted, shape = data$the_study_area)  
        update_account_info(connection=connection, user_id = user_id, run_id = data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crs'), new_value = DBI::dbQuoteString(connection, data$the_study_area_utm))
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'sa_o_source'), new_value=DBI::dbQuoteString(connection, glue::glue('Local study area data from user. Submitted {Sys.time()}')))
        psql_update_epsg(connection=connection, table=data$table_sql_formatted, new_epsg=data$the_study_area_utm)
        shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_local_data'), '").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(2) > a\').click(); ')  
        waiter::waiter_hide()
      }
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
  
  ###################################################################### CENSUS COUNTY SELECTOR 
  # update county selector as new states are selected, uses selected_state_reactive()
  observe({
    updateSelectInput(session, "county_selector",
                      choices = unique(selected_state_reactive()$county_namelsad)
    )})

  # create reactive object as selected state 
  selected_state_reactive <- reactive({
    selected_state <- counties[which(counties$state_name == input$state_selector),]
    return(selected_state)
  })
  
  # create reactive object as selected county 
  selected_county_reactive <- reactive({
    selected_county <- counties[which(selected_state_reactive()$county_namelsad == input$county_selector),]
    return(selected_county)
  })

  # handler to get spatial data when the user selects a county 
  observeEvent(input$county_selector, {
    tryCatch({
    if (input$county_selector != "Select a state"){
      #grab selector 
      
      c <- DBI::dbQuoteString(connection, get_county_fip(state=input$state_selector, county=input$county_selector))
      s <- DBI::dbQuoteString(connection, get_state_fip(state=input$state_selector))
      
      #retrieve county 
      data$the_study_area  <- fetch_spatial_table(connection = connection,
                                                  columns= 'statefp, countyfp, ST_AsEWKT((ST_Dump(ST_SIMPLIFY(geom, .01))).geom) as geom', 
                                                  schema = 'static',
                                                  table =  'us_county_2018',
                                                  clauses = glue::glue('WHERE statefp = {s} AND countyfp = {c}'),
                                                  is_wkt = T,
                                                  geom_type = "POLYGON")
      
      # get centroid
      data$the_study_area <- transform_with_epsg(data$the_study_area, 4326)
      data$the_study_area_centroid <- get_medroid(data$the_study_area)
      data$the_study_area_utm <- get_utm_code(data$the_study_area_centroid$x, data$the_study_area_centroid$y)
      # get bbox as vector for map extents
      data$sa_bbox <- sf::st_bbox(data$the_study_area)  %>% as.vector()
      
      #reload map with data
      leafletProxy("study_area_map", session) %>%
        clearShapes() %>% 
        addPolygons(data = data$the_study_area, 
                    fillColor = "aliceblue", 
                    color = "#fcba03") %>%
        leaflet::fitBounds(data$sa_bbox[1], data$sa_bbox[2], data$sa_bbox[3], data$sa_bbox[4])

      shinyjs::runjs(code = paste0('$("#', session$ns('back_county_source_selector_counties'), '").toggleClass("hide")'))
      shinyjs::runjs(code = paste0('$("#', session$ns('sa_selectors'), '").toggleClass("hide")'))
      shinyjs::runjs(code = paste0('$("#', session$ns('submit_county_button'), '").toggleClass("hide")'))
    }}, error = function(cond){
      c <- toString(cond)
      reset('load_study_area_shp')
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })

  # submit local study area as selected census county. Several things occur here, including: 
  # user's local study area gets sent to postgresql database 
  # local study area get's transformed to local UTM server side 
  # crs is stored on the accounts table to ensure roads and crashes all use the same CRS
  observeEvent(input$submit_county_button, {
    tryCatch({
      
      # get fips 
      c <- DBI::dbQuoteString(connection, get_county_fip(state=input$state_selector, county=input$county_selector))
      s <- DBI::dbQuoteString(connection, get_state_fip(state=input$state_selector))

      # udpate account bounding box area 
      account_bbox_udpates(connection=connection, user_id = user_id, run_id = data$run_id_sql_formatted, shape = data$the_study_area)
      update_account_info(connection=connection, user_id = user_id, run_id = data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crs'), new_value = DBI::dbQuoteString(connection, data$the_study_area_utm))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'sa_o_source'), new_value=DBI::dbQuoteString(connection, glue::glue('US Census County Boundary, 2018. Submitted {Sys.time()}')))
      
      # load data 
      create_county_query <- glue::glue('
                                       DROP TABLE IF EXISTS local_user_data.{data$table_sql_formatted}; 
                                       SELECT * 
                                       INTO local_user_data.{data$table_sql_formatted}
                                       FROM static.us_county_2018 
                                       WHERE statefp = {s} and countyfp = {c};
                                       
                                       ALTER TABLE local_user_data.{data$table_sql_formatted}
                                       ALTER COLUMN geom TYPE geometry(MultiPolygon,{data$the_study_area_utm}) 
                                       USING ST_Transform(geom,{data$the_study_area_utm});
                                       
                                       DROP INDEX IF EXISTS index_{user_id}_{run_id}_study_area;
                                       CREATE INDEX index_{user_id}_{run_id}_study_area
                                       ON local_user_data.{data$table_sql_formatted}
                                       USING GIST (geom);
                                       ANALYZE local_user_data.{data$table_sql_formatted};
                                        
                                       ')
      DBI::dbGetQuery(connection, create_county_query)
      
      # Hid submit button 
      shinyjs::runjs(code = paste0('$("#', session$ns('submit_county_button'), '").toggleClass("hide")'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_selector'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('study_area_choose_county'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(2) > a\').click(); ')  
      
    }, error = function(cond){
      c <- toString(cond)
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
}

#     To be copied in the UI
# mod_manage_study_area_upload_ui("manage_study_area_upload_ui_1")
    
## To be copied in the server
# callModule(mod_manage_study_area_upload_server, "manage_study_area_upload_ui_1")
 
