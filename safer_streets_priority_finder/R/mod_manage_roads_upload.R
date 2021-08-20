#' manage_roads_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manage_roads_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    bs4Card(inputId=ns('intro_card'), title='Introduction', width = 12, closable=F,
            tags$ul(
              tags$li("You can select nationally available OpenStreetMap (OSM) data or upload your own local roads data."),
              tags$li("A downside to using the default OSM data is that it will not have your local roadway network IDs, so it may be more difficult to integrate outputs with your local roadway network dataset. When you upload your own roadway network data, you will be able to join the analysis results back to your road network later on. You should also consider whether you are fine with the OSM data’s allocation of functional classes on your road network. WARNING: We make no guarantees about the reliability of OSM data."),
              tags$li("If uploading your own local roads data, you’ll need to format your data for the tool. Find more information on formatting your data by clicking on the buttons below. It may take several minutes to process your data upon upload, depending on the size of the roadway network."),
              tags$li(HTML("<div><a href=\"https://en.wikipedia.org/wiki/OpenStreetMap\"target=\"_blank\" style=\"color: #007bff;\">Click here</a> to learn more about OSM data.</div>")),
            ),
            actionButton(ns("model_roads_instructions"), label = 'Instructions', class = 'btn btn-primary'),
            actionButton(ns("model_roads_pro_format"), label = 'Properly Formatted Road Data', class = 'btn btn-primary')
     
        ),
        div(id=ns('roads_choice'),
            bs4Card(inputId=ns('source_selector_roads'), title='Select a source for your roads data', width = 12, closable=F,
              radioGroupButtons(inputId = ns("road_data_choice"), label = "Make a choice :",
                                # choices = c("Local Roads Data" = "local_roads_data"),
                                choices = c("OpenStreetMap Roads" = "tiger_roads_data", "Local Roads Data" = "local_roads_data"),
                                justified = TRUE, 
                                width = '450',
                                selected = 'local_roads_data',
                                individual = TRUE,
                                checkIcon = list(
                                  yes = tags$i(class = "fa fa-circle", 
                                               style = "color: steelblue"),
                                  no = tags$i(class = "fa fa-circle-o", 
                                              style = "color: steelblue"))
              ),
              actionButton(ns("select_roads_data_choice"), label = 'Select this roads data', class = 'btn btn-primary')
            )
      ),
      div(id=ns('roads_choice_local'), class='leaflet_none',
          tags$div(class="roads_slideshow-container", checked=NA, 
                   #############################################################
                   ####### First Slide: Load Roads Data
                   tags$div(id=ns("roads_slides1"), checked=NA, 
                            fluidRow(
                              col_12( 
                                bs4Card(inputId=ns('roads_upload_1'), title='Identify your roads data', width = 12, closable=F,
                                     radioGroupButtons(inputId = ns("roads_data"), label = "Make a choice :",
                                                       choices = c("Zipped Shapefile" = "shp"),
                                                       justified = TRUE, status = "primary",
                                                       width = '200',
                                                       checkIcon = list(yes = icon("dot-circle-o"), no = icon("times-circle-o")
                                                       )
                                     ),
                                     p("Please upload a shapefile in a single zipped file. Please include the mandatory file extensions needed for a shapefile: .shp, .shx, .dbf, and .prj. The maximum allowable length of roadway miles that can be uploaded is 26,700 miles. If your data exceeds this limit, you will need to clip the data prior to uploading it. Consider excluding features prior to upload if you experience latency."),
                                     conditionalPanel(condition = "input.roads_data == 'shp'", ns = ns,
                                                      fileInput(inputId = ns("roads_shp"), 
                                                                label = "Upload a .zip file containing a shapefile",  
                                                                multiple = FALSE,
                                                                accept = c('.zip')
                                                      )
                                     ),
                                     
                                     conditionalPanel(condition = "input.roads_data == 'geojson'", ns = ns,
                                                      fileInput(inputId = ns("roads_geojson"), label = "Upload a .geojson file")
                                     ),
                                     fluidRow(
                                       column(12,
                                              actionButton(ns("back_to_source_selector_local"), 
                                                           label = 'Back to Roads Source Selection', 
                                                           class = 'btn btn-primary'
                                                           )
                                       ))
                              )),
 
                              )), # end of first slide
                   ##########################################################
                   ############ Second slide, user select variables 
                   tags$div(id=ns("roads_slides2"), class="leaflet_none", checked=NA, 
                            bs4Card(inputId=ns('roads_upload_2'), title='Attribute Selection', width = 12, closable=F,
                            p('Please indicate how the functional classification, road name, and unique identifier attributes from your data correspond to standard values.'),
                            br(),
                            fluidRow(
                              column(6,
                                     bs4Card(inputId=ns('var_sel_1'), 
                                             title = "Select the attribute associated with the Functional Classification.", 
                                             width = 12, 
                                             collapsible = F, 
                                             closable = F, 
                                             solidHeader = T, 
                                             status = 'info',
                                             helpText('Select the Functional classification associated with each road segment (i.e., \'Local Road\', \'Major Arterial\', etc.)'),
                                             selectInput(ns("func_class_variable"), 
                                                         label = NULL,
                                                         choices = 'Please upload your roads data'),
                                             uiOutput(ns("func_class_variable_notice_1"))
                             
                              )),
                              column(6,
                                     bs4Card(inputId=ns('var_sel1'), 
                                             title = "Select the Road Name Attribute", 
                                             width = 12, 
                                             collapsible = F, 
                                             closable = F, 
                                             solidHeader = T, 
                                             status = 'info',
                                             helpText('Select the attribute associated with the name of each road segment.'),
                                         selectInput(ns("road_name"), 
                                                     label = NULL,
                                                     choices = 'Please upload your roads data')
                              ))),
                            fluidRow(
                              column(6,
                                     bs4Card(inputId=ns('var_sel_3'), 
                                             title = "Select the Unique Identifier Attribute", 
                                             width = 12, 
                                             collapsible = F, 
                                             closable = F, 
                                             solidHeader = T, 
                                             status = 'info',
                                             helpText('Select the attribute associated with the unique identifer of your roads table'),
                                             selectInput(ns("pri_key_roads"), 
                                                         label = NULL,
                                                         choices = 'Please upload your roads data')
                                     ))),
                            fluidRow(
                              column(12,
                                     actionButton(ns("back_1"), 
                                                  label = "Go Back",
                                                  class = "btn btn-primary"
                                     ),
                                     actionButton(ns("page_3"), 
                                                  label = "Next",
                                                  class = "btn btn-primary"
                                     ))))
                   ), # end of second slide
                   
                   ##########################################################
                   ############ Third slide, manage nulls 
                   tags$div(id=ns("roads_slides3"), class="leaflet_none", checked=NA, 
                            bs4Card(inputId=ns('roads_upload_3'), title='Standardize Your Variables', width = 12, closable=F,
                            p('Please assign your local functional classification variable to standard values used in the analysis.'),
                            br(),
                            fluidRow(
                              column(12,
                                     DT::dataTableOutput(ns('fun_class_map_table'),  width = 600),
                                     br(),
                                     verbatimTextOutput(ns('sel'))
                              )),
                            fluidRow(
                              column(12,
                                     actionButton(ns("back_2"), 
                                                  label = "Go Back",
                                                  class = "btn btn-primary"
                                     ),
                                     actionButton(ns("page_4"), 
                                                  label = "Next",
                                                  class = "btn btn-primary"
                                     ))))
                   ),
                   ##########################################################
                   ############ Forth slide, user maps functional classification
                   tags$div(id=ns("roads_slides4"), class="leaflet_none", checked=NA, 
                            bs4Card(inputId=ns('roads_upload_4'), title='Identify how you\'d like to handle NULLs and NAs', width = 12, closable=F,
                                    p("Confirm how you'd like to handle NULLs and NAs in your functional class values, which will need to be omitted. Alternatively, if you have local knowledge that most or all of the null or unknown functional class values are usually related to a specific type of street (e.g., undesignated residential streets), you may assign them to the functional class that best fits local conditions."),
                            fluidRow(
                              column(12,
                                     bs4Card(inputId=ns('null_box1'), 
                                             title ='Functional Classification Attribute',  
                                             width = 12, 
                                             solidHeader = T, 
                                             status='warning',
                                             collapsible = F, 
                                             closable = F,
                                         uiOutput(ns("handle_funclass_nulls_if_exists"))
                                     ))
                              ),
                            br(),
                            fluidRow(
                              col_12(
                                     actionButton(ns("back_3"), 
                                                  label = "Go Back",
                                                  class = "btn btn-primary"
                                     ),
                                     actionButton(ns("page_5"), 
                                                  label = "Submit Roads",
                                                  class = "btn btn-primary"
                                     ),
                                       uiOutput(ns('thank_you_local_rds'), class='leaflet_none')
                                     )))
                   ) # end of second slide
                   )), 
      div(id=ns('roads_choice_tiger'), class='leaflet_none',
          bs4Card(inputId=ns('roads_upload_6'), title='Confirm OSM Data', width = 12, closable=F,
          fluidRow(
            column(12, 
                   uiOutput(ns('thank_you_tiger'), class='leaflet_output')
             ))))
  )
}
    
#' manage_roads_upload Server Function
#'
#' @noRd 
mod_manage_roads_upload_server <- function(input, output, session, connection, user_id, run_id){
  ns <- session$ns
 
  
  
  ############################################################################## Create variables   
  
  data <- reactiveValues(
    the_roads=NULL, 
    road_medroid=NULL, 
    roads_bbox=NULL,
    roads_utm=NULL,
    table=return_table_name('roads', user_id, run_id),
    table_sql_formatted=DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id)),
    sql_literal_model_outputs_schema = DBI::dbQuoteLiteral(connection, 'model_outputs'),
    sql_literal_sliding_windows_outputs_schema = DBI::dbQuoteLiteral(connection, 'sliding_windows_output'),
    sql_literal_model_results = DBI::dbQuoteLiteral(connection, glue::glue('hin_output_roads_{user_id}_{run_id}')),
    sql_literal_sliding_windows = DBI::dbQuoteLiteral(connection, glue::glue('sw_sliding_windows_{user_id}_{run_id}')),
    run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
    fun_class_count=0,
    table_empty=data.frame("Your Dataset's Functional Class Value" = "Select a variable",  "Standard Functional Class Values"= "Select a variable"),
    fun_class=c("Expressway", "Major Arterial", "Minor Arterial", "Major Collector",  "Minor Collector", "Local Road", "Omit From Analysis"),
    fun_class_reactive=NULL,
    number_func_class_nulls=0,
    road_name=NULL,
    sa_table_sql_formatted=DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id)),
    sql_literal_local_data_schema = DBI::dbQuoteLiteral(connection, 'local_user_data'),
    sql_literal_crashes = DBI::dbQuoteLiteral(connection, glue::glue('crashes_{user_id}_{run_id}')),
    re_exists=NULL,
    cd_exists=NULL,
    fclass_colors=c('#1f78b4', '#33a02c', '#b2df8a', '#fb9a99', '#fdbf6f', '#a6cee3'),
    w=NULL,
    msgs=NULL
  )
  
  
  source(file.path(getwd(), 'inst', 'app', 'www', 'road_mod_objects.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'handle_nulls.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'send_local_data.R'), local = TRUE)
 
  # test to see if user has model running if so, they're blocked we don't want mismatched data during the model production phase 
  # The crash data depends on the roads data from functional class, so if that data exists, crashes need to be deleted before uploading new data 
  # if model results or sliding windows results are present, those table need to be deleted to move forward 
  observeEvent(input$select_roads_data_choice, {
    model_status <- model_production_status(connection, user_id, run_id)
    if (model_status == 'model_needed_running' || model_status == 'model_currently_running' ) {
      shiny_warming_alert(title = 'Whoa!', text='Your model is processing! You can\'t upload new data right now.')
    } else if (model_status == 'model_needed') {
      shiny_warming_alert(title = 'Whoa!', text='You\'re in line for model production! You can\'t upload new data right now.')
    } else {
      data$re_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results)
      data$cd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sliding_windows_outputs_schema, table=data$sql_literal_sliding_windows)
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_data_schema, table=data$sql_literal_crashes)
      
      if ( data$re_exists || data$cd_exists  || data$cr_exists ) {
        showModal(delete_analysis_results_rd)
      } else {
        rds_proceed(data=data, connection=connection, output=output, session=session, input=input, user_id=user_id, run_id=run_id)
      }
    }
  })
  
  # if user opts to delete thier data to upload new roads data 
  observeEvent(input$delete_data_rd, {
    tryCatch({
      removeModal()
      delete_tables(connection, list_of_tables=list(c('model_outputs', glue::glue('hin_output_roads_{user_id}_{run_id}')), 
                                                    c('model_output_scratch', glue::glue('hin_sliding_windows_{user_id}_{run_id}')), 
                                                    c('sliding_windows_outputs', glue::glue('sw_sliding_windows_{user_id}_{run_id}')), 
                                                    c('model_output_scratch', glue::glue('hin_crashes_{user_id}_{run_id}')),
                                                    c('local_user_data', glue::glue('crashes_{user_id}_{run_id}'))
      ))
      shiny_warming_alert(title='Results Deleted', text="Proceeding", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
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
      
      # process roads data
      rds_proceed(data=data, connection=connection, output=output, session=session, input=input, user_id=user_id, run_id=run_id)
      
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
  
  # removes model if user doesn't want to delete data 
  observeEvent(input$do_not_delete_data_rd, {
    removeModal()
  })
  
  observeEvent(input$back_to_source_selector_local, {
    reset('roads_shp')
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").addClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").addClass("leaflet_none");'))
  })
  
  observeEvent(input$back_to_source_selector_tiger, {
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").addClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_tiger'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_tiger'), '").addClass("leaflet_none");'))
  })
  
  observeEvent(input$model_roads_pro_format, {
    showModal(properly_formatted_roads_instructions)
  })
  
  observeEvent(input$model_roads_instructions, {
    showModal(roads_instructions_modal)
  })
  
  observeEvent(input$ok, {
    removeModal()
  })

  # the following onclicks manage the cards for each step of the upload process.   
  # moves roads slides back to step 1 from step 2
  onclick("back_1", {
    reset('roads_shp')
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides1'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides1'), '").addClass("leaflet_block");'))
  })
  
  # moves roads slides back to step 2 from step 3
  onclick("back_2", {
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").addClass("leaflet_block");'))
  })
  
  # moves roads slides back to step 2 from step 3
  onclick("back_3", {
    # shinyjs::runjs(code = paste0('$("#', session$ns('roads_map'), '").trigger("shown");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides4'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides4'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").addClass("leaflet_block");'))
  })

  ############################################################################## process roads  
  # This is the main function that takes and receives the users loaded roads data 
  # A few things happen when the user submits their data by clicking load roads data, including: 
  # A) the app checks to make sure the user has input data 
  # B) the app checks to ensure the data are polylines 
  # C) converts the data into WGS84, epsg, 4326
  # D) moves onto variables selection 
  # note) a tryCatch is setup for any errors that map occur. The error function includes the error message in a modal for the user to reivew. 
  observeEvent(input$roads_shp, {
    tryCatch({
      if (input$roads_data == 'shp') {
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
        preload_local_data_shp_roads(data=data, input=input, session=session, connection=connection, user_id=user_id, run_id=run_id)
        waiter::waiter_hide()
      }
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
  
  ########################################################################################
  ###### LOCAL Data - Fun Class Variable Process
  ########################################################################################  
  observe( {
    isolate({
      DT::reloadData(proxy_table, data$fun_class_reactive)
    })
  })
  
  ########################################################################################
  ###### LOCAL Data - Handle unwanted conditions from the selected variables to map 
  ########################################################################################  
  observeEvent(input$page_3, {
    tryCatch({
    road_variable_selection_warnings(data=data, input=input, session=session, output=output)
      }, error = function(cond){
        c <- toString(cond)
        shiny_warming_alert(title = 'Something Went Wrong.', text=c)
      })
  })
  
  ########################################################################################
  ###### LOCAL Data - Handle NULLs/NAs
  ########################################################################################  
  observeEvent(input$page_4, {
    if( handle_road_nulls(data=data, input=input, session=session, output=output) ){
      tryCatch({
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides4'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides4'), '").addClass("leaflet_block");'))
      data$the_roads <- map_roads(data=data, session=session, input=input)
      }, error = function(cond){
        c <- toString(cond)
        shiny_warming_alert(title = 'Something Went Wrong.', text=c)
      })
    }
  })
  
  ########################################################################################
  ###### LOCAL Data - Fun Class Variable Process
  ########################################################################################  
 
  # upload local roads data 
  observeEvent( input$page_5, {
    tryCatch({
    start_time <- Sys.time()
    data$w <- waiter::Waiter$new(html = tagList(
      tags$div(waiter::spin_1()),
      tags$br(),
      tags$div(HTML("Getting setup to load your data.."))
    ),  
    color='rgba(175, 175, 175, 0.85)')
    data$msgs <- c("Uploading your roads data...", "Finishing up...")
    data$w$show()
    # create empty dataframe to accept the selected inputs from the user 
    fclassf <- data.frame()
    
    #creates a table of equal length as the number of selected inputs (the selectinputs are in each row of this reactive object)
    fclass_table <- as.data.frame(data$fun_class_reactive)
    
    # loops equal to the number of rows and binds the var id = $(#id).val() with Shiny.setInputValue('input', id)
    # at the same time, this loop populates the fclassf data.frame with the user's selections in the same order as the rows 
    for (i in 1:nrow(fclass_table)) {
      shinyjs::runjs(code = paste0('var sel', i, ' = $("#', session$ns(paste0("sel", i)), '").val(); Shiny.setInputValue("', session$ns(paste0("sel", i)),'", ', paste0("sel", i), ', {priority: \"event\"});'))
      df <- data.frame(input[[paste0('sel', i)]])
      fclassf <- rbind(fclassf,df)
    }
    
    # this part is populates a new column on the spatial_data_frame with the mapped values the user selected 
    for (i in 1:nrow(fclass_table)) {
      data$the_roads[['usdot_fun_class_mapped']][data$the_roads[[input$func_class_variable]] == fclass_table[i,1]] <- fclassf[i,]
    }
    
    if ( !is.null(data$number_func_class_nulls) && data$number_func_class_nulls > 0 ) {
      data$the_roads[['usdot_fun_class_mapped']][is.na(data$the_roads[["usdot_fun_class_mapped"]])] <- toString(input$fun_class_nulls_map)
      data$the_roads[['usdot_fun_class_mapped']][is.null(data$the_roads[["usdot_fun_class_mapped"]])] <- toString(input$fun_class_nulls_map)
    }
    
    print('Sending data as wkt')

    data$w$update(html = tagList(
      tags$div(waiter::spin_1()),
      tags$br(),
      tags$div(HTML(data$msgs[1]))
    ))
    
    long_job_roads_to_postgresql(connection=connection, 
                                 user_id=user_id,
                                 run_id=run_id,
                                 roads_utm=data$roads_utm,
                                 table=data$table, 
                                 schema='local_user_data',
                                 geodata=data$the_roads,
                                 promote_to_multi=T,
                                 geom_type='MULTILINESTRING')
 
    print("Recording identified columns and source information")
    
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_name'), new_value=DBI::dbQuoteLiteral(connection, input$road_name))
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'roads_fun_c_col'), new_value=DBI::dbQuoteLiteral(connection, input$func_class_variable))
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_source'), new_value=DBI::dbQuoteString(connection, glue::glue('Local roads data from user. Submitted {Sys.time()}')))
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'road_o_id'), new_value=DBI::dbQuoteLiteral(connection, input$pri_key_roads))
    
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").addClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_local'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(3) > a\').click(); ')  
    end_time <- Sys.time()
    print(paste0('Time to load roads: ', end_time-start_time))
    data$w$hide()

    print('Done loading roads')
    
    }, error = function(cond){
      c <- toString(cond)
      data$w$hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
  
  observeEvent(input$submit_roads_button_tiger, {
    tryCatch({
    waiter::waiter_show(
      color='rgba(175, 175, 175, 0.85)',
      html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML("Building roads data ..."))
      )
    )
      create_account_tiger(connection, user_id, run_id)
      shiny_warming_alert(title='Confirmed', text="OSM roads data has been submitted for analysis.", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_tiger'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('roads_choice_tiger'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(3) > a\').click(); ')  
      waiter::waiter_hide()
    }, error = function(cond){
      c <- toString(cond)
      waiter::waiter_hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })
}
    
## To be copied in the UI
# mod_manage_roads_upload_ui("manage_roads_upload_ui_1")
    
## To be copied in the server
# callModule(mod_manage_roads_upload_server, "manage_roads_upload_ui_1")
 
