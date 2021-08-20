#' manage_crash_upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_manage_crash_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    bs4Card(inputId=ns('intro_card'), title='Introduction', width = 12, closable = F,
            tags$ul(
              tags$li("You can select nationally available FARS data or upload your own local crash data."),
              tags$li("Using FARS data may produce unexpected or unhelpful results. If you have access to crash data containing more than just fatal crashes, we recommend you use it. Because the default FARS data only includes fatalities, it is not ideal for the purposes of the analyses within the HRSF. If you have access to any kind of geocoded crash data, we strongly recommend using this data, even if it requires some time on the front end to clean this data up to meet the data requirements listed above."),
              tags$li("If uploading your own crash data, you’ll need to format your data for the tool. Find more information on formatting your data by clicking on the buttons below. It may take several minutes to process your data upon upload, depending on the number of crashes."),
              tags$li(HTML("<a href=\"https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars\" target=\"_blank\" style=\"color: #007bff;\">Click here</a> to learn more about FARS data.")),
            ),
            actionButton(ns("modal_crash_instructions"), label = 'Instructions', class = 'btn btn-primary'),
            actionButton(ns("modal_crash_pro_format"), label = 'Properly Formatted Crash Data', class = 'btn btn-primary')
    ),
    div(id=ns('crash_choice'), 
        bs4Card(inputId=ns('crs_source_selector'), title='Select a source for your crash data', width = 12, closable = F,
        radioGroupButtons(inputId = ns("crash_choice_selector"), label = "Make a choice:",
                          # choices = c("Local Crash Data" = "local_crash_data"),
                          choices = c("FARS Crash Data (2015 - 2019)" = "fars_data", "Local Crash Data" = "local_crash_data"),
                          justified = TRUE, 
                          width = '600',
                          selected = 'local_crash_data',
                          individual = TRUE,
                          checkIcon = list(
                            yes = tags$i(class = "fa fa-circle", 
                                         style = "color: steelblue"),
                            no = tags$i(class = "fa fa-circle-o", 
                                        style = "color: steelblue"))
        ),

        actionButton(ns("select_crash_data_choice"), label = 'Select this crash data', class = "btn btn-primary")
    )),
    tags$div(id = ns('crashes_choose_local'), class='leaflet_none',
             tags$div(id = ns('cont_crash_slideshow'), class="crash_slideshow-container", checked=NA, 
                      #####################################################
                      #####First Slide, User select data type and loads data
                      tags$div(id = ns("crash_slides1"), class="leaflet_block", checked=NA,  
                               bs4Card(inputId=ns('crash_upload_1'), title='Upload Your Local Crash Data', width = 12, closable = F,
                                       fluidRow(
                                         column(12,
                                                radioGroupButtons(inputId = ns("crash_data"), 
                                                                  label = "Make a choice :",
                                                                  choices = c("Zipped Shapfile" = "shp"),
                                                                  justified = TRUE, status = "primary",
                                                                  width = '300',
                                                                  checkIcon = list(yes = icon("dot-circle-o"), no = icon("times-circle-o"))
                                                ),
                                                p("Please upload a shapefile in a single zipped file. Please include the mandatory file extensions needed for a shapefile: .shp, .shx, .dbf, and .proj. The maximum allowable number of crashes is 150,000. If your data exceeds this limit, you will need to clip the data prior to uploading it. Consider excluding features prior to upload if you experience latency."),
                                                conditionalPanel(condition = "input.crash_data == 'shp'", ns = ns, 
                                                                 fileInput(inputId = ns("crash_data_shp"), 
                                                                           multiple = FALSE, 
                                                                           label = "Upload a .zip file containing a shapefile", 
                                                                           accept = '.zip'),
                                                                 ),
                                                       
                                         )),
                                       fluidRow(
                                         column(12,
                                                actionButton(ns("back_to_start_local"), 
                                                             label = 'Back to Crash Source Selection',
                                                             class = "btn btn-primary"),   
                                         )))
                      ), # end of first slide
                      
                      ##########################################################################
                      ############################## Second Slide: User selects lat long inputs (if csv) and severity measures 
                      div(id = ns("crash_slides2"), class="leaflet_none", checked=NA,  
                          bs4Card(inputId=ns('crash_upload_2'), title='Attribute Selection', width = 12, closable = F,
                                  p('Please select the severity, mode, year, and crash report ID attributes from your crash data.'),
                                  
                          fluidRow(
                            column(6,
 
                                     bs4Card(inputId=ns('var_sel1'), 
                                             title = "Select the Crash Severity Attribute",  
                                             width = 12, 
                                             collapsible = F, 
                                             closable = F, 
                                             solidHeader = T, 
                                             status = 'info', 
                                         helpText('Select the attribute associated with the highest severity of the crash (i.e., Fatality, Incapacitating Injury, etc).'),
                                         selectInput(ns('crash_sev_variable'), label='Crash Severity', choices='Please upload your crash data'),
                                         uiOutput(ns("crash_sev_variable_notice_1"))
                                   ),
                                   bs4Card(inputId=ns('var_sel4'), 
                                           title = "Select the Report ID Attribute", 
                                           width = 12, 
                                           collapsible = F, 
                                           closable = F, 
                                           solidHeader = T, 
                                           status = 'info', 
                                           helpText('This must be unique for each crash.'),
                                           selectInput(ns('reportid_variable'), label='Report ID', choices='Please upload your crash data')
                                           
                                   )
                                   ),
                            column(6,
                                   bs4Card(inputId=ns('var_sel2'), 
                                           title = "Select the Mode Attribute", 
                                           width = 12, 
                                           collapsible = F, 
                                           closable = F,
                                           solidHeader = T, 
                                           status = 'info', 
                                           helpText('Select the attribute associated with the crash mode (i.e., Pedestrian, Bicyclist, Motor Vehicle).'),
                                           selectInput(ns('mode_variable'), label='Crash Mode', choices='Please upload your crash data'),
                                           uiOutput(ns("mode_variable_notice_1"))
                                   ),
                                   bs4Card(inputId=ns('var_sel3'), 
                                           title = "Select the attribute associated with the year", 
                                           width = 12, 
                                           collapsible = F, 
                                           closable = F, 
                                           solidHeader = T, 
                                           status = 'info',
                                           helpText('Select the attribute associated with the crash year. This must be a four-digit integer value.'),
                                           selectInput(ns('year_variable'), label='Year Attribute', choices='Please upload your crash data'),
                                           uiOutput(ns("year_variable_notice_1")),
                                           uiOutput(ns("year_variable_notice_2")),
                                           uiOutput(ns("year_variable_notice_3"))
                                           
                                   )
                                   )),
                          conditionalPanel(condition = "input.crash_data == 'csv'", ns = ns,
                                           uiOutput(ns("notice_1")),
                                           uiOutput(ns("select_lat_ui")),
                                           fluidRow(
                                             column(3,
                                                    uiOutput(ns("select_longs_ui"))),
                                             column(9,
                                                    uiOutput(ns("select_latlong_button_ui")))),
                          ),
                          br(),
                          fluidRow(
                            column(12,
                                   actionButton(ns("back_1"), 
                                                label = "Go Back",
                                                class = "btn btn-primary prev"
                                   ),
                                   actionButton(ns("page_3"), 
                                                label = "Next",
                                                class = "btn btn-primary next"
                                   )
                            ))
                          )
                      ),# end of second slide
                      
                      ##########################################################
                      ############ Step three, map severity and mode, and confirm crash costs
                      tags$div(id = ns("crash_slides3"), class="leaflet_none", checked=NA, 
                               bs4Card(inputId=ns('crash_upload_3'), title='Map Variables, Mode and Severity', width = 12, closable = FALSE,
                               p("Please indicate how the severity, mode, report ID, and year attributes from your crash data correspond to standard values."),
                               fluidRow(
                                 column(12,
                                        DT::dataTableOutput(ns('cr_severity_map_table'),  width = 600),
                                        br()
                                 )),
                               fluidRow(
                                 column(12,
                                        DT::dataTableOutput(ns('cr_mode_map_table'),  width = 600),
                                        br()
                                 )),
                               fluidRow(
                                 column(12,
                                        actionButton(ns("back_2"), 
                                                     label = "Go Back",
                                                     class = "btn btn-primary prev"
                                        ),
                                        actionButton(ns("page_4"), 
                                                     label = "Next",
                                                     class = "btn btn-primary next"
                                        ))
                                 ))), 
                      ##########################################################
                      ####### Step 4: Identify how you'd like to handle NULLs and NAs
                       tags$div(id = ns("crash_slides4"), class="leaflet_none", checked=NA, 
                                bs4Card(inputId=ns('crash_upload_3'), title='Select Crash Costs', width = 12, closable = FALSE,
                                 fluidRow(
                                   column(12, 
                                          tags$div(HTML('<div>The default costs reflect NHTSA’s 2015 person-injury unit costs, which were updated to crash unit costs expressed in 2020 values, following guidance set forth in Chapter 6 of FHWA\'s <a href="https://safety.fhwa.dot.gov/hsip/docs/fhwasa17071.pdf" target="_blank" style=\"color: #007bff;\">Crash Costs for Highway Safety Analysis (2018)</a>. These costs are also discounted with a rate of 3% to reflect today\'s value of costs projected over a five year time horizon.</div>')),
                                          actionButton(ns('update_defaults'), 'Update Default Crash Costs'),
                                          actionButton(ns("modal_crash_costs_format"), label = 'More Information', class = 'btn btn-primary')
                                   )),
                                 fluidRow(
                                   column(12,
                                          uiOutput(ns("discount_rate")),
                                          DT::dataTableOutput(ns('cr_cost_map_table'),  width = 600),
                                          br()
                                   )),
                                 br(),
                                 fluidRow(
                                   col_12(
                                          actionButton(ns("back_3"), 
                                                       label = "Go Back",
                                                       class = "btn btn-primary next"
                                          ),
                                          actionButton(ns("page_5"), 
                                                       label = "Next",
                                                       class = "btn btn-primary next"
                                          )
                                          ))
                                      )
                                 ),
                      tags$div(id = ns("crash_slides5"), class="leaflet_none", checked=NA, 
                      bs4Card(inputId=ns('crash_upload_5'), title='Identify how you\'d like to handle NULLs and NAs', width = 12, closable = FALSE,
                              helpText("Confirm how you'd like to handle NULL or NA values in your crash data, which will need to be omitted in most cases.  If you have local knowledge that most or all of the null or unknown severity and mode values are usually related to a specific type of crash, you may assign them to the severity or mode that best fits local conditions."),
                              
                              tags$div(br()),
                              
                              fluidRow(col_12(
                                bs4Card(inputId=ns('null_box1'), 
                                        title ='Crash Report ID',  
                                        status='warning',
                                        solidHeader = T, 
                                        width = 12, 
                                        collapsible = F, 
                                        closable = F,
                                        uiOutput(ns("handle_reportid_nulls_if_exists"))
                                ))
                              ),
                              fluidRow(col_12(
                                bs4Card(inputId=ns('null_box2'), 
                                        title ='Crash Severity',  
                                        status='warning',
                                        solidHeader = T, 
                                        width = 12, 
                                        collapsible = F, 
                                        closable = F,
                                        uiOutput(ns("handle_severity_nulls_if_exists"))
                                ))
                              ),
                              fluidRow(col_12(
                                bs4Card(inputId=ns('null_box3'), 
                                        title ='Crash Mode',  
                                        width = 12, 
                                        solidHeader = T, 
                                        status='warning',
                                        collapsible = F, 
                                        closable = F,
                                        uiOutput(ns("handle_mode_nulls_if_exists"))
                                ))
                              ),
                              fluidRow(col_12(
                                bs4Card(inputId=ns('null_box4'), 
                                        title ='Crash Year',  
                                        width = 12, 
                                        solidHeader = T, 
                                        status='warning',
                                        collapsible = F, 
                                        closable = F,
                                        uiOutput(ns("handle_year_nulls_if_exists"))
                                ))
                              ),
                              br(),
                              fluidRow(
                                col_12(
                                  actionButton(ns("back_4"), 
                                               label = "Go Back",
                                               class = "btn btn-primary next"
                                  ),
                                  actionButton(ns("page_6"), 
                                               label = "Submit Crashes",
                                               class = "btn btn-primary next"
                                  )
                                ))
                      
                              
                              )
                      
                      
                      
                           )
                       )),
    tags$div(id = ns('crashes_fars'), class='leaflet_none',
    fluidRow(
      column(12, 
             fluidRow(
               column(12, 
                      actionButton(ns("back_to_start_fars"), 
                                   label = 'Back to Crash Source Selection',
                                   class = "btn btn-primary"),   
                      actionButton(ns("fars_submit_county_button"), label = 'Submit Crashes', class = 'hide btn btn-success')
                      
               ))
      ))
    )
    )
}
    
#' manage_crash_upload Server Function
#'
#' @noRd 
mod_manage_crash_upload_server <- function(input, output, session, connection, user_id, run_id){

  ns <- session$ns

    data <- reactiveValues(
    crashes=NULL, 
    crashes_bbox=NULL,
    crashes_utm=NULL,
    table=return_table_name('crashes', user_id, run_id),
    table_sql_formatted=DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
    sa_table_sql_formatted=DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id)),
    sql_literal_model_outputs_schema = DBI::dbQuoteLiteral(connection, 'model_outputs'),
    sql_literal_sliding_windows_outputs_schema = DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs'),
    sql_literal_model_results = DBI::dbQuoteLiteral(connection, glue::glue('hin_output_roads_{user_id}_{run_id}')),
    sql_literal_sliding_windows = DBI::dbQuoteLiteral(connection, glue::glue('sw_sliding_windows_{user_id}_{run_id}')),
    sql_literal_local_data = DBI::dbQuoteLiteral(connection, 'local_user_data'),
    sql_literal_roads_table = DBI::dbQuoteLiteral(connection, glue::glue('roads_{user_id}_{run_id}')),
    sql_literal_sa_table = DBI::dbQuoteLiteral(connection, glue::glue('study_area_{user_id}_{run_id}')),
    run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
    number_year_nulls=NULL,
    number_report_id_nulls=NULL, 
    number_mode_nulls=NULL,
    number_severiy_nulls=NULL,
    sev_reactive=NULL,
    sev_levels=c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)",  "Possible Injury (C)", "Property Damage Only (O)", "Omit From Analysis"),
    mode_reactive=NULL,
    mode_levels=c("Pedestrian Crash", "Bicycle Crash", "Other Crash", "Omit From Analysis"),
    table_empty=data.frame("Your Dataset's Severity Value" = "Standard Severity Value",  "Your Datset's Mode Value"= "Standard Mode Value"),
    kabco_costs=NULL,
    default_kabco = default_kabco,
    default_kabco_costs = default_kabco[,2],
    mode_count=0,
    sev_id_count=0,
    re_exists=F,
    cd_exists=F,
    rds_exists=F,
    sa=NULL,
    crash_colors=c('#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#f16913'),
    crash_col_levs = c('Property Damage Only (O)', 'Possible Injury (C)', 'Non-Incapacitating Injury (B)', 'Incapacitating Injury (A)', 'Fatality (K)'),
    w=NULL,
    msgs=NULL
  )
    
    # attempts to fix odd ui condition 
    shinyjs::runjs(code = paste0('$("#load_data_ui_1-data_explorer_tab_select > li:nth-child(2) > a").click(function(){$("#', session$ns('cr_mode_map_table'), '").trigger("shown");})'))

    source(file.path(getwd(), 'inst', 'app', 'www', 'crash_mod_objects.R'), local = TRUE)
    source(file.path(getwd(), 'inst', 'app', 'www', 'handle_nulls.R'), local = TRUE)
    source(file.path(getwd(), 'inst', 'app', 'www', 'send_local_data.R'), local = TRUE)

    # this observer will begin process crash data if the user does not have a model currently running     
    observeEvent(input$select_crash_data_choice, {
        model_status <- model_production_status(connection, user_id, run_id)
        if (model_status == 'model_needed_running' || model_status == 'model_currently_running' ) {
          shiny_warming_alert(title = 'Whoa!', text='Your model is processing! You can\'t upload new data right now.')
        } else if (model_status == 'model_needed') {
          shiny_warming_alert(title = 'Whoa!', text='You\'re in line for model production! You can\'t upload new data right now.')
        } else {
          data$re_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results)
          data$cd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sliding_windows_outputs_schema, table=data$sql_literal_sliding_windows)
          if ( data$re_exists || data$cd_exists ) {
            showModal(delete_analysis_results_cr)
          } else {
            cr_proceed(data=data, connection=connection, output=output, session=session, input=input, user_id=user_id, run_id=run_id) 
          }
        }
      })
    
    # deletes tables model results if user wants to upload new crash data. 
    observeEvent(input$delete_data_cr, {
      removeModal()
      delete_tables(connection, list_of_tables=list(c('model_outputs', glue::glue('hin_output_roads_{user_id}_{run_id}')), c('model_outputs', glue::glue('hin_sliding_windows_{user_id}_{run_id}')), c('sliding_windows_outputs', glue::glue('sw_sliding_windows_{user_id}_{run_id}')), c('model_outputs', glue::glue('hin_crashes_{user_id}_{run_id}'))))
      shiny_warming_alert(title='Results Deleted', text="Proceeding", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_since_model_desired'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_status'), new_value=DBI::dbQuoteLiteral(connection, 'no_model_desired'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_process_time'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_mode_finished'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'move_windows_long_comp'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_comp'), new_value='NULL')
      cr_proceed(data=data, connection=connection, output=output, session=session, input=input, user_id=user_id, run_id=run_id) 
    })
    
    
    observeEvent(input$do_not_delete_data_cr, {
       removeModal()
    })
  
    observeEvent(input$back_to_start_local, {
      reset('crash_data_shp')
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").addClass("leaflet_none");'))
    })
    
    observeEvent(input$back_to_start_fars, {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crashes_fars'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crashes_fars'), '").addClass("leaflet_none");'))
    })
    
    
########################################################################################
###### Page On Click Event Handlers  
########################################################################################  
    
  onclick("back_1", {
    reset('crash_data_shp')
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides1'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides1'), '").addClass("leaflet_block");'))
  })
 
  onclick("back_2", {
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").addClass("leaflet_block");'))
  })

  onclick("back_3", {
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").addClass("leaflet_block");'))
  })
  
  onclick("back_4", {
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides5'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides5'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").addClass("leaflet_block");'))
  })
 
  

  ########################################################################################
  ###### LOCAL Data - Upload Button  
  ########################################################################################  
    # handle load of crash data
    observeEvent(input$crash_data_shp, {
      tryCatch({
      if (input$crash_data == 'shp') {
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
        preload_local_data_shp_crashes(data=data, input=input, session=session, connection=connection, user_id=user_id, run_id=run_id)
        waiter::waiter_hide()
      }
      }, error = function(cond){
        c <- toString(cond)
        shiny_warming_alert(title = 'Something Went Wrong.', text=c)
      })
    })
    
  ########################################################################################
  ###### LOCAL Data - Severity Variable Process
  ########################################################################################  
    
  # Observe differences in the data$sev_reactive reactive and repopulate datatable.
  # 1. new/old datatable need to have the same number of columns 
  # 2. data needs to be managed server side (server = true in DT::renderDataTable)
  observeEvent(input$crash_sev_variable, {
    isolate({
      DT::reloadData(proxy_table_cr, data$sev_reactive)
    })
  })
  
  ########################################################################################
  ###### LOCAL Data - Mode Variable Process
  ########################################################################################  
  observeEvent(input$mode_variable, {
    data$mode_reactive
    isolate({
      DT::reloadData(proxy_table_cr_mode, data$mode_reactive)
    })
  })
  
  ########################################################################################
  ###### LOCAL Data - Crash Cost Variable Process
  ########################################################################################  
  # Observe differences in the data$kabco_costs reactive and repopulate datatable.
  # Hides the button to update the table onclick. Default values are supplied in case the user wants to keep the default values. 
  observeEvent(input$update_defaults, {
    tryCatch({
    kabco_costs()
    data$kabco_costs <- kabco_costs()
     
     DT::reloadData(proxy_table_cr_costs, isolate(data$kabco_costs))
     shinyjs::runjs(code = paste0('$("#', session$ns('update_defaults'), '").removeClass("leaflet_block");'))
     shinyjs::runjs(code = paste0('$("#', session$ns('update_defaults'), '").addClass("leaflet_none");'))
     output$discount_rate <- renderUI({
       output = tagList()
       output[['c_d_rate']] = numericInput(ns("custom_discount_rate_value"), "Apply Custom Discount Rate:", .03, min = 0, max = 10, step = .01)
       output 
     })
     data$crashes <- map_crashes(data=data, session=session, input=input, discount_rate=input$discount_rate)
     runjs(paste0('$("#', session$ns('cr_cost_map_table'), '").trigger("shown");'))
    }, error = function(cond){
      c <- toString(cond)
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    }) 
  })
 
  ########################################################################################
  ###### LOCAL Data - Flags any problems with the variables selected for mapping
  ########################################################################################  
  observeEvent(input$page_3, {
    tryCatch({
      crash_variables_selection_warnings(data=data, input=input, session=session, output=output)
    }, error = function(cond){
      c <- toString(cond)
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    }) 
  })

  ########################################################################################
  ###### LOCAL Data - Handle Nulls Across Important Attributes
  ########################################################################################  
  observeEvent(input$page_4, {
  if(handle_crash_nulls(data=data, session=session, input=input, output=output)){
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").addClass("leaflet_block");'))
    } else {
      shiny_warming_alert(title='Something went wrong.', text="There\'s an issue with the nulls or NAs in your data. Please contact the developers if this problem persists.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
    }
  })
 
  ########################################################################################
  ###### LOCAL Data - Instantiate shiny bindings
  ########################################################################################  
  observeEvent(input$page_5, {
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides4'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides5'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides5'), '").addClass("leaflet_block");'))
    data$crashes <- map_crashes(data=data, session=session, input=input, discount_rate=input$discount_rate)
  })
  
  ########################################################################################
  ###### LOCAL Data - Intended to send data to the database, various checks are performed before sending
  ########################################################################################  
 observeEvent( input$page_6, {
    tryCatch({
      start_time <- Sys.time()
      # submit local crash data. This runs the function from fct_submit_local_data.R.  
      if (!is.null(input$custom_discount_rate_value)){
        discount_rate <- input$custom_discount_rate_value
      } else {
        discount_rate <- .03
      }
 
      data$w <- waiter::Waiter$new(html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML("Setting up to load your crash data..."))
      ),  
      color='rgba(175, 175, 175, 0.85)')
      data$msgs <- c("Uploading your crash data...", "Finishing up...")
      data$w$show()
      
      if ( !is.null(data$number_mode_nulls) && data$number_mode_nulls > 0) {
        data$crashes[['usdot_mode_mapped']][is.na(data$crashes[[input$mode_variable]])] <- toString(input$handle_mode_nulls_if_exists_radio)
        data$crashes[['usdot_mode_mapped']][is.null(data$crashes[[input$mode_variable]])] <- toString(input$handle_mode_nulls_if_exists_radio)
      }
      
      if ( !is.null(data$number_severiy_nulls) && data$number_severiy_nulls > 0 ) {
        data$crashes[['severity_mapped']][is.na(data$crashes[[input$crash_sev_variable]])] <- toString(input$handle_severity_nulls_if_exists_radio)
        data$crashes[['severity_mapped']][is.null(data$crashes[[input$crash_sev_variable]])] <- toString(input$handle_severity_nulls_if_exists_radio)
      }
      
      values <-c()
      sev_final_holding <- data.frame()
      kcosts_table <- as.data.frame(data$kabco_costs)
      sev_table <- as.data.frame(data$sev_reactive)
      for (i in 1:nrow(sev_table)) {
        shinyjs::runjs(code = paste0('var sev', i, ' = $("#', session$ns(paste0("sev", i)), '").val(); console.log(', paste0("sev", i), '); Shiny.setInputValue("', session$ns(paste0("sev", i)),'", ', paste0("sev", i), ', {priority: \"event\"});'))
        df <- data.frame(input[[paste0('sev', i)]])
        sev_final_holding <- rbind(sev_final_holding, df)
      }
      
      for (i in 1:nrow(sev_table)) {

        data$crashes$severity_mapped[data$crashes[[input$crash_sev_variable]] == sev_table[i,1]] <- sev_final_holding[i,]
        df <- which(kcosts_table[['KABCO Value']] == sev_final_holding[i,])
        shinyjs::runjs(code = paste0('var kab', df, ' = $("#', session$ns(paste0("kab", df)), '").val(); console.log(', paste0("kab", df), '); Shiny.setInputValue("', session$ns(paste0("kab", df)),'", ', paste0("kab", df), ', {priority: \"event\"});'))
        values <- append(values, input[[paste0('kab', df)]])
 
        
        if (sev_final_holding[i,] == 'Fatality (K)') {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'o_k_cost'), new_value=input[[paste0('kab', df)]])
        }
        if (sev_final_holding[i,] == 'Incapacitating Injury (A)') {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'o_a_cost'), new_value=input[[paste0('kab', df)]])
        }
        if (sev_final_holding[i,] == 'Non-Incapacitating Injury (B)') {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'o_b_cost'), new_value=input[[paste0('kab', df)]])
        }
        if (sev_final_holding[i,] == 'Possible Injury (C)') {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'o_c_cost'), new_value=input[[paste0('kab', df)]])
        }
        if (sev_final_holding[i,] == 'Property Damage Only (O)') {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'o_o_cost'), new_value=input[[paste0('kab', df)]])
        }
      }
      
      
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'discount_rate'), new_value=discount_rate)
      
      if (any(as.integer(values) > 250000000)) {
        shiny_warming_alert(title = 'Out of Range', text='Your crash costs are too high. Please use a crash cost less than or equal to $250 million.')
        data$w$hide()
        return(FALSE)
      } else {
        start_time <- Sys.time()
        data$crashes <- map_crashes(data=data, session=session, input=input, discount_rate=discount_rate) 
        print('Sending data as wkt')
        
        data$w$update(html = tagList(
          tags$div(waiter::spin_1()),
          tags$br(),
          tags$div(HTML(data$msgs[1]))
        ))
  
        long_job_crashes_to_postgresql (connection=connection, 
                                        user_id=user_id,
                                        run_id=run_id,
                                        crashes_utm=data$crashes_utm,
                                        table=data$table, 
                                        schema='local_user_data',
                                        geodata=data$crashes,
                                        promote_to_multi=T,
                                        geom_type='MULTIPOINT')
        

        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_rep_id_col'), new_value=DBI::dbQuoteLiteral(connection, input$reportid_variable))
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_source'), new_value=DBI::dbQuoteLiteral(connection, glue::glue('Local crash data from user. Submitted {Sys.time()}')))
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_year_col'), new_value=DBI::dbQuoteLiteral(connection, input$year_variable))
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_serv_col'), new_value=DBI::dbQuoteLiteral(connection, input$crash_sev_variable))
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_o_mode_col'), new_value=DBI::dbQuoteLiteral(connection, input$mode_variable))   
        
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_choice'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crashes_choose_local'), '").addClass("leaflet_none");'))
        
        shinyjs::runjs(code = ' $(\'#load_data_ui_1-data_explorer_tab_select > li:nth-child(4) > a\').click(); ')  
        
        data$w$hide()
        end_time <- Sys.time()
        print(paste0('Time to load crashes: ', end_time - start_time))
      }
    }, error = function(cond){
      c <- toString(cond)
      data$w$hide()
      shiny_warming_alert(title = 'Something Went Wrong.', text=c)
    })
  })

}
    
## To be copied in the UI
# mod_manage_crash_upload_ui("manage_crash_upload_ui_1")
    
## To be copied in the server
# callModule(mod_manage_crash_upload_server, "manage_crash_upload_ui_1")