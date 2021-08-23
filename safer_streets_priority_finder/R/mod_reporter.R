#' reporter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import bs4Dash
#' @import plotly
mod_reporter_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        h4('Exploratory Data Dashboard'),
        tags$div(hr())
      )),
    fluidRow(
      col_6(
        bs4Card(inputId=ns('explore1'), title ='Dive Into Your Data',  width = 12, collapsible = T, closable = F,
                p('Use the button below to refresh the descriptive statistics on this page. Refreshing the data can be useful if you’ve uploaded new data or performed an additional analysis.'), 
                p('Crashes that occurred outside your study area are not included in the summaries.' ),
                actionButton(ns("refresh_button"), "Refresh", class = "btn btn-primary"),
                actionButton(ns("refresh_button_hidden"), "Fetch Data", class = "btn btn-primary leaflet_none")
        )),
      col_6(
        bs4Card(inputId=ns('explore2'), 
                title ='Data Downloader',  
                width = 12, 
                collapsible = T, 
                closable = F, 
                solidHeader = T, 
                dropdownIcon = 'download',
                labelTooltip = 'Use this box to download your data.',
                status = 'warning',
                p('Download your input data, analysis results, or a PDF summary report of this dashboard page.'),
                fluidRow(
                  col_12(
                    actionButton(ns("download_instructions"), "Instructions", class = "btn btn-primary"),
                    actionButton(ns("create_report"), label = 'Build Report', class = "btn btn-primary"),
                    tags$div(id=ns("downloader_div"), 
                    downloadButton(ns('download_report'), label = "Download Report", class = 'btn btn-primary')
                    )
                  )
                ),
                fluidRow(
                  col_12(
                    uiOutput(ns("reporter_message"))
                  )
                ),
                hr(),
                selectInput(
                  inputId = ns("data_selector_for_download"),
                  label = "Select Data To Download",
                  choices = c("Study Area", "Road Network", "Crashes", "Sliding Windows Analysis", "Safer Streets Model", "Bike and Pedestrian Crashes", "Top Ten Pedestrian Crash Corridors", "Top Ten Bicycle Crash Corridors", "Top Ten Other Crash Corridors")
                ),
                fluidRow(
                  col_12(
                    downloadButton(ns('download_data'), label = "Download Data", class = 'btn btn-primary')
                  )
                )
        )) 
    ),
    fluidRow(
      col_3(
        bs4ValueBox(
          value = textOutput(ns('ped_crashes')),
          subtitle = 'Pedestrian Crashes',
          icon = 'users',
          elevation = NULL,
          status = 'danger',
          width = 12,
          footer = NULL,
          href = NULL
        )),
      col_3(
        bs4ValueBox(
          value = textOutput(ns('bike_crashes')),
          subtitle = 'Bicycle Crashes',
          icon = 'bicycle',
          elevation = NULL,
          status = 'success',
          width = 12,
          footer = NULL,
          href = NULL
        )),
      col_3(
        bs4ValueBox(
          value = textOutput(ns('other_crashes')),
          subtitle = 'Other Crashes',
          icon = 'bullseye',
          elevation = NULL,
          status = 'warning',
          width = 12,
          footer = NULL,
          href = NULL
        )),
      col_3(
        bs4ValueBox(
          value = textOutput(ns('total_analsis')),
          subtitle = 'Total Crashes in Analysis',
          icon = 'arrows',
          elevation = NULL,
          status = 'primary',
          width = 12,
          footer = NULL,
          href = NULL
        ))),
      fluidRow(
        col_4(
          bs4ValueBox(
            value = textOutput(ns('omitted_by_sev_mode')),
            subtitle = 'Omitted by Severity or Mode',
            icon = 'window-close-o',
            elevation = NULL,
            status = 'info',
            width = 12,
            footer = NULL,
            href = NULL
          )), 
 
        col_4(
          bs4ValueBox(
            value = textOutput(ns('omited_outside')),
            subtitle = 'Omitted, Outside Study Area',
            icon = 'window-close-o',
            elevation = NULL,
            status = 'light',
            width = 12,
            footer = NULL,
            href = NULL
          )),
        col_4(
          bs4ValueBox(
            value = textOutput(ns('total_crashes')),
            subtitle = 'Total Crashes',
            icon = 'arrows',
            elevation = NULL,
            status = '#2c6e49',
            width = 12,
            footer = NULL,
            href = NULL
          ))
        ) ,
    tags$div(id=ns('crash_reporters'), class='leaflet_none',
             
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore5'), title ='Crashes by Mode',  width = 12, collapsible = T, closable = F,
                         fluidRow(
                           col_3(
                             fluidRow(
                               col_12(
                                 selectInput(ns('chart2_selector'), choices = c('All Crashes', 'Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'), selected= 'All Crashes', label='Select Severity:')
                               )
                             ), 
                             fluidRow(
                               col_12(
                                 uiOutput(ns("slider_mode_ui"))
                               )
                             ), 
                             downloadButton(ns('download_cr_mode'),"Download Table")
                             ), 
                           col_9(
                           fluidRow(
                             col_12(
                               #p("Review crashes by mode in this section within your study area my mode. Use the dropdown menu to the left to select the mode you'd like to summarize. Only crashes that fall outside your study area are omitted from this analysis."),
                             )
                           ), 
                           fluidRow(
                             col_6(
                         
                                 DT::dataTableOutput(ns("explore5_table"))
                           
                             ) , 
                           col_6(
                            
                               plotly::plotlyOutput(ns("explore2_ui"), height = "300px")
                             ) 
                           )))
                       ))),
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore4'), title ='Crashes by Severity',  width = 12, collapsible = T, closable = F,
                         fluidRow(
                           col_3(
                             fluidRow(
                               col_12(
                                 selectInput(ns('chart1_selector'), choices = c('All Crashes'), selected= 'All Crashes', label='Select Mode:')
                               )), 
                             fluidRow(
                               col_12(
                                 uiOutput(ns("slider_sev_ui"))
                               )), 
                             fluidRow(
                               col_12(
                                 downloadButton(ns('download_cr_sev'),"Download Table Data")
                               ))
                             ),
                           col_9(
                             fluidRow(
                               col_6(
                                 fluidRow(
                                   col_12(
                                  
                                       DT::dataTableOutput(ns("explore4_table"))
                                  
                                   ) 
                                   )
                               ), 
                               col_6(
                           
                                   plotly::plotlyOutput(ns("explore1_ui"))
                               )))
                          ),
                         tags$div(hr()),
                         fluidRow(
                           column(10,  
                                  uiOutput(ns("explore2b_title_ui")),     
                                    plotly::plotlyOutput(ns("explore2b_ui"), height = "300px")
                           ), 
                           col_2(
                             downloadButton(ns('download_stacked'),"Download Chart Table")
                           ))
                 ))),
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore9_fclass'), title ='Crashes by Functional Classification',  width = 12, collapsible = T, closable = F,
                         fluidRow(
                           col_3(
                             fluidRow(
                               col_12(
                                 selectInput(ns('chart5_selector'), choices = list('All Crashes' = 'All Crashes'), selected= 'All Crashes', label='Select Mode:')
                               )),
                            p('Crash counts are summarized by the functional class of the segment closest to the crash. This application avoids double counting crashes, but it also means that results are subject to the crash location. Crashes may actually be associated with intersections and related to all four legs, not just the closest segment.'),

                             fluidRow(
                               col_12(
                                 uiOutput(ns("slider_fc_ui")),
                               )),
                             # fluidRow(
                             #   col_12(
                             #     prettySwitch(
                             #       inputId = ns("omit_fc"),
                             #       label = "Inlcude Omitted Crashes", 
                             #       slim = TRUE,
                             #       inline = TRUE,
                             #       status = "primary"
                             #     ))),
                             fluidRow(
                               col_12(
                                 downloadButton(ns('download_cr_flcass'),"Download Table Data")
                               )) 
                           ),
                         col_9(
                           fluidRow(
                             col_12(
                               plotly::plotlyOutput(ns("explore5_ui"))
                             ) 
                           ) 
                         )), 
                         fluidRow(
                           col_12(
                               DT::dataTableOutput(ns("explore9_table"))
                              )
                         )
                 )) 
             ),
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore9_cr_yr'), title ='Crashes by Year',  width = 12, collapsible = T, closable = F,
                           fluidRow(
                             col_3(
                               fluidRow(
                                 col_12(
                                   selectInput(ns('chart8_selector'), choices = list('All Crashes' = 'All Crashes'), selected= 'All Crashes', label='Select Mode:'),
                                 )),
                               # fluidRow(
                               #   col_12(
                               #     prettySwitch(
                               #       inputId = ns("omit_yr"),
                               #       label = "Inlcude Omited Crashes", 
                               #       slim = TRUE,
                               #       inline = TRUE,
                               #       status = "primary"
                               #     )                                 
                               #     )),
                               fluidRow(
                                 col_12(
                                   downloadButton(ns('download_cr_year'),"Download Table Data")
                                 ))
                             ),
                             col_9(
                               fluidRow(
                                 col_12(
                                
                                     plotly::plotlyOutput(ns("explore9_ui"))
                                   
                                 )
                               )
                             )
                         ),
                         fluidRow(
                           col_12(
                          
                               DT::dataTableOutput(ns("explore10_table"))
                          
                           )
                         )
                 )) 
             )
    ),
    
    tags$div(id=ns('crash_reporters2'), class='leaflet_block',
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore8_sum'), title ='Crash Data Summary',  width = 12, collapsible = T, closable = F,
                         HTML(paste(tags$span(style="color:#ed921a", "Fetch your data to explore crash summaries."), sep = ""))
                 )
               )
             )
    ),
    tags$div(id=ns('dns_reporters'), class='leaflet_none',
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore10'), title ='Highest Crash Corridors (Sliding Windows Scores)',  width = 12, height = 600, collapsible = T, closable = F,
                         
                           fluidRow(
                              
                               col_6(
                                 HTML("<div style='height: 500px;'>"),
                                 selectInput(ns('chart6_selector'), 
                                             choices = list('--Choose One--',
                                                            'Top Ten by Pedestrian Sliding Windows Score' = 'ped_score', 
                                                            'Top Ten by Bicycle Sliding Windows Score' = 'bike_score', 
                                                            'Top Ten by Other Sliding Windows Score' = 'other_score'), 
                                             selected = '--Choose One--', 
                                             label = 'Select Count:',
                                             width='100%'),
                                 p('This analysis uses the sliding window output but only lists the segments with the highest observed score. '),
                                 DT::dataTableOutput(ns("explore6_ui")),
                                 HTML("</div>")
                               ),
                               col_6(  
                        
                                 leafletOutput(ns("dang_cor_map"), height=450, width='100%'),
                          
                             )
                         )
                 )
                 )
             )
             
    ),
    tags$div(id=ns('dns_reporters2'), class='leaflet_block',
             fluidRow(
               col_12(
                 bs4Card(inputId=ns('explore12'), title ='Highest Crash Corridors (Sliding Windows Scores)',  width = 12, collapsible = T, closable = F,
                         HTML(paste(tags$span(style="color:#ed921a", "Run the Sliding windows analysis to review the highest crash corridors."), sep = ""))
                 )
               )
             )
    ),
    tags$div(id=ns('est_reporters'), class='leaflet_none',
             fluidRow(
               col_6(
                 bs4Card(inputId=ns('explore6'), title ='Safer Streets Model Fit: Pedestrian',  width = 12, collapsible = T, closable = F,
                           plotly::plotlyOutput(ns("explore3_ui"))
                 )
               ),
               col_6(
                 bs4Card(inputId=ns('explore7'), title ='Safer Streets Model Fit: Bicycle',  width = 12, collapsible = T, closable = F,
                           plotly::plotlyOutput(ns("explore4_ui"))
                 )
               )
             )       
    ),
    tags$div(id=ns('est_reporters2'), class='leaflet_block',
             fluidRow(
               col_12(
                 bs4Card(inputId='explore8_mod_fit', title ='Model Fit Summary Statistics',  width = 12, collapsible = T, closable = F,
                         HTML(paste(tags$span(style="color:#ed921a", "Create a Safer Streets Model to see fit summaries."), sep = ""))
                 )
               )
             )
    )
    
  )
}

#' reporter Server Function
#'
#' @noRd 
mod_reporter_server <- function(input, output, session, connection, user_id, run_id){
  ns <- session$ns
  
  source(file.path(getwd(), 'inst', 'app', 'www', 'report_downloader.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'dashboard_downloaders.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'dashboard_initializer.R'), local = TRUE)
  
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
    sql_identifier_roads=DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id)),
    sql_identifier_crashes=DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
    sql_identifier_local_user_data_schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'),
    run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
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
    bike_est_sev_cols=c('e_cr_bike_k', 'e_cr_bike_a', 'e_cr_bike_b', 'e_cr_bike_c', 'e_cr_bike_o'),
    bike_his_sev_cols=c('tot_bike_k', 'tot_bike_a', 'tot_bike_b', 'tot_bike_c', 'tot_bike_o'),
    ped_est_sev_cols=c('e_cr_ped_k', 'e_cr_ped_a', 'e_cr_ped_b', 'e_cr_ped_c', 'e_cr_ped_o'),
    ped_his_sev_cols=c('tot_ped_k', 'tot_ped_a', 'tot_ped_b', 'tot_ped_c', 'tot_ped_o'),
    sev_cols=c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'),
    fclass_cols=c('Major Arterial', 'Minor Arterial', 'Major Collector', 'Minor Collector', 'Local Road'),
    sa=NULL,
    rd=NULL,
    cr=NULL,
    es=NULL,
    ds=NULL,
    dns_table=NULL,
    reporter_dropdown=data.frame(values= c('study_area', 'Bicycle Crash', 'Pedestrian Crash', 'Other Crash', 'roads','ped_score', 'bike_score', 'other_score', 'rt_ped_cost_1y', 'rt_bike_cost_1y'), 
                                 names=c("Study Area", 'Bicycle Crashes', 'Pedestrian Crashes', 'Other Crashes', "Roads by Functional Classification", "Pedestrian Sliding Windows Results", "Bicycle Sliding Windows Results", "Other Sliding Windows Results", "Estimated Pedestrian Crash Rate", "Estimated Bicycle Crash Rate") 
    ), # notice the other between values and names. Also notice the values of values line match following three reactive values.
    sw_columns = c('ped_score', 'bike_score', 'other_score'),
    mod_columns = c('rt_bike_cost_1y', 'rt_ped_cost_1y'),
    local_usr_data=c('study_area', 'Bicycle Crash', 'Pedestrian Crash', 'Other Crash', 'roads'),
    crash_colors=c('#fdd0a2', '#fdae6b', '#fd8d3c', '#f16913', '#f16913'),
    crash_col_levs = c('Property Damage Only (O)', 'Possible Injury (C)', 'Non-Incapacitating Injury (B)', 'Incapacitating Injury (A)', 'Fatality (K)'),
    user_d=NULL,
    fclass_perc_tnames=c("Functional Classification","Total Crashes","Percent of Total Crashes", "Total Miles", "Crashes Per Mile"),
    mode_count_tnames=c("Crashes", "Total Crashes", "Percent of Total"),
    roads_length=NULL,
    pie_chart2_table_hold=NULL,
    top_10_segs=NULL,
    to_download_crash_by_severity=NULL,
    to_download_crash_by_mode=NULL,
    to_download_crash_by_fclass=NULL,
    to_download_crash_by_year=NULL,
    to_download_stacked_bar=NULL,
    crash_years=NULL,
    d2=NULL,
    est_table=NULL,
    chart_sev_crashes=NULL,
    table_sev_crashes=NULL,
    chart_fclass_crashes=NULL,
    table_fclass_crashes=NULL,
    fclass_crashes_holding=NULL,
    chart_mode_crashes=NULL,
    table_mode_crashes=NULL,
    stacked_mode_crashes=NULL,
    slider_fc=NULL,
    model_fit_chart_by_severity=NULL,
    model_fit_chart_by_fclass=NULL,
    table_crashes_per_year=NULL,
    chart_crashes_per_year=NULL,
    t10ped_exists=F,
    t10bike_exists=F,
    t10other_exists=F,
    init = 0,
    entries = 1,
    report_exists = F,
    search_for_report = F,
    report_exists_init=F,
    report_status = 'no_report_requested'

  )

  # render map 
  render_map(output, 'dang_cor_map')
  outputOptions(output, "dang_cor_map", suspendWhenHidden = FALSE)
  
  #disable downloader until report exists 
 
   shinyjs::runjs(code = '
           document.getElementById(\'reporter_ui_1-download_report\').classList.add("disabled");
                                  document.getElementById(\'reporter_ui_1-download_report\').classList.add(\'report_hover_override\');
                                  document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: no-drop !important;");
         ')
              

    
  # test to see if report exists 
  data$report_exists_init <- suppressMessages(aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), Sys.getenv("S3_BUCKET")))
  print(data$report_exists_init )
  if (data$report_exists_init) {
    shinyjs::runjs(code = 'document.getElementById(\'reporter_ui_1-download_report\').classList.remove("disabled");
                           document.getElementById(\'reporter_ui_1-download_report\').classList.remove(\'report_hover_override\');
                           document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: pointer !important;");
                            ')
  } 
  
  report_status_q <- glue::glue('SELECT report_status FROM gen_management.accounts WHERE user_id = user_id AND run_id = \'{run_id}\';')
  init_check <- DBI::dbGetQuery(connection, report_status_q)
  data$report_status <- init_check
  if (init_check == 'building_report' || init_check == 'report_requested'){
    data$search_for_report <- TRUE
  } 
 

 
  # add event listerns 
  shinyjs::runjs(code = paste0('$("#fun_class_map_table").click(function(){$("#', session$ns('dang_cor_map'), '").trigger("shown");})'))
  shinyjs::runjs(code = paste0('$("#tab-manage_data").click(function(){$("#', session$ns('refresh_button_hidden'), '").click();})'))
  shinyjs::runjs(code = paste0('$("#tab-manage_data").click(function(){$("#', session$ns('dang_cor_map'), '").trigger("shown");})'))
  
  # this observer listens for a click of the download instructions button
  observeEvent(input$download_instructions, {
    showModal(download_instructions_modal)
  })
  
  # closes download instructions button
  observeEvent(input$ok, {
    removeModal()
  })
  
  # hide and show crash statistics box depending on if crash table exists or not 
  observe({
      if(data$cr_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters2'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters2'), '").addClass("leaflet_none");'))
      } else if (!data$cr_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters'), '").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters2'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('crash_reporters2'), '").addClass("leaflet_block");'))
      }
    })
  
  # hide and show crash estimation statistics box depending on if model table exists or not 
  observe({
      if(data$est_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters2'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters2'), '").addClass("leaflet_none");'))
      } else if (!data$est_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters'), '").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters2'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('est_reporters2'), '").addClass("leaflet_block");'))
      }
    })
  
  # hide and show crash density results box depending on if crash density results exists or not 
  observe({
      if(data$dns_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters2'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters2'), '").addClass("leaflet_none");'))
      } else if (!data$dns_exists){
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters'), '").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters2'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('dns_reporters2'), '").addClass("leaflet_block");'))
      }
    })
 
  # Crashes by functional classification data plot
  observe({
    if (!is.null(data$fclass_crashes_holding) && !is.null(input$slider_fc)){
    filtered <- data$fclass_crashes_holding %>% 
      filter(crash_year >= input$slider_fc[1]) %>%
      filter(crash_year <= input$slider_fc[2])
    if (nrow(filtered) < 1 && !is.null(input$chart5_selector)){
      shiny_warming_alert(title = 'No Data', text='You filtered out all the data.')
    } else {
        if ( input$chart5_selector == 'Bicycle Crashes' && !is.null(input$chart5_selector) ) {
          crashes <- get_crashes_sev_fclass(filtered, 'Bicycle Crash')
        } else if (input$chart5_selector == 'Pedestrian Crashes' && !is.null(input$chart5_selector) ) {
          crashes <- get_crashes_sev_fclass(filtered, 'Pedestrian Crash')
        } else if (input$chart5_selector == 'Other Crashes' && !is.null(input$chart5_selector) ) {
          crashes <- get_crashes_sev_fclass(filtered, 'Other Crash')
        } else if (input$chart5_selector == 'All Crashes' && !is.null(input$chart5_selector) ) {
          crashes <- get_crashes_sev_fclass(filtered, 'All Crashes')
        } else if (input$chart5_selector == 'Omited Crashes' && !is.null(input$chart5_selector) ) {
          crashes <- get_crashes_sev_fclass(filtered, 'Omit From Analysis')
        }
     data$table_fclass_crashes <- crashes 

     output$explore5_ui <- plotly::renderPlotly({
       plotly::plot_ly(crashes, x = ~fclass_mapped, y = ~k, name = 'Fatality (K)', type = 'bar')  %>%
         plotly::add_trace(y = ~a, name = 'Incapacitating Injury (A)')  %>%
         plotly::add_trace(y = ~b, name = 'Non-Incapacitating Injury (B)')  %>%
         plotly::add_trace(y = ~c, name = 'Possible Injury (C)')  %>%
         plotly::add_trace(y = ~o, name = 'Property Damage Only (O)')  %>%
         plotly::layout(legend = list(orientation = "h",
                                      xanchor = "center",
                                      y = -.5,
                                      x = .5),
                        yaxis = list(
                          title = 'Count'),
                        xaxis = list(
                          title = ''
                        ))
     })  
     outputOptions(output, "explore5_ui", suspendWhenHidden = FALSE)
    }}
    })
  
  # Crashes by functional classification table 
  observe({ 
    if (!is.null(data$table_fclass_crashes) && 
        !is.null(input$slider_fc) && 
        !is.null(data$roads_length)  && 
        !is.null(data$fclass_perc_tnames)
        ){
      
      # build data 
     c_table <- data$table_fclass_crashes %>% 
       dplyr::rowwise() %>% 
       dplyr::mutate(tot = sum(c(k, a, b, c, o)))
     c_table$total <- sum(c_table$tot)
     c_table <- c_table %>% 
       dplyr::mutate(perc = round(as.numeric(tot/total)*100, 2)) %>% 
       dplyr::select(-c(k, a, b, c, o, total)) %>% 
       dplyr::left_join(data$roads_length, by = c("fclass_mapped" = "usdot_fun_class_mapped") ) %>% 
       dplyr::mutate(density = round(as.numeric(tot/miles), 2)) %>% 
       dplyr::mutate_if(is.numeric, ~round(., 2)) %>% 
       dplyr::mutate_if(is.numeric, ~gsub(",", ".", .))  
     colnames(c_table) <- data$fclass_perc_tnames
     
     # create table to download 
     data$to_download_crash_by_fclass <- c_table
     
     # render 
     output$explore9_table <- DT::renderDataTable({c_table}, options = list( pageLength = 10, dom = 't', initComplete = JS(
       "function(settings, json) {",
       "$(this.api().table().body()).css({'font-size': '90%'});",
       "$(this.api().table().header()).css({'font-size': '95%'});",
       "}") ))
     outputOptions(output, "explore9_table", suspendWhenHidden = FALSE)
    }})
  
  # Crashes per year 
  observe({
    if (!is.null(data$table_crashes_per_year)){
        if (input$chart8_selector == 'All Crashes') {
          
          # build table of all crashes 
          y_data <- data$table_crashes_per_year
          y_data <- y_data %>%
            dplyr::group_by(crash_year, severity_mapped) %>%
            replace(is.na(.), 0) %>%
            dplyr::summarise(count = dplyr::n()) 
 
          
        } else if (input$chart8_selector != 'All Crashes') {
          
          # build table with filter 
          y_data <- data$table_crashes_per_year %>%
            filter(crashes == input$chart8_selector) %>%
            dplyr::group_by(crash_year, severity_mapped) %>%
            replace(is.na(.), 0) %>%
            dplyr::summarise(count = dplyr::n()) 
        }
      
        # build pivot table 
        y_data <- y_data %>% tidyr::pivot_wider(names_from=c(severity_mapped), values_from=count) %>% 
          replace(is.na(.), 0) 
        
        # add and populate missing columns with 0
        y_data[c(data$sev_cols)[!(c(data$sev_cols) %in% colnames(y_data))]] = 0
        
        # rename column 
        names(y_data)[names(y_data) == "crash_year"] <- "Year"
        
        # create year table for download 
        data$to_download_crash_by_year <- y_data
        
        # render table 
        output$explore10_table <- DT::renderDataTable({as.data.frame(y_data)}, options = list( pageLength = 20, dom = 't', initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-size': '90%'});",
          "$(this.api().table().header()).css({'font-size': '95%'});",
          "}") ))
        outputOptions(output, "explore10_table", suspendWhenHidden = FALSE)
        
        # render plot 
        output$explore9_ui <- plotly::renderPlotly({
          plotly::plot_ly(y_data, x = ~Year, y = ~`Fatality (K)`, name = 'Fatality (K)', type = 'bar')  %>%
            plotly::add_trace(y = ~`Incapacitating Injury (A)`, name = 'Incapacitating Injury (A)')  %>%
            plotly::add_trace(y = ~`Non-Incapacitating Injury (B)`, name = 'Non-Incapacitating Injury (B)')  %>%
            plotly::add_trace(y = ~`Possible Injury (C)`, name = 'Possible Injury (C)')  %>%
            plotly::add_trace(y = ~`Property Damage Only (O)`, name = 'Property Damage Only (O)')  %>%
            plotly::layout(legend = list(orientation = "h",
                                         xanchor = "center",
                                         y = -.5,
                                         x = .5),
                           yaxis = list(
                             title = 'Count'),
                           xaxis = list(
                             title = ''
                           ))
          
        })
        outputOptions(output, "explore9_ui", suspendWhenHidden = FALSE)
    }
  })
     
  # Model fit chart, severity
  observe({
    if (!is.null(data$est_table) && !is.null(data$cr_exists)){
      
          # downloads data 
          barc_data <- get_fit_data(connection=connection,
                                    table=data$est_table,
                                    est_columns = data$bike_est_sev_cols, 
                                    column_rename = data$sev_cols,
                                    user_id=user_id,
                                    run_id=run_id,
                                    mode = 'bike')
        # estimates data 
        barc_data[['Estimated']] <- round(as.numeric(barc_data[['Estimated']], 2))

        # creates plot 
        output$explore4_ui <- plotly::renderPlotly({
          plotly::plot_ly(barc_data, x = ~Type, y = ~Observed, name = 'Observed', type = 'bar')  %>% 
            plotly::add_trace(y = ~Estimated, name = 'Estimated')  %>%  
            plotly::layout(legend = list(orientation = "h",   
                                         xanchor = "center",
                                         y = -.8,
                                         x = .8),
                           yaxis = list(
                             title = 'Count'),
                           xaxis = list(
                             title = ''
                           )) 
        })
      }
  })
  
  # Model fit chart, functional classification
  observe({
    if (!is.null(data$est_table) && !is.null(data$cr_exists)){
      
      # downloads data 
        barc_data2 <- get_fit_data(connection = connection, 
                                   table=data$est_table,
                                   est_columns = data$ped_est_sev_cols, 
                                   column_rename = data$sev_cols,
                                   user_id = user_id, 
                                   run_id = run_id,
                                   mode = 'ped')
 
        barc_data2[['Estimated']] <- round(as.numeric(barc_data2[['Estimated']], 2))

        # render plot 
        output$explore3_ui <- plotly::renderPlotly({
          plotly::plot_ly(barc_data2, x = ~Type, y = ~Observed, name = 'Observed', type = 'bar')  %>% 
            plotly::add_trace(y = ~Estimated, name = 'Estimated')  %>%  
            plotly::layout(legend = list(orientation = "h",   
                                         xanchor = "center",
                                         y = -.5,
                                         x = .5),
                           yaxis = list(
                             title = 'Count'),
                           xaxis = list(
                             title = ''
                           )) 
        })}
    })
  
  #Crashes by severity table
  observe({
    if (!is.null(data$table_sev_crashes) && !is.null(input$slider_sev)){
      
      # filters data on sliders 
      if(!is.null(input$slider_sev[1]) ){
        pie_chart1 <- data$table_sev_crashes %>% 
          filter(crash_year >= input$slider_sev[1]) %>% 
          filter(crash_year <= input$slider_sev[2])
      } else {
        pie_chart1 <- data$table_sev_crashes
      }
      
      # filters data on mode 
      if ( input$chart1_selector != 'All Crashes' && !is.null(input$chart1_selector) ) { 
        pie_chart1 <-  pie_chart1 %>% 
          filter(crashes == input$chart1_selector) 
      }
        if (nrow(pie_chart1) == 0) {
          shiny_warming_alert(title = 'No Data', text='You filtered out all the data.')
        } else {
          pie_chart1 <- pie_chart1 %>% 
            dplyr::group_by(severity_mapped) %>%
            dplyr::summarise(n = dplyr::n())
          
          # plot data 
          output$explore1_ui <- plotly::renderPlotly({
            plotly::plot_ly(pie_chart1) %>%
              plotly::add_pie(
                hole = 0.6, 
                labels = unique(pie_chart1$severity_mapped), 
                values = ~n, 
                customdata = ~severity_mapped
              ) %>% 
              plotly::layout(legend = list(orientation = "h",    
                                           xanchor = "center",  
                                           x = 0.5))  
          })
          outputOptions(output, "explore1_ui", suspendWhenHidden = FALSE)
          
          # additional work 
          d1 <- pie_chart1
          d1 <- d1 %>% mutate(Percent = round((n/sum(n)*100), 1))
          d1 <- as.data.frame(d1)
          colnames(d1 ) <- c("Severity","Count","Percent of Total")
          d1["Percent of Total"] <- sapply(d1["Percent of Total"], gsub, pattern = ",", replacement= ".")
          df <- data.frame(one='Total',
                           two=sum(d1[2]),
                           three=100.0
          )
          colnames(df ) <- c("Severity","Count","Percent of Total")
          d1 <- rbind(d1, df)

          output$explore4_table = DT::renderDataTable({ d1 }, options = list( pageLength = 10, dom = 't', initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().body()).css({'font-size': '90%'});",
            "$(this.api().table().header()).css({'font-size': '95%'});",
            "}") ))
          outputOptions(output, "explore4_table", suspendWhenHidden = FALSE)
          data$to_download_crash_by_severity <- d1
        }
      }
    })

  # Crashes by severity pie chart 
  observe({
    if (!is.null(data$chart_sev_crashes) && nrow(data$chart_sev_crashes) > 0 && !is.null(input$slider_mod)){
      if(!is.null(input$slider_mod[1]) ){
        pie_chart2 <- data$chart_sev_crashes %>% 
          filter(crash_year >= input$slider_mod[1]) %>% 
          filter(crash_year <= input$slider_mod[2])
      } else {
        pie_chart2 <- data$chart_sev_crashes
      }
      if ( input$chart2_selector != 'All Crashes' ) { 
        data$d2 <- pie_chart2
        pie_chart2 <-  pie_chart2 %>% 
          filter(severity_mapped == input$chart2_selector)
      } else {
        data$d2 <- pie_chart2
      }
      if (nrow(pie_chart2) == 0) {
        shiny_warming_alert(title = 'No Data', text='You filtered out all the data.')
      } else {

        pie_chart2_final <- pie_chart2 %>% 
          dplyr::group_by(crashes) %>%
          dplyr::summarise(n = dplyr::n())
        output$explore2_ui <- plotly::renderPlotly({
          plotly::plot_ly(pie_chart2_final) %>%
            plotly::add_pie(
              hole = 0.6,
              labels = ~crashes, 
              values = ~n, 
              customdata = ~crashes
            ) %>% 
            plotly::layout(legend = list(orientation = "h",   
                                         xanchor = "center",  
                                         x = 0.5)) 
        })
        outputOptions(output, "explore2_ui", suspendWhenHidden = FALSE)
        data$pie_chart2_table_hold <- pie_chart2_final
        
        pie_chart2_table <- data$pie_chart2_table_hold
        pie_chart2_table <- pie_chart2_table %>% mutate(perc_total = round((n/sum(n)*100), 1))
        pie_chart2_table <- as.data.frame(pie_chart2_table)
        colnames(pie_chart2_table) <- data$mode_count_tnames
        pie_chart2_table["Percent of Total"] <- sapply(pie_chart2_table["Percent of Total"], gsub, pattern = ",", replacement= ".")
        df <- data.frame(one='Total',
                         two=sum(pie_chart2_table[2]),
                         three=100.0
        )
        colnames( df ) <- data$mode_count_tnames
        pie_chart2_table <- rbind(pie_chart2_table, df)
        data$to_download_crash_by_mode <- pie_chart2_table
        output$explore5_table = DT::renderDataTable({ pie_chart2_table }, options = list( pageLength = 10, dom = 't', initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().body()).css({'font-size': '90%'});",
          "$(this.api().table().header()).css({'font-size': '95%'});",
          "}") ))
        outputOptions(output, "explore5_table", suspendWhenHidden = FALSE)
      }
    }  
  })
  
  # Creates data for stacked bar chart 
  observe({
    if (!is.null(data$d2) && !is.null(input$slider_mod)){
      count1 <- data$d2 %>% 
        filter(crashes == 'Bicycle Crashes') %>%  
        group_by(severity_mapped) %>%      
        dplyr::summarise(n = dplyr::n()) 
      count2 <- data$d2 %>% 
        filter(crashes == 'Pedestrian Crashes') %>%  
        group_by(severity_mapped) %>%      
        dplyr::summarise(n = dplyr::n()) 
      count3 <- data$d2 %>% 
        filter(crashes == 'Other Crashes') %>%  
        group_by(severity_mapped) %>%      
        dplyr::summarise(n = dplyr::n())  
      
      
      empty <- as.data.frame(data$sev_cols)
      colnames(empty ) <- c("severity_mapped")
      
      table <-  dplyr::left_join(empty, count1, by = 'severity_mapped')
      table <-  dplyr::left_join(table, count2, by = 'severity_mapped')   
      table <-  dplyr::left_join(table, count3, by = 'severity_mapped')
      
      colnames(table ) <- c("Severity","Bicycle_count","Pedestrian_count", "Other_count")
      table[is.na(table)] <- 0
      table <- table %>%  dplyr::rowwise() %>%
        mutate(total = sum(c(Bicycle_count, Pedestrian_count, Other_count)))
      table[is.na(table)] <- 0
      table['Bicycle'] <- round(as.numeric(table$Bicycle_count/table$total)*100, 1)
      table['Pedestrian'] <- round(as.numeric(table$Pedestrian_count/table$total)*100, 1)
      table['Other'] <- round(as.numeric(table$Other_count/table$total)*100, 1)
      table <- table %>% 
        select(-c("Bicycle_count","Pedestrian_count", "Other_count"))
      table[is.na(table)] <- 0
      
      output$explore2b_title_ui <- renderUI({  paste0('All crashes, by severity and mode between ', 
                                                      input$slider_mod[1], 
                                                      ' and ', 
                                                      input$slider_mod[2], 
                                                      '.')})
      outputOptions(output, "explore2b_title_ui", suspendWhenHidden = FALSE)
      data$to_download_stacked_bar <- table %>% 
        dplyr::mutate_if(is.numeric, ~gsub(",", ".", .)) %>% 
        dplyr::select(-c(total)) 
      
      data$crash_by_severity_stacked_chart <- table
      fig <- plotly::plot_ly(table, 
                             x = ~Bicycle, 
                             y = ~Severity, 
                             type = 'bar', 
                             orientation = 'h', 
                             name = 'Bicycle',
                             marker = list(color = 'rgba(50,171, 96, 0.7)',
                                           line = list(color = 'rgba(50,171,96,1.0)',
                                                       width = 1)),
                             hovertemplate = paste('<b>%{x}%</b>, %{y}'))
      fig <- fig %>% plotly::add_trace(x = ~Pedestrian, name = 'Pedestrian',
                                       marker = list(color = 'rgba(55,128,191,0.7)',
                                                     line = list(color = 'rgba(55,128,191,1.0)',
                                                                 width = 1)))
      fig <- fig %>% plotly::add_trace(x = ~Other, name = 'Other',
                                       marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                                     line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                                 width = 1)))
      fig <- fig %>% plotly::layout(barmode = 'stack',
                                    xaxis = list(title = ""),
                                    yaxis = list(title =""))
      output$explore2b_ui <- plotly::renderPlotly({
        fig
      })
      outputOptions(output, "explore2b_ui", suspendWhenHidden = FALSE)
    }
  })

  # Downloads high scoring corridors  
  observe({
    if(data$dns_exists && !is.null(input$slider_mod) && input$chart6_selector != '--Choose One--'){
      top_10 <- get_dangerous_locations(c=connection, the_input=input$chart6_selector, user_id=user_id, run_id=run_id)
      top_10 <- top_10[!is.na(names(top_10))]
      top_10 <- top_10 %>% filter(`Crash Score` > 0)
      t <- top_10[1:3]  
      
      if (nrow(t) < 1) {
       print('none')
      } else {
        fsf  <- sf::st_as_sf(top_10 , wkt='geom')
        fsf2 <- transform_with_epsg(fsf, 4326)
        bboxes <- sf::st_bbox(fsf2) %>% as.vector()
        
        if (nrow(fsf2)>0) {
          leaflet::leafletProxy("dang_cor_map", session) %>%
            leaflet::clearShapes() %>%
            leaflet::addPolylines(data=fsf2,
                                  color = "#d10d79",
                                  opacity = 1,
                                  weight = 3,
                                  popup=~paste0(HTML('<strong>Road Name</strong>: '), fsf2[['Name']], '<br>',
                                                HTML('<strong>Functional Classification</strong>: '), fsf2[['Functional Class']], '<br>',
                                                HTML(paste('<strong>Crash Density Score</strong>: ')), fsf2[['Crash Score']])
            ) %>%
            leaflet::fitBounds(bboxes[1], bboxes[2], bboxes[3], bboxes[4])
          
          
          data$top10_table <- t
          output$explore6_ui <-  DT::renderDataTable({t}, options = list( pageLength = 10, dom = 't', initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().body()).css({'font-size': '90%'});",
            "$(this.api().table().header()).css({'font-size': '95%'});",
            "}") 
          ))
          outputOptions(output, "explore6_ui", suspendWhenHidden = FALSE)
        } else {
          leaflet::leafletProxy("dang_cor_map", session) %>%
            leaflet::clearShapes()  
        }
      }}
      })

  # hidden button engages when user clicks Dashboard tab in sidebar
  observeEvent(input$refresh_button_hidden, {
    if (data$entries == 1) {
      waiter::waiter_show(
        color='rgba(175, 175, 175, 0.85)',
        html = tagList(
          tags$div(waiter::spin_1()),
          tags$br(),
          tags$div(HTML("Fetching data ..."))
        )
      )
      # initialize when a user first arrives 
      initialize()
      waiter::waiter_hide()
      data$entries = data$entries + 1
    }
    })

  # Engages when user clicks Refresh button
  observeEvent(input$refresh_button, {
    waiter::waiter_show(
      color='rgba(175, 175, 175, 0.85)',
      html = tagList(
        tags$div(waiter::spin_1()),
        tags$br(),
        tags$div(HTML("One moment ..."))
      )
    )
    # initializes, 
    init <- initialize()
        waiter::waiter_hide()
        if (init == 'after_first_init_data') {
          shinyjs::delay(1200, shiny_warming_alert(title='Complete', text='Refreshed your dashboard', showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=3000, type="success"))
        } else if (init == 'after_first_init_no_data') {
          shinyjs::delay(600, shiny_warming_alert(title='No Data', text='No data exists to summarize', showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=3000, type="warning"))
        }
    })
  
  # Report downloader 
  output$download_report <- 
    downloadHandler(
      paste0(run_id, '_sspf_', "report.pdf"),
      content = 
        function(file)
        {
          fetch_report_from_s3(bucket=Sys.getenv("S3_BUCKET"), user_id=user_id, run_id=run_id)
          file.copy(glue::glue('{user_id}_{run_id}_report.pdf'), file)
          file.remove(glue::glue('{user_id}_{run_id}_report.pdf'))
          data$report_status <- 'no_report_requested'
      }
  )
  

  rebuild_report_mod <- mod_delete_model_ui("delete_model_ui_4", confirm_button = ns("delete_report"), cancel_button = ns("do_not_delete_report"), text="Are you sure you want to rebuild your existing Safer Streets Priority Finder Report? By rebuilding this report, you will permanently delete your current version.")
  rebuild_rep <- function(){
    shiny_warming_alert(title='Request Submitted', text='Your request for a Safer Streets Priority Finder Report has been submitted! The Download Report button will be enabled when your report is ready.', showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="success")
    aws.s3::delete_object(glue::glue('{user_id}_{run_id}_report.pdf'), Sys.getenv("S3_BUCKET"), quiet = TRUE)
    data$report_exists <- suppressMessages(aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), Sys.getenv("S3_BUCKET")))
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'report_requested_time'), new_value='now()')
    update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'report_status'), new_value=DBI::dbQuoteString(connection, glue::glue('report_requested')))
    data$report_status <- DBI::dbGetQuery(connection, report_status_q)[1,1]
    data$search_for_report <- TRUE

  }
  
  observeEvent(input$do_not_delete_report, {
    removeModal()
  })
  
  observeEvent(input$delete_report, {
    removeModal()
    rebuild_rep()
  })
  
  observeEvent(input$create_report, {
    if (data$report_status == 'report_ready' || data$report_status == 'no_report_requested') {
      if(suppressMessages(aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), Sys.getenv("S3_BUCKET")))){
        showModal(rebuild_report_mod)
      } else {
        rebuild_rep()
      }
    }
  })

  observe({
    if(data$search_for_report){
      invalidateLater(10000)
        data$report_exists <- suppressMessages(aws.s3::object_exists(glue::glue('{user_id}_{run_id}_report.pdf'), Sys.getenv("S3_BUCKET")))
        data$report_status <- DBI::dbGetQuery(connection, report_status_q)[1,1]
        print(data$report_status )
    }
  })

  observe({
    print(data$report_status )
    if (data$report_status == 'no_report_requested') {
      output$reporter_message <- renderUI({NULL})
      outputOptions(output, "reporter_message", suspendWhenHidden = FALSE)
      if (data$report_exists) {
         data$search_for_report <- FALSE
         shinyjs::runjs(code = 'document.getElementById(\'reporter_ui_1-download_report\').classList.remove("disabled");
                                document.getElementById(\'reporter_ui_1-download_report\').classList.remove(\'report_hover_override\');
                                document.getElementById(\'reporter_ui_1-create_report\').classList.remove("disabled");
                                document.getElementById(\'reporter_ui_1-create_report\').classList.remove(\'report_hover_override\');
                                document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: pointer !important;");
                               ')
      }
    } else if (data$report_status == 'report_requested') {
      shinyjs::runjs(code = '
                            document.getElementById(\'reporter_ui_1-create_report\').classList.add("disabled");
                            document.getElementById(\'reporter_ui_1-create_report\').classList.add(\'report_hover_override\');
                            document.getElementById(\'reporter_ui_1-download_report\').classList.add("disabled");
                            document.getElementById(\'reporter_ui_1-download_report\').classList.add(\'report_hover_override\');
                            document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: no-drop !important;");
                           ')
      output$reporter_message <- renderUI({
        HTML(
          '<div class="center_reporter">
                Report request submitted.
                </div>'
        )  
        })
    } else if (data$report_status == 'building_report') {
      shinyjs::runjs(code =  'document.getElementById(\'reporter_ui_1-create_report\').classList.add("disabled");
                              document.getElementById(\'reporter_ui_1-create_report\').classList.add(\'report_hover_override\');
                              document.getElementById(\'reporter_ui_1-download_report\').classList.add("disabled");
                              document.getElementById(\'reporter_ui_1-download_report\').classList.add(\'report_hover_override\');
                              document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: no-drop !important;");
                             ') 
      output$reporter_message <- renderUI({
         HTML(
              '<div class="center_reporter">
                The Safer Streets Priority Finder is building your report. We\'ll notify you as soon it\'s ready.
                </div>'
               )  
        })
    } else if (data$report_status == 'report_ready') {

      shinyjs::runjs(code = 'document.getElementById(\'reporter_ui_1-download_report\').classList.remove("disabled");
                             document.getElementById(\'reporter_ui_1-download_report\').classList.remove(\'report_hover_override\');
                             document.getElementById(\'reporter_ui_1-create_report\').classList.remove("disabled");
                             document.getElementById(\'reporter_ui_1-create_report\').classList.remove(\'report_hover_override\');
                             document.getElementById(\'reporter_ui_1-downloader_div\').setAttribute("style", "cursor: pointer !important;");
                            ') 
      output$reporter_message <- renderUI({
         HTML(
          '<div class="center_reporter">
                Your report is ready! Click the \'Download Report\' button to get your Safer Streets Priority Finder Report.
                </div>'
        )  
      })
    }
  })

  }
  
## To be copied in the UI
# mod_reporter_ui("reporter_ui_1")

## To be copied in the server
# callModule(mod_reporter_server, "reporter_ui_1")

