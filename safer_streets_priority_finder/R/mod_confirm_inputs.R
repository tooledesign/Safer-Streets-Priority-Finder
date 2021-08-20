#' confirm_inputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import tidyr

mod_confirm_inputs_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    fluidRow(
    col_12(
        bs4Card(inputId=ns('intro_card'), title='Review Your Data', width = 12, closable = F,
                p("In this section, you'll have a chance to review and confirm your input data."),
                tags$ul(
                  tags$li("If you’ve uploaded your data, click the 'Perform Checks' button below."),
                  tags$li("You can display your data in the map by clicking the 'Map Data' button."),
                  tags$li("The \'Update Data Management Table\' button refreshes the table, which includes key information about the current scenario."),
                )
        ))),
    fluidRow(
      col_12(
      bs4Card(inputId=ns('issue_tracker'), title='Issue Tracker', width = 12, closable = F, 
              fluidRow(
                col_12(
                p('This card will highlight any issues in your data. Click the Perform Checks button.'),
                tags$div( 
                uiOutput(ns("all_data_exists_check")),
                uiOutput(ns("same_crs_check")),
                uiOutput(ns("within_spatial_bounds_check1")),
                uiOutput(ns("within_spatial_bounds_check2")),
                uiOutput(ns("within_spatial_bounds_check3")),
                hr()
                ),
                fluidRow(
                  col_12(
                    actionButton(ns("perform_checks"), "Refresh", class = "btn btn-primary"),
                    actionButton(ns("input_instructions"), "Instructions", class = "btn btn-primary"),
                    #actionButton(ns("update_data_review_table"), "Update Data Management Table", class = "btn btn-primary"),
                    actionButton(ns("to_analysis"), "Jump to Analysis Page", class = "btn btn-primary"),
                    actionButton(ns("perform_checks_hidden"), "Refresh", class = "btn btn-primary leaflet_none")
                  )))))
            )),
    fluidRow(
      col_12(
        bs4Card(inputId=ns('map_confirm_data'), title='Visualize Your Inputs', width = 12, closable = F,
          leafgl::leafglOutput(ns("data_map"), height=750, width='100%')
        ))),
    fluidRow(
      col_12(
        bs4Card(inputId=ns('account_confirm_data'), title='Review Your Account Information', width = 12, closable = F,
        textOutput(ns("last_updated")),
        br(),
        DT::dataTableOutput(ns("table_user_data"))
      ))),
    fluidRow(
      col_12(
        bs4Card(inputId=ns('account_confirm_data'), title='Review How Your Data\'s Variables Were Assigned', width = 12, closable = F,
         p("This section summarizes how your uploaded data or default data variables were assigned to the standard
          variables used in the tool during the initial load processing. Each table below includes information on how
          the original/user uploaded data variables relate to the standard variables, as well as the total count and
          proportion of of each variable."),
         br(),
         tags$h6('Crash Severity'),
          tags$div(id = ns('crash_sev_crosswalk_t_null'), class='leaflet_block',
                   HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))
                  ), 
          tags$div(id = ns('crash_sev_crosswalk_t_active'), class='leaflet_none',
                   DT::dataTableOutput(ns("crash_sev_mapped_vars")),
          ),
         br(),
         tags$h6('Crash Mode'),
         tags$div(id = ns('crash_mode_crosswalk_t_null'), class='leaflet_block',
                  HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))
         ), 
         tags$div(id = ns('crash_mode_crosswalk_t_active'), class='leaflet_none',
                  DT::dataTableOutput(ns("crash_mode_mapped_vars"))
         ),
         
         br(),
         tags$h6('Crash Costs'),
         tags$div(id = ns('crash_cost_crosswalk_t_null'), class='leaflet_block',
                  HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))
         ), 
         tags$div(id = ns('crash_cost_crosswalk_t_active'), class='leaflet_none',
                  DT::dataTableOutput(ns("crash_cost_mapped_vars"))
         ),
         
         br(),
         tags$h6('Road Functional Classification'),
         tags$div(id = ns('road_fclass_crosswalk_t_null'), class='leaflet_block',
                  HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))
         ), 
         tags$div(id = ns('road_fclass_crosswalk_t_active'), class='leaflet_none',
                  DT::dataTableOutput(ns("road_fclass_mapped_vars"))
         ) 
      ))),
    br(),
    fluidRow(
      col_12(
        bs4Card(inputId=ns('keep_data_roads'), title='Data Storage', width = 12, closable = F,
                p('Would you like to opt in to allowing a copy of your data to be used for future transportation planning and safety research purposes? Please note, as required to run the analysis, your data is automatically saved for six months regardless of whether you opt into storing data for future purposes.'),
                p('Can we save copies of your roads, crash, and study area data for future transportation planning and safety research?'),
                radioGroupButtons(
                  inputId = ns("keep_data_selection"),
                  label = "May We Save Copies of Your Roads, Crash, and Study Area Data?",
                  choices = c("Yes, you may save copies of my data for research!",  "No, my data may not be saved."),
                  individual = TRUE,
                  justified=T,
                  checkIcon = list(
                    yes = tags$i(class = "fa fa-circle", 
                                 style = "color: steelblue"),
                    no = tags$i(class = "fa fa-circle-o", 
                                style = "color: steelblue"))
                )
        ))
    )
 

  
  )
}

#' confirm_inputs Server Function
#'
#' @noRd 
mod_confirm_inputs_server <- function(input, output, session, connection, user_id, run_id){
  ns <- session$ns
  
  data <- reactiveValues(
    the_roads = NULL, 
    the_crashes = NULL,
    the_study_area = NULL,
    run_inputs = NULL,
    hin_network = NULL,
    sql_literal_study_area=DBI::dbQuoteLiteral(connection, return_table_name('study_area', user_id, run_id)),
    sql_literal_roads=DBI::dbQuoteLiteral(connection, return_table_name('roads', user_id, run_id)),
    sql_literal_crashes=DBI::dbQuoteLiteral(connection, return_table_name('crashes', user_id, run_id)),
    sql_identifier_roads=DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id)),
    sql_identifier_crashes=DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
    sql_identifier_sa=DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id)),
    run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
    sql_literal_local_user_data_schema=DBI::dbQuoteLiteral(connection, 'local_user_data'),
    sa_exists=F,
    cr_exists=F,
    rd_exists=F,
    centroid=NULL,
    crs=4326,
    user_table=NULL,
    max_bounds=NULL,
    time = Sys.time(),
    fclass_colors=c('#1f78b4', '#33a02c', '#b2df8a', '#fb9a99', '#7d95ff', '#a6cee3', '#adadad'),
    fclass_weight=c(5, 4, 4, 3, 3, 2, 1),
    fun_class=c("Expressway", "Major Arterial", "Minor Arterial", "Major Collector",  "Minor Collector", "Local Road", "Omit From Analysis"),
    crash_colors=c('#66a61e', '#e6ab02', '#d95f02', '#7570b3', '#e7298a'),
    crash_col_levs = c('Property Damage Only (O)', 'Possible Injury (C)', 'Non-Incapacitating Injury (B)', 'Incapacitating Injury (A)', 'Fatality (K)'),
    sql_identifier_local_user_data_schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'),
    hidden_counter = 0
  )
  
  # try to fix issue with map 
  shinyjs::runjs(code = paste0('$("#tab-load_data").click(function(){$("#', session$ns('data_map'), '").trigger("shown");})'))

  data$time <- Sys.time()
  output$last_updated <- renderText({
    glue::glue('Table last updated: {data$time}')
  })
  
  # resizes table 
  shinyjs::runjs(code = paste0('$("#load_data_ui_1-data_explorer_tab_select > li:nth-child(4) > a").click(function(){$("#', session$ns('perform_checks_hidden'), '").click();})'))
      output$road_fclass_mapped_vars <- DT::renderDataTable({ NULL}, options = list(pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}")))
      output$road_fclass_mapped_vars_check <- renderUI({ HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))}) 
 
  # this function is called when the user clicks the refresh button 
  perform_checks_f <- function(){
    fclass_column = NULL
    mode_column = NULL
    sev_column = NULL
    list_of_tables <- create_list_user_tbls(user_id=user_id, run_id=run_id,  list_of_tbl_prefixes=c('roads', 'crashes', 'study_area'))
    t_check <- check_for_user_inputs(connection=connection, user_id=user_id, run_id=run_id, schema=data$sql_literal_local_user_data_schema, list_of_tables=list_of_tables)
    data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
    data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
    data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
    fclass_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'roads_fun_c_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id))
    mode_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'crash_o_mode_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id))
    sev_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'crash_o_serv_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id))
    
    data$crs <- get_account_crs(
      connection = connection, 
      user_id = user_id,  
      run_id = data$run_id_sql_formatted
    )
    if (data$sa_exists) {
      psql_update_epsg(connection=connection, table=data$sql_identifier_sa, new_epsg=data$crs)
    }
   
    # This section populates the crash data related tables that summarize how the user mapped thier data.
      crash_cost_crosswalk_t <- NULL
      crash_mode_crosswalk_t <- NULL
      crash_sev_crosswalk_t <- NULL
    if (data$cr_exists && !is.null(mode_column) && !is.null(sev_column)) {
      psql_update_epsg(connection=connection, table=data$sql_identifier_crashes, new_epsg=data$crs)
      

      crash_cost_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                           schema = data$sql_identifier_local_user_data_schema,
                                                           table = data$sql_identifier_crashes,
                                                           values = c('severity_mapped', 'crashes_costs_usdot'))
      names(crash_cost_crosswalk_t)[names(crash_cost_crosswalk_t)=="crashes_costs_usdot"] <- "Crash Cost"
      names(crash_cost_crosswalk_t)[names(crash_cost_crosswalk_t)=="severity_mapped"] <- "Severity"
      


      crash_mode_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                           schema = data$sql_identifier_local_user_data_schema,
                                                           table = data$sql_identifier_crashes,
                                                           values = c(glue::glue('{mode_column}'), 'usdot_mode_mapped'))
      names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)=="severity_mapped"] <- "Standard Mode"
      names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)==glue::glue("{mode_column}")] <- "Your Dataset's Mode"
      names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)=="usdot_mode_mapped"] <- "Standard Mode"
      


      crash_sev_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                          schema = data$sql_identifier_local_user_data_schema,
                                                          table = data$sql_identifier_crashes,
                                                          values = c(glue::glue('{sev_column}'), 'severity_mapped'))
      names(crash_sev_crosswalk_t)[names(crash_sev_crosswalk_t)=="severity_mapped"] <- "Standard Severity"
      names(crash_sev_crosswalk_t)[names(crash_sev_crosswalk_t)==glue::glue("{sev_column}")] <- "Your Dataset's Severity"
      


  
    }
    road_fclass_crosswalk_t <- NULL
    if (data$rd_exists && !is.null(fclass_column)) {
      road_fclass_crosswalk_t <- get_crosswalk_table_lines(connection=connection, 
                                                           schema = data$sql_identifier_local_user_data_schema,
                                                           table = data$sql_identifier_roads,
                                                           values = c(glue::glue('{fclass_column}'), 'usdot_fun_class_mapped'))
      names(road_fclass_crosswalk_t)[names(road_fclass_crosswalk_t)=="usdot_fun_class_mapped"] <- "Standard Functional Class"
      names(road_fclass_crosswalk_t)[names(road_fclass_crosswalk_t)==glue::glue("{fclass_column}")] <- "Your Dataset's Functional Class"
      road_fclass_crosswalk_t[['Total Miles']] <- round(as.numeric(road_fclass_crosswalk_t[['Total Miles']]), 2)
      psql_update_epsg(connection=connection, table=data$sql_identifier_roads, new_epsg=data$crs)

    }
 
    if (!is.null( crash_mode_crosswalk_t )) {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_null'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_null'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_active'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_active'), '").addClass("leaflet_block");'))
      output$crash_mode_mapped_vars <- DT::renderDataTable({ crash_mode_crosswalk_t }, options = list(pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}")))
 
    } else {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_null'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_null'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_active'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_mode_crosswalk_t_active'), '").addClass("leaflet_none");'))
    }
    
    if (!is.null(crash_cost_crosswalk_t)) {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_null'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_null'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_active'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_active'), '").addClass("leaflet_block");'))
      output$crash_cost_mapped_vars <- DT::renderDataTable({ crash_cost_crosswalk_t }, options = list(pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}")))

    } else {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_null'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_null'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_active'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_cost_crosswalk_t_active'), '").addClass("leaflet_none");'))
    }


    if (!is.null( road_fclass_crosswalk_t )) {
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_null'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_null'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_active'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_active'), '").addClass("leaflet_block");'))
      output$road_fclass_mapped_vars <- DT::renderDataTable({ road_fclass_crosswalk_t }, options = list(pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}")))
          
    } else {
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_null'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_null'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_active'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('road_fclass_crosswalk_t_active'), '").addClass("leaflet_none");'))
    }

    if (!is.null( crash_sev_crosswalk_t )) {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_null'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_null'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_active'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_active'), '").addClass("leaflet_block");'))
      output$crash_sev_mapped_vars <- DT::renderDataTable({ crash_sev_crosswalk_t }, options = list(pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}")))
         
    } else {
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_null'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_null'), '").addClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_active'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_sev_crosswalk_t_active'), '").addClass("leaflet_none");'))
      # output$crash_sev_mapped_vars <- DT::renderDataTable({ NULL }, options = list(pageLength = 15, dom = 't'))
      # output$crash_sev_mapped_vars_check <- renderUI({ HTML(paste(tags$span(style="color:#ed921a", "Fetch or upload your data to explore mapped values."), sep = ""))}) 
    }

    # general check for tables
    
    if (!data$sa_exists || !data$cr_exists || !data$rd_exists) {
      
      output$all_data_exists_check <- renderUI({
        output = tagList()
        output[['general_message']] = HTML('<i class="fa fa-times" style="color: red" aria-hidden="true"> <div style="display: inline; color: black;">At least one of your datasets is missing. Go back to the Load Data tab to resolve this issue.</div>')
        if (!data$sa_exists ) {
          output[['study_area']] = HTML('<br>&emsp;Your <strong>study area</strong> is missing.')
        } 
        if (!data$cr_exists ) {
          output[['crashes']] = HTML('<br>&emsp;Your <strong>crash</strong> data are missing.')
        } 
        if (!data$rd_exists){
          output[['roads']] = HTML('<br>&emsp;Your <strong>roads</strong> data are missing.')
        } 
        output 
      })
      waiter::waiter_hide()
    } else {


      # test for intersects among uploaded data
      roads_in_study_area <- intersect_server_side(connection = connection,
                                                   user_id = user_id,
                                                   run_id = data$run_id_sql_formatted,
                                                   schema1 = 'local_user_data',
                                                   table1  = glue::glue('roads_{user_id}_{run_id}'),
                                                   schema2 = 'local_user_data',
                                                   table2  = glue::glue('study_area_{user_id}_{run_id}')
      )

      crashes_in_study_area <- intersect_server_side(connection = connection,
                                                     user_id = user_id,
                                                     run_id = data$run_id_sql_formatted,
                                                     schema1 = 'local_user_data',
                                                     table1  = glue::glue('crashes_{user_id}_{run_id}'),
                                                     schema2 = 'local_user_data',
                                                     table2  = glue::glue('study_area_{user_id}_{run_id}')
      )
 
      crashes_near_roads <- intersect_server_side(connection = connection, 
                                                     user_id = user_id, 
                                                     run_id = data$run_id_sql_formatted, 
                                                     schema1 = 'local_user_data',
                                                     table1  = glue::glue('crashes_{user_id}_{run_id}'),
                                                     schema2 = 'local_user_data',
                                                     table2  = glue::glue('roads_{user_id}_{run_id}')
      ) 
      
      c_check <- check_crs_of_tables(connection=connection, user_id=user_id, run_id=run_id, list_of_tables=list_of_tables)
      dupid_check <- FALSE
      
      output$all_data_exists_check <- renderUI({
        HTML('<i class="fa fa-check" style="color: green; display: inline;"  aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;You have uploaded all your data.</div>')
      })
      # check for intersects
      if ( !roads_in_study_area ) {
        output$within_spatial_bounds_check1 <- renderUI({
          HTML('<i class="fa fa-times" style="color: red; display: inline;"  aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;It doesn\'t look like your roads data intersects with your study area. Ensure your data overlaps.</div>')
        })
      } else {
        output$within_spatial_bounds_check1 <- renderUI({
          HTML('<i class="fa fa-check" style="color: green; display: inline;"  aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;Your roads data intersects with your study area.</div>')
        })
      }
      if ( !crashes_in_study_area ) {
        output$within_spatial_bounds_check2 <- renderUI({
          HTML('<i class="fa fa-times" style="color: red; display: inline;"  aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;It doesn\'t look like your crash data intersects with your study area. Eensure your data overlaps.</div>')
        })
      } else {
        output$within_spatial_bounds_check2 <- renderUI({
          HTML('<i class="fa fa-check" style="color: green; display: inline;"  aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;Your crash data intersects with your study area.</div>')
        })
      }
      if ( !crashes_near_roads ) {
        output$within_spatial_bounds_check3 <- renderUI({
          HTML('<i class="fa fa-times" style="color: red; display: inline;" aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;It doesn\'t look like your crash and roads data are near each other. Ensure your data overlap.</div>')
        })
      } else {
        output$within_spatial_bounds_check3 <- renderUI({
          HTML('<i class="fa fa-check" style="color: green; display: inline;" aria-hidden="true"> <div style="display: inline; color: black;">&nbsp;&nbsp;Great, your roads and crash data are close enough to each other.</div>')
        })
      }}
    # map the data in the configuration page 
 
    shinyjs::runjs(code = paste0('$("#', session$ns('viz_model_inputs'), '").click();'))
    
  }
  
  # this function is called when the user wants to update the data management table 
  update_data_mgmt <- function(){
    data$user_table <- get_user_data(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted)
    data$user_table <- data$user_table %>% dplyr::mutate(dplyr::across(everything(), as.character))
    data$user_table <- data$user_table %>% tidyr::pivot_longer(cols=c(1:15))
    names(data$user_table)[names(data$user_table)=="name"] <- "Variable"
    names(data$user_table)[names(data$user_table)=="value"] <- "Value"
    
    output$table_user_data = DT::renderDataTable({
      data$user_table
    },     options = list(
      pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}"))
    )
    
    # update 'table last updated'
    data$time <- Sys.time()
    output$last_updated <- renderText({
      glue::glue('Table last updated: {data$time}')
    })
  }
  
  # silent function that's called when the user first navigates to the tab for the first time 
  observeEvent(input$perform_checks_hidden, {
    print(data$hidden_counter)
    if ( data$hidden_counter < 1 ){
          waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
      perform_checks_f()
      update_data_mgmt()
      data$hidden_counter <- 1
      print(data$hidden_counter)
      waiter::waiter_hide()
    }
  })

  # Instructions 
  inputs_instructions_modal <- modalDialog(
    title = "Instructions to Perform Checks",
    easyClose = TRUE,
    next_label = NULL,
    tagList(
      tags$div( 
               p("The Perform Checks button ensures:"),
               tags$ol(
                 tags$li("all three of the necessary datasets are uploaded,"),
                 tags$li("that all datasets are in the correct Coordinate Reference System (they will be automatically re-projected if this is not the case),"),
                 tags$li("and that all three datasets cover the same area."),
               ),
               p("If all three datasets are not uploaded, you must go back to prior steps to upload them."),
               p("If all three datasets do not cover the same area, you must address this outside of the tool.")
      )
    ),
    footer = tagList(
      actionButton(ns("ok"), "OK")
    )
  )
  
  # opens instructions 
  observeEvent(input$input_instructions, {
    showModal(inputs_instructions_modal)
  })
  
  # removes instructions 
  observeEvent(input$ok, {
    removeModal()
  })
  
  # handles decision to provide data for research 
  observeEvent(input$keep_data_selection, {
    if (input$keep_data_selection == 'Yes, you may save copies of my data for research!') {
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'sa_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'FALSE'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'roads_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'FALSE'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'FALSE'))
      
    } else if (input$keep_data_selection == 'No, my data may not be saved.') {
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'sa_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'TRUE'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'roads_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'TRUE'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'crash_storage_opt_out'), new_value=DBI::dbQuoteString(connection,'TRUE'))
      
    }
  })
  
  # this block grabs general user account information for display
  data$user_table <- get_user_data(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted)
  data$user_table <- data$user_table %>% dplyr::mutate(dplyr::across(everything(), as.character))
  data$user_table <- data$user_table %>% tidyr::pivot_longer(cols=c(1:15))
  
  # renders the information above
  output$table_user_data = DT::renderDataTable({
     data$user_table
  },     options = list(
    pageLength = 15, dom = 't', initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().body()).css({'font-size': '90%'});",
      "$(this.api().table().header()).css({'font-size': '95%'});",
      "}"))
  )

  
  # performs several checks on the data for analysis.
  observeEvent(input$perform_checks, {
      waiter::waiter_show(
        color='rgba(175, 175, 175, 0.85)',
        html = tagList(
          tags$div(waiter::spin_1()),
          tags$br(),
          tags$div(HTML("Loading ..."))
        )
      )
      perform_checks_f()
      update_data_mgmt()
      data$hidden_counter <- 1
      waiter::waiter_hide()
  })
    
  # visualization 
  observeEvent(input$viz_model_inputs, {
    data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
    data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
    data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
    # clear map 
    
    if (!data$sa_exists && !data$cr_exists && !data$rd_exists) {
      shiny_warming_alert(title = 'Whoa!', text='You don\'t have any data to map. Please upload data.')
    } else {
      if(data$hidden_counter > 0) {
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Fetching Data ..."))
          )
        ) 
      }
      
      leafletProxy("data_map", session) %>%
        leaflet::clearShapes()  %>% 
        leafgl::clearGlLayers() %>%
        leaflet::clearControls()  %>%
        addControl(html = actionButton(ns("viz_model_inputs"),"Map Data", icon = icon("refresh")), position = "topleft")
      
      # get roads if exists 
      if ( data$rd_exists ) {
        r_name <- paste0('roads_', user_id, '_', run_id)
        data$the_roads <- fetch_spatial_table(connection = connection,
                                              columns= 'ST_AsEWKT((ST_Dump(ST_Simplify(geom, .1))).geom) as geom, usdot_fun_class_mapped', 
                                              schema = 'local_user_data',
                                              table =  r_name,
                                              geom_type='LINESTRING',
                                              is_wkt=T
        )
        
        
        data$the_roads <- transform_with_epsg(data$the_roads, 4326)
        
        palette <- colorFactor(palette=data$fclass_colors, levels=data$fun_class)
        ("Done downloading the roads")
        
        
        
        roads <- data$the_roads %>% arrange(usdot_fun_class_mapped)
        leafletProxy("data_map", session) %>%
          leafgl::addGlPolylines(data=roads, 
                                 color = ~palette(roads$usdot_fun_class_mapped),
                                 opacity = 1, 
                                 weight = 1,
                                 group='Roads'
          ) %>%  
          addLegend(position = "bottomright",
                    pal = palette, 
                    values = roads$usdot_fun_class_mapped,
                    title =  "Roads"
          )  
        
      }
      

      
      # get study area if exists 
      if ( data$sa_exists ) {
        sa_name <- paste0('study_area_', user_id, '_', run_id)
        data$the_study_area <- fetch_spatial_table(connection = connection,
                                                   columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                                   schema = 'local_user_data',
                                                   table =  sa_name,
                                                   geom_type='POLYGON',
                                                   is_wkt=TRUE
        )
        
        data$the_study_area <- transform_with_epsg(data$the_study_area, 4326)
        ("Done downloading the study area.")
 
        leafletProxy("data_map", session) %>%
          leaflet::addPolygons(data=data$the_study_area, 
                               fillColor = "#666566", 
                               color = '#666566',
                               stroke=TRUE,
                               fillOpacity = 0,
                               opacity = .8,
                               weight = 2,
                               group = 'Study Area'
          )  
      }
      
      
      # get crashes if exists 
      if ( data$cr_exists ) {
        c_name <- paste0('crashes_', user_id, '_', run_id)
        data$the_crashes <- fetch_spatial_table(connection = connection,
                                                columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom, severity_mapped, crashes_costs_usdot', 
                                                schema = 'local_user_data',
                                                table =  c_name,
                                                geom_type='POINT',
                                                is_wkt=TRUE,
                                                clauses = glue::glue('WHERE severity_mapped != \'Property Damage Only (O)\' 
                                                                      AND severity_mapped != \'Omit From Analysis\'
                                                                      AND in_sa_{user_id}_{run_id}')
        )
        data$the_crashes <- transform_with_epsg(data$the_crashes, 4326)
        palette_crashes <- colorFactor(palette=data$crash_colors, levels=data$crash_col_levs)
        
        ("Done downloading the crashes.")
        
        leafletProxy("data_map", session) %>%
          leafgl::addGlPoints(data=data$the_crashes, 
                              radius=7, 
                              opacity=1, 
                              color='#a6d854', 
                              fillColor=palette_crashes(data$the_crashes$severity_mapped),
                              fillOpacity=1,
                              group='Crashes') %>% 
          addLegend(position = "bottomright",
                    pal = palette_crashes, 
                    values = data$the_crashes$severity_mapped,
                    title =  "Crashes"
          )
      }
      
      groups <- c()
      bboxes <- c()
      if (data$sa_exists) {
        groups <- append(groups, 'Study Area')
        bboxes$sa <- sf::st_bbox(data$the_study_area) 
      } 
      if (data$cr_exists ){
        groups <- append(groups, 'Crashes')
        bboxes$cr <- sf::st_bbox(data$the_crashes)
      }
      if ( data$rd_exists ) {
        groups <- append(groups, 'Roads')
        bboxes$rd <- sf::st_bbox(data$the_roads) 
      }
      box <- get_max_bounds(bboxes)

      leafletProxy("data_map", session) %>%
      leaflet::fitBounds(box[1], box[2], box[3], box[4])  %>% 
      leaflet::addLayersControl(baseGroups = c("Grey", "Negative", "OpenStreetMap"), 
                                overlayGroups = groups,
                                options = layersControlOptions(collapsed = F),
                                position = "topright")
    }
    if(data$hidden_counter > 0) {
      waiter::waiter_hide()
    }
  }) 
  
  # Navigates user to next step 
  observeEvent(input$to_analysis, {
    shinyjs::runjs(code = ' $(\'#tab-analysis_production\').click(); ')  
  })
 
  # Updates table 
  observeEvent(input$update_data_review_table, {
    data$user_table <- get_user_data(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted)
    data$user_table <- data$user_table %>% dplyr::mutate(dplyr::across(everything(), as.character))
    data$user_table <- data$user_table %>% tidyr::pivot_longer(cols=c(1:15))
    names(data$user_table)[names(data$user_table)=="name"] <- "Variable"
    names(data$user_table)[names(data$user_table)=="value"] <- "Value"
    
    output$table_user_data = DT::renderDataTable({
      data$user_table
    },     options = list(
      pageLength = 15, dom = 't', initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().body()).css({'font-size': '90%'});",
        "$(this.api().table().header()).css({'font-size': '95%'});",
        "}"))
    )
    
    # update 'table last updated'
    data$time <- Sys.time()
    output$last_updated <- renderText({
      glue::glue('Table last updated: {data$time}')
    })
    shiny_warming_alert(title='Complete', text='Table Updated', showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
    
  })
 
 
  # custom map 
  output$data_map <- leaflet::renderLeaflet({
    leaflet(options = leaflet::leafletOptions(zoomControl = FALSE)) %>%
      leaflet::addProviderTiles(providers$CartoDB.DarkMatter, group = 'Negative') %>%
      leaflet::addTiles(group = "OpenStreetMap") %>%
      leaflet::addProviderTiles(providers$CartoDB.Positron, group = 'Grey', options = providerTileOptions(noWrap = TRUE)) %>%
      leaflet::addLayersControl(baseGroups = c("Grey", "Negative",  "OpenStreetMap"), position = 'topright') %>%
      leaflet::fitBounds(-125.0, 25.0, -66.96, 49.5) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomright' }).addTo(this)
    }") %>%
      addControl(html = actionButton(ns("viz_model_inputs"),"Map Data", icon = icon("refresh")), position = "topleft")
  })
  
  outputOptions(output, "data_map", suspendWhenHidden = FALSE)
  outputOptions(output, "table_user_data", suspendWhenHidden = FALSE)
  shinyjs::runjs(code = paste0('$("#load_data_ui_1-data_explorer_tab_select > li:nth-child(4) > a").click(function(){$("#', session$ns('data_map'), '").trigger("shown");})'))
  shinyjs::runjs(code = paste0('$("#tab-load_data").click(function(){$("#', session$ns('data_map'), '").trigger("shown");})'))
    
}
    
## To be copied in the UI
# mod_confirm_inputs_ui("confirm_inputs_ui_1")
    
## To be copied in the server
# callModule(mod_confirm_inputs_server, "confirm_inputs_ui_1")
 
