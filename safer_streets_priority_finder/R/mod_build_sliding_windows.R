#' build_sliding_windows UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import DBI
mod_build_sliding_windows_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        h5('Sliding Windows Analysis'),
        tags$div(br()),
        bs4Card(inputId=ns('into_card_model'), title='Summary', width = 12,  closable = F, 
                fluidRow(
                  col_12(
                    tags$ul(
                      tags$li(
                        "This analysis takes the crashes and roads data within the study area and allocates the crashes to roads, measured on 1/2- mile sliding window segments stepped in 1/10-mile increments along the network."
                      ),
                      tags$li(
                        "The Sliding Windows Score weights the most severe crashes more heavily than lower severity crashes. The Sliding Windows Score is calculated by multiplying the number of Fatal (K) and Incapacitating Injury (A) crashes by 3, and multiplying the number of Non-Incapacitating Injury (B) crashes by 1. Once the weights are established and applied to the crashes, the total number of crashes are aggregated along a corridor while incorporating the crash severity weighting. Possible Injury (C) and Property Damage Only (O) Crashes are not reflected."
                      ),
                      tags$li(
                        "If you used FARS data alone, only fatal crashes will have been used and visualized."
                      ),
                    )
                  )),
                fluidRow(
                  
                  col_12(
                    uiOutput(ns("sliding_windows_controls"))
                  )
                ),
                
                fluidRow(
                  col_12(
                    uiOutput(ns("status_report")),
                  ))),
        div(id = ns('sliding_windows_map_ui'),
            fluidRow(
              col_12(
                bs4Card(inputId=ns('map_sliding_windows'), title='Sliding Windows Map', width = 12,  closable = F, 
                        fluidRow(
                          col_12(
                            p("This map depicts severity-weighted pedestrian/bicycle/other crashes (including severities K, A, and B) per mile. 
                              Only segments with a crash score of 1 or more are visualized (please disregard the value of zero shown in the legend). 
                              Note that road geometries are simplified in order to visualize them in the browser. Additional information will pop-up if you click on a roadway segment score in the map."),
                            hr(),
                            shinycssloaders::withSpinner(
                            leafletOutput(ns("sliding_windows_map"), height=750, width='100%')
                            )
                          )))
              )
            )
        )
      )))
}
    
#' build_sliding_windows Server Functions
#'
#' @noRd 
mod_build_sliding_windows_server <- function(input, output, session, connection, user_id, run_id){
 
    ns <- session$ns
    data <- reactiveValues(
      hin_sliding_windows_sql_formatted=DBI::dbQuoteLiteral(connection, return_table_name('sw_sliding_windows', user_id, run_id)),
      sql_literal_sw_output_schema=DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs'),
      run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
      run_inputs=NULL,
      number_nulls=NULL,
      hin_network=NULL,
      hin_sliding_windows_existing=FALSE,
      sql_literal_study_area=DBI::dbQuoteLiteral(connection, return_table_name('study_area', user_id, run_id)),
      sql_literal_roads=DBI::dbQuoteLiteral(connection, return_table_name('roads', user_id, run_id)),
      sql_literal_crashes=DBI::dbQuoteLiteral(connection, return_table_name('crashes', user_id, run_id)),
      sql_literal_local_user_data_schema=DBI::dbQuoteLiteral(connection, 'local_user_data'),
      sql_literal_scratch_schema=DBI::dbQuoteLiteral(connection, 'scratch'),
      sa_exists=F,
      cr_exists=F,
      rd_exists=F,
      sw_exists=F,
      clicks_build_hin=input$build_hin,
      n = 1
    )
    
    # tests if results exist, if so buttons response 
    data$hin_sliding_windows_existing <- test_if_table_exists(connection = connection, schema = data$sql_literal_sw_output_schema, table = data$hin_sliding_windows_sql_formatted)
 
    # renders buttons available based on presents of sliding windows results or not 
    render_base_ui_controls <- function(build='Rebuild', exists=F){
      if (exists) {

        output$sliding_windows_controls <- renderUI({
          fluidRow(
            col_3(
              actionButton(ns("build_hin"), "Rebuild the Sliding Windows Analysis", class = "btn btn-primary")
            ),
            col_3(
              actionButton(ns("pull_existing"), "Visualize Results of Sliding Windows Analysis", class = "btn btn-primary hide")
            ),
            col_3(
              actionButton(ns("move_to_modeling"), "Skip to Modeling", class = "btn btn-primary")
            )
          )
        })
      } else {
        output$sliding_windows_controls <- renderUI({
          fluidRow(
            col_3(
              actionButton(ns("build_hin"), paste0(build, " the Sliding Windows Analysis"), class = "btn btn-primary")
            ),
            col_3(
              actionButton(ns("move_to_modeling"), "Skip to Modeling", class = "btn btn-primary")
            )
          )
        })
      }
    }
    
    # navigation button 
    observeEvent(input$move_to_modeling, {
      shinyjs::runjs(code = ' $(\'#data_explorer_tab_select > li:nth-child(2) > a\').click(); ')  
    })
    
    # updates buttons based on whether sliding windows results exist or now
    observe({
      if(!data$hin_sliding_windows_existing ){
        render_base_ui_controls('Build')
      } else if (data$hin_sliding_windows_existing && (is.null(data$clicks_build_hin))) {
        render_base_ui_controls('Rebuild', exists=T)
      } else {
        output$sliding_windows_controls <- renderUI({
          render_base_ui_controls('Rebuild', exists=T)
        })
      }
    })
     
    # confirms whether user wants to delete results 
    ready_results_in_sw_tab <- modalDialog(
      title = "Remove Results?",
      easyClose = FALSE,
      footer = NULL,
      next_label = NULL,
      # create login 
      tagList(
        tags$div(class = 'login_well',
                 p('You already have results from a sliding windows analysis. By proceeding, you will delete your current results and rebuild the analysis with your current data.')
        ),
        fluidRow(
          column(12, align="center",
                 tags$div(style="display:inline-block",
                          id = "access_buttons", class = 'login_buttons leaflet_block',
                          actionButton(ns("ready_delete_results"), "Yes, Delete Results", class = "btn btn-primary"),
                          actionButton(ns("ready_dont_delete_results"), "No, Keep My Results", class = "btn btn-primary")
                          )
          ))
      )
    )
    
    # long job of creating sliding windows 
    long_job_sliding_windows <- function(
      user_id,
      run_id
    ) {
      c <- DBI::dbConnect(
        drv =  RPostgreSQL::PostgreSQL(),
        dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
        host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
        user = Sys.getenv("SSPF_AMAZON_USERNAME"),
        password = Sys.getenv("SSPF_AMAZON_PASSWORD")
      )
      
      source(file.path(getwd(), 'R', 'fct_proj_funcs.R'), local = TRUE)
      source(file.path(getwd(), 'R', 'fct_db_utils.R'), local = TRUE)
      source(file.path(getwd(), 'R', 'fct_sliding_windows_tool.R'), local = TRUE)
      source(file.path(getwd(), 'R', 'fct_helpr_funs.R'), local = TRUE)
      
      crs <- get_account_crs(
        connection = c, 
        user_id = user_id,  
        run_id = DBI::dbQuoteLiteral(c, run_id)
      )

      run_inputs <- hin_0_update_inputs(user_id=user_id, run_id=run_id, con=c)
      hin_run_sliding_windows(con=c, inputs=run_inputs, user_id=user_id, run_id=run_id)
      update_account_info(connection=c, user_id=user_id, run_id=DBI::dbQuoteLiteral(c, run_id), column = DBI::dbQuoteIdentifier(c, 'move_windows_long_comp'), new_value='TRUE')
      DBI::dbDisconnect(c)
    }
 
    # once results are ready, bring them into the UI 
    visulize_sw_results <- function(){
      leafletProxy("sliding_windows_map") %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() 
      callModule(mod_visualize_sliding_windows_server, 
                 "visualize_sliding_windows_ui_1", 
                 connection=connection, 
                 user_id=user_id, 
                 run_id=run_id, 
                 data=data, 
                 leaflet_proxy = leafletProxy("sliding_windows_map", session),
                 n = data$n)
      render_base_ui_controls('Rebuild', exists=T)
      data$n <- data$n + 1
    }
    
    # sets up background process to build sliding windows 
    long_run_sw <- eventReactive( input$build_hin, {
      #model_status <- model_production_status(connection, user_id, run_id)
      data$sw_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_output_schema, table=data$hin_sliding_windows_sql_formatted)
      if (data$sw_exists) {
        showModal(ready_results_in_sw_tab)
      } else {
      data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
      data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
      # general check for tables
      if (!data$sa_exists || !data$cr_exists || !data$rd_exists) {
        shiny_warming_alert(title = 'Missing Data', text="You're missing data. If you're not sure what you're missing, go to the 'Review Inputs' tab and click the 'Map Data' button.")
      } else {
        tryCatch({
          data$w <- waiter::Waiter$new(html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Setting up to build the analysis..."))
          ),  
          color='rgba(175, 175, 175, 0.85)')
          data$w$show()
          x <- callr::r_bg(
            func = long_job_sliding_windows,
            supervise = TRUE,
            args=list(user_id=user_id,
                      run_id=run_id),
            package=TRUE
          )
          check_status$v=TRUE
          data$w$update(html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML('Building the sliding windows analysis...'))
          ))
          return(x)
        }, error = function(cond){
          print(cond)
        })
      }
      }
    })
    
    # controlers for listening for when background process has finished 
    done <- reactiveValues(v=FALSE)
    check_status <- reactiveValues(v=FALSE)
    observe({long_run_sw()})
    
    # function to monitor for completed background process 
    check <- reactive({
      invalidateLater(millis = 1000, session = session)
      if(check_status$v==TRUE){
        if (long_run_sw()$is_alive()) {
          long_job_status <- "Job running in background"
        } else {
          long_job_status <- "Async job in background completed"
          print(long_run_sw()$get_result())
          done$v = TRUE
          check_status$v = FALSE
        }
        return(long_job_status)
      } else {
        return('No Job')
      }
    })
    
    # monitor for completed background process 
    observe({
      check()
      if(done$v==TRUE) {
        output$sliding_windows_controls <- renderUI({
          fluidRow(
            col_3(
              actionButton(ns("build_hin"), "Rebuild the Sliding Windows Analysis", class = "btn btn-primary")
            ),
            col_3(
              actionButton(ns("move_to_modeling"), "Skip to Modeling", class = "btn btn-primary")
            )
          )
        })
        data$w$update(html = tagList(
          tags$div(waiter::spin_1()),
          tags$br(),
          tags$div(HTML('Fetching Results...'))
        ))
        visulize_sw_results()
 
        data$w$hide()
        print('done loading crashes')
        done$v = FALSE
      }
    })
    
    # this is called if results are ready and the user wants to rebuild sliding windows results 
    observeEvent( input$ready_delete_results, {
      removeModal()
      data$hin_sliding_windows_existing <- test_if_table_exists(connection = connection, schema = data$sql_literal_sw_output_schema, table = data$hin_sliding_windows_sql_formatted)
      shiny_warming_alert(title='Results Deleted', text="Proceeding", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
      delete_tables(connection, list_of_tables=list(c('sliding_windows_outputs', glue::glue('sw_sliding_windows_{user_id}_{run_id}')), c('scratch', glue::glue('hin_sliding_windows_{user_id}_{run_id}'), c('scratch', glue::glue('hin_crashes_{user_id}_{run_id}')))))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'move_windows_long_comp'), new_value='FALSE')
      shinyjs::runjs(code = ' $(\'#build_sliding_windows_ui_1-build_hin\').click(); ')  
      })

    # removes modal if user wants to keep the results 
    observeEvent(input$ready_dont_delete_results, {
      removeModal()
    })
    
    # visualization listener 
    observeEvent(input$pull_existing, {
      data$hin_sliding_windows_existing <- test_if_table_exists(connection = connection, schema = data$sql_literal_sw_output_schema, table = data$hin_sliding_windows_sql_formatted)
      if (data$hin_sliding_windows_existing) {
      tryCatch({
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Fetching Data ..."))
          )
        )
        visulize_sw_results()
      }, error = function(cond){
        print(cond)
        waiter::waiter_hide()
      })
      waiter::waiter_hide()
      } else {
        shiny_warming_alert(title='No Data', text="Sliding window results do not exist.", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2500, type="success")
      }
    })
    
    # render map 
    render_map(output, 'sliding_windows_map') 
    outputOptions(output, "sliding_windows_map", suspendWhenHidden = FALSE)
    shinyjs::runjs(code = paste0('$("#tab-analysis_production").click(function(){$("#', session$ns('sliding_windows_map'), '").trigger("shown");})'))
  
}
    
## To be copied in the UI
# mod_build_sliding_windows_ui("build_sliding_windows_ui_1")
    
## To be copied in the server
# mod_build_sliding_windows_server("build_sliding_windows_ui_1")
