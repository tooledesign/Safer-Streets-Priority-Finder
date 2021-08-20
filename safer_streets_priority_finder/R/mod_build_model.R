#' build_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import future
#' @import promises
#' @import DBI
#' @import callr
mod_build_model_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_12(
        h5("Safer Streets Model"),
        tags$div(br()),
             bs4Card(inputId=ns('into_card'), title='Summary', width = 12, closable = F,
                     
                     fluidRow(
                       col_12(
                         p("This analysis uses a Bayesian modeling framework to assign risk values to segments for different severities of crashes over a one-year period. These values are then converted to crash cost estimates based on costs associated with each crash severity. Click the Build Model button below to begin."),
                       )),
                     uiOutput(ns("gen_ui_comp"))

                     )
             )
      ),
 
    fluidRow(
      col_12(
             bs4Card(inputId=ns('model_map_ped_card'), title='Estimated One-Year Crash Cost Map - Caution - Cost Outputs Are Still In Beta Testing', width = 12, closable = F,
                     p('Once your model results are ready, you\'ll have a chance to visualize the results on this map. A visualization button will appear on login once your results are ready. Only segments with an estimated annual average cost per mile of $120,563 or greater are visualized. This amount reflects the default value for C - possible injury crashes. For models in rural areas or areas with lower observed crashes, the results may not appear, but can be viewed in GIS software once downloaded.  Additional information will pop-up if you click on a roadway segment score in the map.'),
                     tags$div(hr()),   
                     shinycssloaders::withSpinner(
             leafletOutput(ns("model_map"), height=750, width='100%')))
      )
      )
  )
}
    
#' build_model Server Functions
#'
#' @noRd 
mod_build_model_server <- function(input, output, session, connection, user_id, run_id){

    ns <- session$ns
    
    # reactive variables 
    data <- reactiveValues(
      output_roads_sql_literal = DBI::dbQuoteLiteral(connection, return_table_name('hin_output_roads', user_id, run_id)),
      ouput_roads = return_table_name('hin_output_roads', user_id, run_id),
      run_id_sql_formatted=DBI::dbQuoteLiteral(connection, run_id),
      run_inputs=NULL,
      hin_network=NULL,
      sql_literal_study_area=DBI::dbQuoteLiteral(connection, return_table_name('study_area', user_id, run_id)),
      sql_literal_roads=DBI::dbQuoteLiteral(connection, return_table_name('roads', user_id, run_id)),
      sql_literal_crashes=DBI::dbQuoteLiteral(connection, return_table_name('crashes', user_id, run_id)),
      sql_literal_local_user_data_schema=DBI::dbQuoteLiteral(connection, 'local_user_data'),
      sql_literal_scratch_model_outputs_schema=DBI::dbQuoteLiteral(connection, 'model_output_scratch'),
      sql_literal_model_outputs_schema=DBI::dbQuoteLiteral(connection, 'model_outputs'),
      sa_exists=F,
      cr_exists=F,
      rd_exists=F,
      # model_info=get_model_timeline_information(connection, user_id, run_id),
      model_results=NULL,
      model_processing=model_production_status(connection, user_id, run_id),
      crs=NULL,
      mr_exists=F,
      n = 1
    )
    
    # set of instructions for the user.
    model_instructions_modal <- modalDialog(
      title = "Uploading Roads Data",
      easyClose = TRUE,
      next_label = NULL,
      tagList(
        tags$div( 
                 p("The model execution takes several hours, and up to a full day to run. You'll receive an email notification once you begin the model production process and another email once the model is complete and ready for review. You can close the webpage and the model will continue to process. Please check your junk email inbox if you do not see the email notifications. ")
        )
      ),
      footer = tagList(
        actionButton(ns("ok"), "OK")
      )
    )
    
    # Listens for user to click instructions button, when done so, instructions are shown 
    observeEvent(input$model_instructions, {
      showModal(model_instructions_modal)
    })
    
    # close modal button closes the modal 
    observeEvent(input$ok, {
      removeModal()
    })
    
    # navigates user to next step 
    observeEvent(input$to_dashbaord, {
      shinyjs::runjs(code = ' $(\'#tab-manage_data\').click(); ')  
    })
    
    # creates buttons based on where the user is in the modeling process
   observe({
     if (data$model_processing == 'model_estimation_completed') {
       output$gen_ui_comp <- renderUI({
         fluidRow(
           col_3(
             actionButton(ns("build_model"), "Rebuild Safer Streets Model", class = "btn btn-primary leaflet_block"),
             uiOutput(ns("build_model_process"), class='leaflet_none'),
           ),
           col_3(
             actionButton(ns("pull_existing_model_results"), "Visualize Results of Model", class = "btn btn-primary")
           ),
           col_3(
             actionButton(ns("to_dashbaord"), "Jump to Dashboard", class = "btn btn-primary")
           ),
           col_3(
             actionButton(ns("model_instructions"), "Instructions", class = "btn btn-primary")
           )
         )
       })
     } else if (data$model_processing == 'model_needed' ) {
       output$gen_ui_comp <- renderUI({
         fluidRow(
           col_3(
             p("Model currently in line for production."),
           ),
           col_3(
             actionButton(ns("to_dashbaord"), "Jump to Dashboard", class = "btn btn-primary")
           ),
           col_3(
             actionButton(ns("model_instructions"), "Instructions", class = "btn btn-primary")
           )
         )
       })
     } else if (data$model_processing == 'model_needed_running' || data$model_processing == 'model_currently_running' ) {
       output$gen_ui_comp <- renderUI({
         fluidRow(
           col_3(
             p("Model currently in production."),
           ),
           col_3(
             actionButton(ns("to_dashbaord"), "Jump to Dashboard", class = "btn btn-primary")
           ),
           col_3(
             actionButton(ns("model_instructions"), "Instructions", class = "btn btn-primary")
           )
         )
       })
     } else  {
       output$gen_ui_comp <- renderUI({
         fluidRow(
           col_3(
             actionButton(ns("build_model"), "Build Safer Streets Model", class = "btn btn-primary leaflet_block"),
             uiOutput(ns("build_model_process"), class='leaflet_none'),
           ),
           col_3(
             actionButton(ns("to_dashbaord"), "Jump to Dashboard", class = "btn btn-primary")
           ),
           col_3(
             actionButton(ns("model_instructions"), "Instructions", class = "btn btn-primary")
           )
         )
       })
     }
   })

   # modal to handle model overwrites 
   back_of_queue <- modalDialog(
    title = "Start Over?",
    easyClose = FALSE,
    footer = NULL,
    next_label = NULL,
    # create login 
    tagList(
      tags$div(class = 'login_well',
               h5('You\'re Already In Line For Model Production!'),
      ),
     fluidRow(
       column(12, align="center",
       tags$div(style="display:inline-block",
                actionButton(ns("inqueue_cancel"), "Yes, Start Over", class = "btn btn-primary"),
                actionButton(ns("inqueue_dont_cancel"), "No, Keep My Place In Line", class = "btn btn-primary")
          )
     ))
     )
    )
    
   # Confirmation modal for user to rebuild model
    ready_results <- modalDialog(
      title = "Remove Results?",
      easyClose = FALSE,
      footer = NULL,
      next_label = NULL,
      tagList(
        tags$div(class = 'login_well',
                 p('Are you sure you want to rebuild your models? By doing so, you will overwrite your current results.'),
        ),
        fluidRow(
          column(12, align="center",
                 tags$div(style="display:inline-block",
                  id = ns("access_buttons"), class = 'login_buttons leaflet_block',
                  actionButton(ns("ready_delete"), "Yes, Delete Results And Start Over", class = "btn btn-primary"),
                  actionButton(ns("ready_dont_delete"), "No, Keep My Results", class = "btn btn-primary")
                   )
          ))
      )
    )
    
    # Executes model build
    model_func <- function(){
      crs <- get_account_crs(
        connection = connection, 
        user_id = user_id,  
        run_id = DBI::dbQuoteLiteral(connection, run_id)
      )
      if (data$sa_exists) {
        psql_update_epsg(connection=connection, table=DBI::dbQuoteIdentifier(connection, return_table_name('study_area', user_id, run_id)), new_epsg=crs)
      }
      if (data$cr_exists) {
        psql_update_epsg(connection=connection, table=DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)), new_epsg=crs)
      }
      if (data$rd_exists) {
        psql_update_epsg(connection=connection, table=DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id)), new_epsg=crs)
      }
      waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
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
      
      if ( !roads_in_study_area || !crashes_in_study_area ) {
        waiter::waiter_hide()
        shiny_warming_alert(title = 'Non intersecting data', text='Your data do not intersect with the study area you uploaded. Please upload intersecting data.')
      } else  {
        crashes_near_roads <- intersect_server_side(connection = connection, 
                                                    user_id = user_id, 
                                                    run_id = data$run_id_sql_formatted, 
                                                    schema1 = 'local_user_data',
                                                    table1  = glue::glue('crashes_{user_id}_{run_id}'),
                                                    schema2 = 'local_user_data',
                                                    table2  = glue::glue('roads_{user_id}_{run_id}')
        )
        
        if (!crashes_near_roads) {
          waiter::waiter_hide()
          shiny_warming_alert(title = 'Non intersecting data', text='Your crash and roads data do not intersect. Please upload new data that intersects.')
          
        } else {
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_status'), new_value=DBI::dbQuoteString(connection, glue::glue('model_needed')))
          update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_since_model_desired'), new_value=DBI::dbQuoteString(connection,'NOW()'))
          # data$model_info<-get_model_timeline_information(connection, user_id, run_id)

          output$gen_ui_comp <- renderUI({
            fluidRow(
              col_3(
                p('Model in Production')
              ),
              col_3(
                actionButton(ns("to_dashbaord"), "Jump to Dashboard", class = "btn btn-primary")
              )
            )
          })
          waiter::waiter_hide()
          }}
    }
    
    # this set of events updates information on the user's account that will trigger a model build in the build model results page 
    observeEvent(input$build_model, {
      data$model_processing <- model_production_status(connection, user_id, run_id)
      if (data$model_processing == 'model_needed') {
        showModal(back_of_queue)
      } else if ( data$model_processing == 'model_needed_running' || data$model_processing == 'model_currently_running' ) {
        shiny_warming_alert(title = 'Whoa!', text='Your model is processing right now! Come back soon to see your results.')
      } else if ( data$model_processing == 'model_estimation_completed' ) {
        showModal(ready_results)
      } else {
        removeModal()
        data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
        data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
        data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
        
        # general check for tables
        if (!data$sa_exists || !data$cr_exists || !data$rd_exists) {
          shiny_warming_alert(title = 'Missing Data', text="You're missing data. If you're not sure what you're missing, go to the 'Confirm Input Data' tab and click the 'Map Data' button.")
        } else {
          v <- five_year_crash_data_test(connection=connection, user_id=user_id, run_id=run_id)
          if (!v) {
            shiny_warming_alert(title = 'Not enough data.', text=" You have not supplied 5 years worth of crash data, please supply additional years of crash data.")
          } else {
          model_func()
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").removeClass("leaflet_none");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").addClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").removeClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").addClass("leaflet_none");'))
          shiny_warming_alert(title='Started', text='Your model is being produced. This process can take up to a day to complete. Come back soon to review your results.', showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="success")
          }
        }
       }
    })
    
    # starts over 
    observeEvent(input$inqueue_cancel, {
      removeModal()
      data$model_processing <- model_production_status(connection, user_id, run_id)
      if ( data$model_processing == 'model_needed_running' || data$model_processing == 'model_currently_running'  ) {
        shiny_warming_alert(title = 'Whoa!', text='Your model has started since the time your decided to cancel your place in the queue. Come back soon to see your results.')
      } else {
        data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
        data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
        data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
        # general check for tables
        if (!data$sa_exists || !data$cr_exists || !data$rd_exists) {
          shiny_warming_alert(title = 'Missing Data', text="You're missing data. If you're not sure what you're missing, go to the 'Load Data' page and check the 'Confirm Input Data' tab. Then, click the 'Map Data' button.")
        } else {
          model_func()
 
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").removeClass("leaflet_none");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").addClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").removeClass("leaflet_block");'))
          shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").addClass("leaflet_none");'))
          shiny_warming_alert(title='Started', text='Your model is being produced. This process can take up to a day to complete. Come back soon to review your results.', showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="success")
        }
      }
      })
    
    observeEvent(input$inqueue_dont_cancel, {
      removeModal()
    })
    
    # deletes results on confirmation 
    observeEvent(input$ready_delete, {
      removeModal()
      shiny_warming_alert(title='Results Deleted', text="Proceeding", showConfirmButton=FALSE, showCancelButton=FALSE, size="s", timer=2000, type="success")
      delete_tables(connection, list_of_tables=list(c('model_outputs', glue::glue('hin_output_roads_{user_id}_{run_id}')), c('model_output_scratch', glue::glue('hin_sliding_windows_{user_id}_{run_id}'), c('model_output_scratch', glue::glue('hin_crashes_{user_id}_{run_id}')))))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_since_model_desired'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_status'), new_value=DBI::dbQuoteLiteral(connection, 'no_model_desired'))
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_process_time'), new_value='NULL')
      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'time_mode_finished'), new_value='NULL')

      update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'model_comp'), new_value='NULL')
      data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
      data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
      
      # general check for tables
      if (!data$sa_exists || !data$cr_exists || !data$rd_exists) {
        shiny_warming_alert(title = 'Missing Data', text="You're missing data. If you're not sure what you're missing, go to the 'Load Data' page and check the 'Confirm Input Data' tab. Then, click the 'Map Data' button.")
      } else {
        model_func()
        output$build_model_process <- renderUI({'Model in production'})
        shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('build_model_process'), '").addClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#', session$ns('build_model'), '").addClass("leaflet_none");'))
        shiny_warming_alert(title='Started', text='Your model is being produced. This process can take up to a day to complete. Come back soon to review your results.', showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="success")
        }
      })
    
    # removes modal 
    observeEvent(input$ready_dont_delete, {
      removeModal()
    })
  
    # pulls existing model results for map 
    observeEvent(input$pull_existing_model_results, {
      data$mr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$output_roads_sql_literal, column=DBI::dbQuoteLiteral(connection, 'rt_bike_cost_1y'), info_schema=DBI::dbQuoteIdentifier(connection, 'columns'))
      if (!data$mr_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "You don't have model results."
        )
      } else {
      
       waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
       
      # grab table from postgresql database
      data$model_results <- fetch_spatial_table(connection = connection,
                                                schema = 'model_outputs',
                                                table =  data$ouput_roads,
                                                geom_type = 'LINESTRING',
                                                is_wkt = T,
                                                clauses = 'WHERE  length > 20'
      )
      
      # replace NA's with 0
      data$model_results <- data$model_results %>% 
        replace(is.na(.), 0)  
      
      # visualize module
      callModule(mod_visualize_model_results_server, 
                 "visualize_model_results_ui_1", 
                 connection=connection, 
                 user_id=user_id, 
                 run_id=run_id, 
                 data=data, 
                 leaflet_proxy = leafletProxy("model_map", session),
                 n=data$n
      )
      data$n <- data$n + 1
      waiter::waiter_hide()
      }
    })

    # renders map  
    render_map(output, 'model_map') 
    outputOptions(output, "model_map", suspendWhenHidden = FALSE)
    shinyjs::runjs(code = paste0('$("#tab-analysis_production").click(function(){$("#', session$ns('model_map'), '").trigger("shown");})'))

  }
 
    
## To be copied in the UI
# mod_build_model_ui("build_model_ui_1")
    
## To be copied in the server
# mod_build_model_server("build_model_ui_1")
