# checks selected attributes the user identified to map for unacceptable conditions are met
# if any unacceptable conditions are met, the user is notified with UI components 
crash_variables_selection_warnings <- function(data, input, session, output){
  tryCatch({
    start_time <- Sys.time()
    w <- waiter::Waiter$new(html = span("Loading..."),  color='rgba(175, 175, 175, 0.85)')    
    w$show()
    year_test <- are_all_integers(na.omit(as.vector(data$crashes[[input$year_variable]])))
    year_formatted <- are_year_formatted_sortof(na.omit(as.integer(as.vector(data$crashes[[input$year_variable]]))))
 
    # check for problems in data  
    if (!year_test || !year_formatted || data$sev_id_count > 20 || data$mode_count > 20 || is.null(unique(data$crashes[[input$year_variable]])) || data$mode_count == 0 || data$sev_id_count == 0) {
      w$hide()
      # check for non integer values 
      if (!year_test){
        output$year_variable_notice_1 <- renderUI({})
        outputOptions(output, "year_variable_notice_1", suspendWhenHidden = FALSE)
        output$year_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("The <strong>crash year</strong> attribute you selected <u>includes non integer values</u>.")
          )
        })
      } else if (is.null(unique(data$crashes[[input$year_variable]]))) {
        output$year_variable_notice_1 <- renderUI({})
        outputOptions(output, "year_variable_notice_1", suspendWhenHidden = FALSE)
        output$year_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("There are <u>no values</u> within the <strong>crash year</strong> attribute you identified. Please select a different attribute.")
          )
        })
        
      } else {
        output$year_variable_notice_1 <- renderUI({})
        # check for 4 digit integer data 
        # TODO check for reasonable range of years 
        if (!year_formatted){
          output$year_variable_notice_2 <- renderUI({})
          outputOptions(output, "year_variable_notice_2", suspendWhenHidden = FALSE)
          output$year_variable_notice_2 <- renderUI({
            tags$div(style="color:#e04a5e",
                     HTML("The <strong>crash year</strong> attribute you selected <u>includes integers longer or shorter than 4 digits</u>. Please format the year values as 4 digit integers (e.g., 2019).")
            )
          })
        } else {
          output$year_variable_notice_2 <- renderUI({})
        }
      }  
      
      
      # ensure no more than 20 unique values in severity column 
 
      if (data$sev_id_count > 20){
        output$crash_sev_variable_notice_1 <- renderUI({}) 
        outputOptions(output, "crash_sev_variable_notice_1", suspendWhenHidden = FALSE)
        output$crash_sev_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("There are <u>greater than 20 unique values</u> within the <strong>crash severity</strong> attribute you identified. Please select an attribute that has fewer than 20 unique values.")
          )
        })
      } else if (data$sev_id_count == 0){
        output$crash_sev_variable_notice_1 <- renderUI({}) 
        outputOptions(output, "crash_sev_variable_notice_1", suspendWhenHidden = FALSE)
        output$crash_sev_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("There are <u>no values</u> within the <strong>crash severity</strong> attribute you identified. Please select a different attribute.")
                   
          )
        })
      } else {
        output$crash_sev_variable_notice_1 <- renderUI({})
      }
      
      
      
      # ensure no more than 20 unique values in mode column 
      if (data$mode_count > 20){
        output$mode_variable_notice_1 <- renderUI({})
        outputOptions(output, "mode_variable_notice_1", suspendWhenHidden = FALSE)
        output$mode_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("There are <u>greater than 20 unique values</u> within the <strong>crash mode</strong> attribute you identified. Please select an attribute that has fewer than 20 unique values.")
          )
        })
      } else if (data$mode_count == 0){
        output$mode_variable_notice_1 <- renderUI({})
        outputOptions(output, "mode_variable_notice_1", suspendWhenHidden = FALSE)
        output$mode_variable_notice_1 <- renderUI({
          tags$div(style="color:#e04a5e",
                   HTML("There are <u>no values</u> within the <strong>crash mode</strong> attribute you identified. Please select a different attribute.")
          )
        })
      } else {
        output$mode_variable_notice_1 <- renderUI({})
      }

    } else {
      w$hide()
      output$mode_variable_notice_1 <- renderUI({})
      output$crash_sev_variable_notice_1 <- renderUI({})
      output$year_variable_notice_3 <- renderUI({})
      output$year_variable_notice_2 <- renderUI({})
      output$year_variable_notice_1 <- renderUI({})
      
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").removeClass("leaflet_block");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides2'), '").addClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").removeClass("leaflet_none");'))
      shinyjs::runjs(code = paste0('$("#', session$ns('crash_slides3'), '").addClass("leaflet_block");'))
      
      runjs(paste0('$("#', session$ns('cr_severity_map_table'), '").trigger("shown");
                    $("#', session$ns('cr_mode_map_table'), '").trigger("shown");
                         ')
      )
    }

    end_time <- Sys.time()
    print(paste0('Time to check data: ', end_time-start_time))
  }, error = function(cond){
    print(cond)
    shiny_warming_alert(title='Warning', text="There was a problem loading your data. This may have resulted from a bad database connection or something else. If the problem persists, please contact the developers.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
    
  })
}

# checks selected attributes the user identified to map for unacceptable conditions are met
# if any unacceptable conditions are met, the user is notified with UI components 
road_variable_selection_warnings <- function(data, input, session, output){
  tryCatch({
  if (data$fun_class_count > 20 || data$fun_class_count == 0) {
    if (data$fun_class_count > 20) {
      output$func_class_variable_notice_1 <- renderUI({}) 
      outputOptions(output, "func_class_variable_notice_1", suspendWhenHidden = FALSE)
      output$func_class_variable_notice_1 <- renderUI({
        tags$div(style="color:#e04a5e",
                 HTML("There are <u>greater than 20 unique values</u> within the <strong>functional classification</strong> attribute you identified. Please use an attribute that has fewer than 20 unique values.")
        )
      })
    } else if (data$fun_class_count == 0){
      output$func_class_variable_notice_1 <- renderUI({}) 
      outputOptions(output, "func_class_variable_notice_1", suspendWhenHidden = FALSE)
      output$func_class_variable_notice_1 <- renderUI({
        tags$div(style="color:#e04a5e",
                 HTML("There are <u>no values</u> within the <strong>functional classiciation</strong> attribute you identified. Please select a different attribute.")
        )
      })
    }} else {
    output$func_class_variable_notice_1 <- renderUI({})
    shinyjs::runjs(code = paste0('$("#', session$ns('fun_class_map_table'), '").trigger("shown");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").removeClass("leaflet_block");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides2'), '").addClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").removeClass("leaflet_none");'))
    shinyjs::runjs(code = paste0('$("#', session$ns('roads_slides3'), '").addClass("leaflet_block");'))
    }
  }, error = function(cond){
    print(cond)
    shiny_warming_alert(title='Warning', text="There was a problem loading your data. This may have resulted from a bad database connection or something else. If the problem persists, please contact the developers.", showConfirmButton=TRUE, showCancelButton=FALSE, size="s", type="warning")
    
  })
} 