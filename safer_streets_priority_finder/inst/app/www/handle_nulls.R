handle_crash_nulls <- function(data, session, input, output){
  tryCatch({
    data$number_year_nulls=sum(is.na(data$crashes[[input$year_variable]]) + sum(is.null(data$crashes[[input$year_variable]])))
    data$number_report_id_nulls=sum(is.na(data$crashes[[input$reportid_variable]]) + sum(is.null(data$crashes[[input$reportid_variable]])))
    data$number_mode_nulls=sum(is.na(data$crashes[[input$mode_variable]]) + sum(is.null(data$crashes[[input$mode_variable]])))
    data$number_severiy_nulls=sum(is.na(data$crashes[[input$crash_sev_variable]]) + sum(is.null(data$crashes[[input$crash_sev_variable]])))
    
    if (!is.null(data$number_severiy_nulls) && data$number_severiy_nulls > 0){
      output$handle_severity_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_severity_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_severity_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML(paste("<p><strong>A total of", as.integer(data$number_severiy_nulls), 'NULL(s) or NA(s) were found in your crash severity attribute.</strong></p>'))
        output[[2]] = HTML("<p>Please identify how you'd like to handle NULLs and NAs in your crash rash severity attribute</p>")
        output[[3]] = radioButtons(ns("handle_severity_nulls_if_exists_radio"), 
                                   label =  NULL,
                                   choices = data$sev_levels,
                                   selected=data$sev_levels[6]
        )
        output
      })
    } else {
      output$handle_severity_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_severity_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_severity_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML("<p>Great, no NULLs or NAs were found in your crash severity attribute.</p>")
        output
      })
    }
    if (!is.null(data$number_mode_nulls) && data$number_mode_nulls > 0){
      output$handle_mode_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_mode_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_mode_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML(paste("<p><strong>A total of", as.integer(data$number_mode_nulls), 'NULL(s) or NA(s) were found in your crash mode attribute.</strong></p>'))
        output[[2]] = HTML("<p>Please identify how you'd like to handle NULLs and NAs in your crash mode attribute.</p>")
        output[[3]] = radioButtons(ns("handle_mode_nulls_if_exists_radio"), 
                                   label =  NULL,
                                   choices = data$mode_levels,
                                   selected=data$mode_levels[3]
        )
        output
      })
    } else {
      output$handle_mode_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_mode_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_mode_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML("<p>Great, no NULLs or NAs were found in your crash mode attribute.</p>")
        output
      })
    }
    
    if (!is.null(data$number_report_id_nulls) && data$number_report_id_nulls > 0){
      output$handle_reportid_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_reportid_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_reportid_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML(paste("<p><strong>A total of", as.integer(data$number_report_id_nulls), 'NULL(s) or NA(s) were found in your crash report id attribute.</strong></p>'))
        output[[2]] = HTML("<p>There isn't much we can do about NULLs and NAs in your report id attribute. We're going to need to drop these from the analysis.</p>")
        output[[3]] = radioButtons(ns("handle_reportid_nulls_if_exists_radio"), 
                                   label = NULL,
                                   choices = c("Omit From Analysis")
        )
        output
      })
    } else {
      output$handle_reportid_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_reportid_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_reportid_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML("<p>Great, no NULLs or NAs were found in your report id attribute.</p>")
        output
      })
    }
    if (!is.null(data$number_year_nulls) && data$number_year_nulls > 0){
      output$handle_year_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_year_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_year_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML(paste("<p><strong>A total of", as.integer(data$number_year_nulls), 'NULL(s) or NA(s) were found in your crash year attribute.</strong></p>'))
        output[[2]] = HTML("<p>There isn't much we can do about NULLs and NAs in your <strong><u>crash year</u></strong> attribute. We're going to need to drop these from the analysis.</p>")
        output[[3]] = radioButtons(ns("handle_year_nulls_if_exists_radio"), 
                                   label = NULL,
                                   choices = c("Omit From Analysis")
        )
        output
      })
    } else {
      output$handle_year_nulls_if_exists <- renderUI({}) 
      outputOptions(output, "handle_year_nulls_if_exists", suspendWhenHidden = FALSE)
      output$handle_year_nulls_if_exists <- renderUI({
        output = tagList()
        output[[1]] = HTML("<p>Great, no NULLs or NAs were found in your crash year attribute.</p>")
        output
      })
    }
    return(TRUE)
  }, error = function(cond){
    return(FALSE)
  })
}

handle_road_nulls <- function(data, session, input, output){
  tryCatch({
  data$number_func_class_nulls=sum(is.na(data$the_roads[[input$func_class_variable]]) + sum(is.null(data$the_roads[[input$func_class_variable]])))
  if (!is.null(data$number_func_class_nulls) && data$number_func_class_nulls > 0 ){
    output$handle_funclass_nulls_if_exists <- renderUI({}) 
    outputOptions(output, "handle_funclass_nulls_if_exists", suspendWhenHidden = FALSE)
    output$handle_funclass_nulls_if_exists <- renderUI({
      output = tagList()
      output[[1]] = HTML(paste("<strong><p>A total of", as.integer(data$number_func_class_nulls), 'NULL(s) or NA(s) were found in your roads functional classification attribute.</strong></p>'))
      output[[2]] = HTML("<p>Please identify how you'd like to handle NULLs and NAs in your roads functional classification attribute.</p>")
      output[[3]] = radioButtons(ns("fun_class_nulls_map"), 
                                 label =  NULL,
                                 choices = data$fun_class,
                                 selected=data$fun_class[7]
      )
      output
    })
  } else {
    output$handle_funclass_nulls_if_exists <- renderUI({}) 
    outputOptions(output, "handle_funclass_nulls_if_exists", suspendWhenHidden = FALSE)
    output$handle_funclass_nulls_if_exists <- renderUI({
      output = tagList()
      output[[1]] = HTML("Great, no NULLs or NAs were found in your functional classification attribute that you identified.")
      output
    })
  }
  return(TRUE)
  }, error = function(cond){
    return(FALSE)
  })
}