# maps vaules from UI to crash data 
map_crashes <- function(data, session, input, discount_rate=NULL) {
  tryCatch({
    
    if (is.null(discount_rate)){
      discount_rate <- .03
    } else {
      discount_rate <- discount_rate
    }
    
    # the following block is a method to bind the selected inputs from the severity and kabco costs tables to the spatial dataframe that will end up in the postgresql database 
    # more or less binds the values selected by the user for each row to an input$ object for Shiny.  
    # create empty dataframe to accept the selected inputs from the user 
    sev_final_holding <- data.frame()
    mode_final_holding <- data.frame()
    
    #creates a table of equal length as the number of selected inputs 
    sev_table <- as.data.frame(data$sev_reactive)
    kcosts_table <- as.data.frame(data$kabco_costs)
    mode_table <- as.data.frame(data$mode_reactive)
    dkab <- data$default_kabco
    dkab[nrow(dkab) + 1,] = c("Omit From Analysis",0)
    
    # loops equal to the number of rows and binds the var id = $(#id).val() with Shiny.setInputValue('input', id)
    # at the same time, this loop populates the fclassf data.frame with the user's selections in the same order as the rows 
    for (i in 1:nrow(sev_table)) {
      shinyjs::runjs(code = paste0('var sev', i, ' = $("#', session$ns(paste0("sev", i)), '").val(); console.log(', paste0("sev", i), '); Shiny.setInputValue("', session$ns(paste0("sev", i)),'", ', paste0("sev", i), ', {priority: \"event\"});'))
      df <- data.frame(input[[paste0('sev', i)]])
      sev_final_holding <- rbind(sev_final_holding, df)
    }
    
    # now do this all for mode 
    for (i in 1:nrow(mode_table)) {
      shinyjs::runjs(code = paste0('var mode', i, ' = $("#', session$ns(paste0("mode", i)), '").val(); Shiny.setInputValue("', session$ns(paste0("mode", i)),'", ', paste0("mode", i), ', {priority: \"event\"});'))
      df <- data.frame(input[[paste0('mode', i)]])
      mode_final_holding <- rbind(mode_final_holding, df)
    }
    
    # this part is populates a new column on the spatial_data_frame with the mapped values the user selected 
    for (i in 1:nrow(sev_table)) {
      data$crashes$severity_mapped[data$crashes[[input$crash_sev_variable]] == sev_table[i,1]] <- sev_final_holding[i,]
      df <- which(kcosts_table[['KABCO Value']] == sev_final_holding[i,])
      shinyjs::runjs(code = paste0('var kab', df, ' = $("#', session$ns(paste0("kab", df)), '").val(); console.log(', paste0("kab", df), '); Shiny.setInputValue("', session$ns(paste0("kab", df)),'", ', paste0("kab", df), ', {priority: \"event\"});'))
      v <- input[[paste0('kab', df)]]
      if (!is.null(v)){
        if (dkab[,2][dkab[,1] == sev_final_holding[i,]] != v) {
          v=round(as.integer(v)/(1+discount_rate)^5, digits = 0)
          data$crashes$crashes_costs_usdot[data$crashes$severity_mapped == sev_final_holding[i,]] <- v
        } else {
          data$crashes$crashes_costs_usdot[data$crashes$severity_mapped == sev_final_holding[i,]] <- dkab[,2][dkab[,1] == sev_final_holding[i,]]  
        } 
      } else {
        data$crashes$crashes_costs_usdot[data$crashes$severity_mapped == sev_final_holding[i,]] <- dkab[,2][dkab[,1] == sev_final_holding[i,]]  
      }
    }
    
    # this part is populates a new column on the spatial_data_frame with the mapped values the user selected 
    for (i in 1:nrow(mode_table)) {
      data$crashes[['usdot_mode_mapped']][data$crashes[[input$mode_variable]] == mode_table[i,1]] <- mode_final_holding[i,]
    }

    return(data$crashes)
  }, error = function(cond){
    print(cond)
  })
}

# maps vaules from UI to roads data 
map_roads <- function(data, session, input){
  tryCatch({
    
    # create empty data frame to accept the selected inputs from the user 
    fclassf <- data.frame()
    
    #creates a table of equal length as the number of selected inputs  
    fclass_table <- as.data.frame(data$fun_class_reactive)
    
    # loops equal to the number of rows and binds the var id = $(#id).val() with Shiny.setInputValue('input', id)
    # at the same time, this loop populates the fclassf data frame from the user's selection in the same order as the rows 
    for (i in 1:nrow(fclass_table)) {
      shinyjs::runjs(code = paste0('var sel', i, ' = $("#', session$ns(paste0("sel", i)), '").val(); Shiny.setInputValue("', session$ns(paste0("sel", i)),'", ', paste0("sel", i), ', {priority: \"event\"});'))
      df <- data.frame(input[[paste0('sel', i)]])
      fclassf <- rbind(fclassf,df)
    }
    
    # this part populates a new column on the spatial_data_frame with the mapped values the user selected 
    for (i in 1:nrow(fclass_table)) {
      data$the_roads[['usdot_fun_class_mapped']][data$the_roads[[input$func_class_variable]] == fclass_table[i,1]] <- fclassf[i,]
    }
    return(data$the_roads)
  }, error = function(cond){
    print(cond)
  })
}