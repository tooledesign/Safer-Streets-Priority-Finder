
initialize <- function(){
  data$cr_exists  <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
  data$dns_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
  data$est_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results, column=DBI::dbQuoteLiteral(connection, 'rt_bike_cost_1y'), info_schema=DBI::dbQuoteIdentifier(connection, 'columns'))
  data$rds_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
  
  if (data$est_exists ){
    data$est_table <- get_modeloutputs(c=connection, user_id=user_id, run_id=run_id, bike_est_sev_cols=data$bike_est_sev_cols, bike_his_sev_cols=data$bike_his_sev_cols, ped_est_sev_cols=data$ped_est_sev_cols, ped_his_sev_cols=data$ped_his_sev_cols)
  }
  
  if (data$cr_exists && data$rds_exists){
    output$total_analsis <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\'')) })
    output$total_crashes <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('TRUE ')) })
    output$other_crashes <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Other Crash\' AND  in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\''))})
    output$bike_crashes <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Bicycle Crash\' AND in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\''))})
    output$ped_crashes <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Pedestrian Crash\' AND in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\''))})
    output$omitted_by_sev_mode <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('in_sa_{user_id}_{run_id} AND (severity_mapped = \'Omit From Analysis\' OR usdot_mode_mapped = \'Omit From Analysis\') '))})
    output$omited_outside <- renderText({ get_count(connection, table=return_table_name('crashes', user_id, run_id), schema='local_user_data', attr='geom', method='COUNT', where_clause = glue::glue('NOT in_sa_{user_id}_{run_id} '))})
    data$crash_table <- get_crashes(c=connection, user_id=user_id, run_id=run_id)
    data$roads_length <- get_roads(c=connection, user_id=user_id, run_id=run_id)
    data$crash_years <-  get_unique_crash_years(connection = connection, user_id=user_id, run_id=run_id, crash_o_year_col=as.character(get_crash_year(connection, user_id, run_id))) 
    
    output$slider_mode_ui <- renderUI({
      sliderInput(
        min = min(data$crash_years), max = max(data$crash_years),
        value = c(min(data$crash_years), max(data$crash_years)), step = 1,
        inputId = ns("slider_mod"),
        label = "Select a Range:",
        sep=""
      ) 
    })
    outputOptions(output, "slider_mode_ui", suspendWhenHidden = FALSE)
    output$slider_fc_ui <- renderUI({
      sliderInput(
        min = min(data$crash_years), max = max(data$crash_years),
        value = c(min(data$crash_years), max(data$crash_years)), step = 1,
        inputId = ns("slider_fc"),
        label = "Select a Range:",
        sep=""
      ) 
    })
    outputOptions(output, "slider_fc_ui", suspendWhenHidden = FALSE)
    output$slider_sev_ui <- renderUI({
      sliderInput(
        min = min(data$crash_years), max = max(data$crash_years),
        value = c(min(data$crash_years), max(data$crash_years)), step = 1,
        inputId = ns("slider_sev"),
        label = "Select a Range:",
        sep="" 
      ) 
    })
    outputOptions(output, "slider_sev_ui", suspendWhenHidden = FALSE)
    Sys.sleep(.25)
    updateSelectInput(session, "chart1_selector",
                      choices = c('All Crashes', unique(data$crash_table$crashes[data$crash_table$crashes != 'Omited Crashes']))
    )
    
    updateSelectInput(session, "chart5_selector", 
                      choices = c('All Crashes', unique(data$crash_table$crashes[data$crash_table$crashes != 'Omited Crashes']))
    )
    
    updateSelectInput(session, "chart8_selector", 
                      choices = c('All Crashes', unique(data$crash_table$crashes[data$crash_table$crashes != 'Omited Crashes']))
    )
    
    if (data$dns_exists) {
      values_q <- glue::glue('SELECT MAX(ped_score) as pscore, MAX(bike_score) as bscore, MAX(other_score) as oscore FROM sliding_windows_outputs.sw_sliding_windows_{user_id}_{run_id}')
      values <- DBI::dbGetQuery(connection, values_q)
      
      top_ten_options <- c('--Choose One--')
      if (values$pscore > 0) {
        top_ten_options <- append(top_ten_options, c('Top Ten by Pedestrian Sliding Windows Score' = 'ped_score'))
      } 
      if (values$bscore > 0) {
        top_ten_options <- append(top_ten_options, c('Top Ten by Bicycle Sliding Windows Score' = 'bike_score'))
      }
      if (values$oscore > 0) {
        top_ten_options <- append(top_ten_options, c('Top Ten by Other Sliding Windows Score' = 'other_score'))
      }
      
      if (length(top_ten_options) < 2) {
        top_ten_options <- c('--No Data--') 
      } else {
        top_ten_options <- top_ten_options[-1] 
      }
      updateSelectInput(session, "chart6_selector", 
                        choices = top_ten_options,
                        selected = top_ten_options[1])
    }
    
    Sys.sleep(.25)
    if (!is.null(data$crash_table)){
      data$chart_sev_crashes <- data$crash_table
      data$table_sev_crashes <- data$crash_table
      data$fclass_crashes_holding <- data$crash_table
      data$chart_mode_crashes <- data$crash_table
      data$table_mode_crashes <- data$crash_table
      data$stacked_mode_crashes <- data$crash_table
      data$table_crashes_per_year <- data$crash_table
      data$chart_crashes_per_year <- data$crash_table
    }
  }  
  
  data$cr_exists  <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
  data$dns_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
  data$est_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results, column=DBI::dbQuoteLiteral(connection, 'rt_bike_cost_1y'), info_schema=DBI::dbQuoteIdentifier(connection, 'columns'))
  data$rds_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
  
  if (data$entries == 1 && (data$rds_exists || data$dns_exists || data$cr_exists || data$est_exists)) {
    return('first_init_data')
  } else if (data$entries == 1 && (!data$rds_exists || !data$dns_exists || !data$cr_exists || !data$est_exists)) {
    return('first_init_no_data')
  } else if (data$entries > 1 && (data$rds_exists || data$dns_exists || data$cr_exists || data$est_exists)) {
    return('after_first_init_data')
  } else if (data$entries > 1 && (!data$rds_exists || !data$dns_exists || !data$cr_exists || !data$est_exists)) {
    return('after_first_init_no_data')
  } else {
    return('first_init_no_data')
  }
}