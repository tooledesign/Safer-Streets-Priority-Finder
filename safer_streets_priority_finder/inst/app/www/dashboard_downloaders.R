############################## Primary Downloader, upper right card 
output$download_data <- shiny::downloadHandler(
  filename = function() {
    paste("study-", run_id, '-', input$data_selector_for_download, ".zip", sep='') 
  },
  content = function(file) {
    if (input$data_selector_for_download == 'Road Network') {
      data$rd_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_roads)
      if (!data$rd_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        r = return_table_name('roads', user_id, run_id)
        schema_literal <- DBI::dbQuoteLiteral(connection, 'local_user_data')
        table_literal <- DBI::dbQuoteLiteral(connection, r) 
        q <- glue::glue('
                        SELECT 
                        \'
                         "tdg_id_{user_id}_{run_id}" p_key,
                           "usdot_fun_class_mapped" as f_class,
                        \'
                        ||
                        array_to_string(ARRAY(SELECT \'"\' || c.column_name || \'"\'
                                                                  FROM information_schema.columns As c
                                                                  WHERE table_name = {table_literal}
                                                                  AND table_schema = {schema_literal}
                                                                  AND  c.column_name NOT IN (\'geom\')
                        ), \',\') || \', ST_ASEWKT(geom) as geom\' As sqlstmt
                  ')
        
        columns <-  DBI::dbGetQuery(connection,q)
        downloader(
          connection = connection,
          schema     = 'local_user_data',
          table      = r,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'LINESTRING',
          file=file,
          columns = columns
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Crashes') {
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
      if (!data$cr_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        c = return_table_name('crashes', user_id, run_id)
        schema_literal <- DBI::dbQuoteLiteral(connection, 'local_user_data')
        table_literal <- DBI::dbQuoteLiteral(connection, c) 
        q <- glue::glue('
                        SELECT 
                        \'
                         "tdg_id_{user_id}_{run_id}" p_key,
                           "severity_mapped" as sev_cat, 
                           "usdot_mode_mapped" as mode,
                           "crashes_costs_usdot" as crsh_cst, 
                           CASE WHEN "in_sa_{user_id}_{run_id}" THEN \'\'True\'\' ELSE \'\'False\'\' END as in_s_area,
                           "fclass_mapped" as f_class,
                        \'
                        ||
                        array_to_string(ARRAY(SELECT \'"\' || c.column_name || \'"\'
                                                                  FROM information_schema.columns As c
                                                                  WHERE table_name = {table_literal}
                                                                  AND table_schema = {schema_literal}
                                                                  AND  c.column_name NOT IN (\'geom\')
                        ), \',\') || \', ST_ASEWKT(geom) as geom\' As sqlstmt
                  ')
        columns <-  DBI::dbGetQuery(connection,q)
        
        downloader(
          connection = connection,
          schema     = 'local_user_data',
          table      = c,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'POINT',
          file=file,
          columns=columns
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Study Area') {
      data$sa_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_study_area)
      if (!data$sa_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        s = return_table_name('study_area', user_id, run_id)
        downloader(
          connection = connection,
          schema     = 'local_user_data',
          table      = s,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'MULTIPOLYGON',
          file=file
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Sliding Windows Analysis') {
      data$sw_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
      
      if (!data$sw_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        l = return_table_name('sw_sliding_windows', user_id, run_id)
        downloader(
          connection = connection,
          schema     = 'sliding_windows_outputs',
          table      = l,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'LINESTRING',
          file=file,
          columns = glue::glue('id,
                                road_name as name, 
                                road_fclass as f_class, 
                                bike_score as bike_sc, 
                                ped_score as ped_sc,
                                other_score as other_sc,
                                length,
                                ST_AsEWKT((ST_Dump(geom)).geom) as geom')
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Safer Streets Model') {
      data$mr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_model_outputs_schema, table=data$sql_literal_model_results, column=DBI::dbQuoteLiteral(connection, 'rt_bike_cost_1y'), info_schema=DBI::dbQuoteIdentifier(connection, 'columns'))
      
      if (!data$mr_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
        return(NULL)
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        m = return_table_name('hin_output_roads', user_id, run_id)
        road_id <-  DBI::dbQuoteIdentifier(connection, as.character(DBI::dbGetQuery(connection, glue::glue("SELECT road_o_id FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = \'{run_id}\';"))[1,1]))
        
        downloader(
          connection = connection,
          schema     = 'model_outputs',
          table      = m,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'LINESTRING',
          file       = file,
          columns = glue::glue('tdg_id_{user_id}_{run_id} as pkey,
                                 {road_id} as orig_pkey,
                                  road_name as name,
                                  road_fclass as f_class,
                                  length,
                                  tot_all,
                                  tot_bike_all as tot_bk_all,
                                  tot_bike_k as tot_bk_k,
                                  tot_bike_a as tot_bk_a,
                                  tot_bike_b as tot_bk_b,
                                  tot_bike_c as tot_bk_c,
                                  tot_bike_o as tot_bk_o,
                                  tot_ped_all as tot_p_all,
                                  tot_ped_k as tot_pd_k,
                                  tot_ped_a as tot_pd_a,
                                  tot_ped_b as tot_pd_b,
                                  tot_ped_c as tot_pd_c,
                                  tot_ped_o as tot_pd_o,
                                  bike_score as bike_score,
                                  ped_score as ped_score,
                                  stan_bike_k as est_bk_k,
                                  stan_bike_a  as est_bk_a,
                                  stan_bike_b  as est_bk_b,
                                  stan_bike_c  as est_bk_c,
                                  stan_bike_o  as est_bk_o,
                                  stan_ped_k  as est_pd_k,
                                  stan_ped_a  as est_pd_a,
                                  stan_ped_b  as est_pd_b,
                                  stan_ped_c  as est_pd_c,
                                  stan_ped_o  as est_pd_o,
                                  e_cr_bike_k as e_cr_bk_k,
                                  e_cr_bike_a as e_cr_bk_a,
                                  e_cr_bike_b as e_cr_bk_b,
                                  e_cr_bike_c as e_cr_bk_c,
                                  e_cr_bike_o as e_cr_bk_o,
                                  e_cr_ped_k as e_cr_pd_k,
                                  e_cr_ped_a as e_cr_pd_a,
                                  e_cr_ped_b as e_cr_pd_b,
                                  e_cr_ped_c as e_cr_pd_c,
                                  e_cr_ped_o as e_cr_pd_o,
                                  stan_bike_cost as t_es_b_cst,
                                  stan_ped_cost as t_es_p_cst,
                                  rt_bike_cost_1y as r_b_cst_1y,
                                  rt_bike_cost_1y*5 as r_b_cst_5y,
                                  rt_ped_cost_1y as r_p_cst_1y,
                                  rt_ped_cost_1y*5 as r_p_cst_5y,
                                  ST_AsEWKT(geom) as geom'
          )
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Bike and Pedestrian Crashes') {
      
      data$cr_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_local_user_data_schema, table=data$sql_literal_crashes)
      if (!data$cr_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        p = return_table_name('crashes', user_id, run_id)
        schema_literal <- DBI::dbQuoteLiteral(connection, 'local_user_data')
        table_literal <- DBI::dbQuoteLiteral(connection, p)  
        q <- glue::glue('
                        SELECT 
                        \'
                         "tdg_id_{user_id}_{run_id}" p_key,
                           "severity_mapped" as sev_cat, 
                           "usdot_mode_mapped" as mode,
                           "crashes_costs_usdot" as crsh_cst, 
                           CASE WHEN "in_sa_{user_id}_{run_id}" THEN \'\'True\'\' ELSE \'\'False\'\' END as in_s_area,
                           "fclass_mapped" as f_class,
                        \'
                        ||
                        array_to_string(ARRAY(SELECT \'"\' || c.column_name || \'"\'
                                                                  FROM information_schema.columns As c
                                                                  WHERE table_name = {table_literal}
                                                                  AND table_schema = {schema_literal}
                                                                  AND  c.column_name NOT IN (\'geom\')
                        ), \',\') || \', ST_ASEWKT(geom) as geom\' As sqlstmt
                  ')
        
        columns <-  DBI::dbGetQuery(connection,q)
        downloader(
          connection = connection,
          schema     = 'local_user_data',
          table      = p,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          geom_type  = 'POINT',
          file=file,
          clauses='WHERE usdot_mode_mapped in (\'Pedestrian Crash\', \'Bicycle Crash\')',
          columns=columns
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Top Ten Pedestrian Crash Corridors') {
      
      data$t10ped_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
      
      if (!data$t10ped_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        p = return_table_name('sw_sliding_windows', user_id, run_id)
        top_10_ped <- get_dangerous_locations(c=connection, the_input='ped_score', user_id=user_id, run_id=run_id)
        top_10_ped <- sf::st_as_sf(top_10_ped , wkt='geom')
        colnames(top_10_ped) <- c("name", "f_class", 'ped_cr_sc', 'drop', 'geom')
        top_10_ped <- top_10_ped %>% 
                        arrange(desc(ped_cr_sc)) %>% 
                        mutate(id=seq_len(nrow(top_10_ped))) %>% 
                        select(c(id, name, f_class, ped_cr_sc, geom)) 
 
        downloader(
          connection = connection,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          file=file,
          custom_table = top_10_ped
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Top Ten Bicycle Crash Corridors') {
      
      data$t10bike_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
      
      if (!data$t10bike_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        p = return_table_name('sw_sliding_windows', user_id, run_id)
        
        top_10_bike <- get_dangerous_locations(c=connection, the_input='bike_score', user_id=user_id, run_id=run_id)
        top_10_bike <- sf::st_as_sf(top_10_bike , wkt='geom')
        colnames(top_10_bike) <- c("name", "f_class", 'bike_cr_sc', 'drop', 'geom')
        top_10_bike <- top_10_bike %>% 
          arrange(desc(bike_cr_sc)) %>% 
          mutate(id=seq_len(nrow(top_10_bike))) %>% 
          select(c(id, name, f_class, bike_cr_sc, geom)) 
        
        downloader(
          connection = connection,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          file=file,
          custom_table = top_10_bike,
        )
        waiter::waiter_hide()
      }
    } else if (input$data_selector_for_download == 'Top Ten Other Crash Corridors') {
      
      data$t10other_exists <- test_if_table_exists(connection=connection, schema=data$sql_literal_sw_results_schema, table=data$sql_literal_sliding_windows)
      
      if (!data$t10other_exists) {
        shiny_warming_alert(
          title = "Whoa!",
          text = "That table doesn\'t exist."
        )
      } else {
        waiter::waiter_show(color='rgba(175, 175, 175, 0.85)', html = tagList(tags$div(waiter::spin_1()), tags$br(), tags$div(HTML("Loading ..."))))
        p = return_table_name('sw_sliding_windows', user_id, run_id)
        
        top_10_other <- get_dangerous_locations(c=connection, the_input='other_score', user_id=user_id, run_id=run_id)
        top_10_other <- sf::st_as_sf(top_10_other , wkt='geom')
        colnames(top_10_other) <- c("name", "f_class", 'othr_cr_sc', 'drop', 'geom')
        top_10_other <- top_10_other %>% 
          arrange(desc(othr_cr_sc)) %>% 
          mutate(id=seq_len(nrow(top_10_other))) %>% 
          select(c(id, name, f_class, othr_cr_sc, geom)) 
        
        downloader(
          connection = connection,
          filename   = paste0(input$data_selector_for_download, '_', user_id, '_', run_id),
          file=file,
          custom_table = top_10_other
        )
        waiter::waiter_hide()
      }
    }
  }
)

############################## Auxiliary Downloaders, dispersed
output$download_cr_mode <- downloadHandler(
  filename = function() {
    paste("Crashes by Mode-", input$chart2_selector, ".csv", sep='') 
  },
  content = function(file) {
    if (is.null(data$to_download_crash_by_mode)) {
      shiny_warming_alert(
        title = "Whoa!",
        text = "That table doesn\'t exist."
      )
    } else{
      write.csv2(data.frame(data$to_download_crash_by_mode), file, row.names = TRUE )
    }
  }
)
output$download_cr_sev <- downloadHandler(
  filename = function() {
    paste("Crashes by Severity-", input$chart1_selector, ".csv", sep='') 
  },
  content = function(file) {
    if (is.null(data$to_download_crash_by_severity)) {
      shiny_warming_alert(
        title = "Whoa!",
        text = "That table doesn\'t exist."
      )
    } else{
      write.csv2(data.frame(data$to_download_crash_by_severity), file, row.names = TRUE )
    }
  }
)
output$download_cr_flcass <- downloadHandler(
  filename = function() {
    paste("Crashes by Road FClass-", input$chart5_selector, ".csv", sep='') 
  },
  content = function(file) {
    if (is.null(data$to_download_crash_by_fclass)) {
      shiny_warming_alert(
        title = "Whoa!",
        text = "That table doesn\'t exist."
      )
    } else{
      write.csv2(data.frame(data$to_download_crash_by_fclass), file, row.names = TRUE )
    }
  }
)
output$download_cr_year <- downloadHandler(
  filename = function() {
    paste("Crashes by Year-", input$chart8_selector, ".csv", sep='') 
  },
  content = function(file) {
    if (is.null(data$to_download_crash_by_year)) {
      shiny_warming_alert(
        title = "Whoa!",
        text = "That table doesn\'t exist."
      )
    } else{
      write.csv2(data.frame(data$to_download_crash_by_year), file, row.names = TRUE )
    }
  }
)
output$download_stacked <- downloadHandler(
  filename = "Percent Severity By Mode.csv" ,
  content = function(file) {
    if (is.null(data$to_download_stacked_bar)) {
      shiny_warming_alert(
        title = "Whoa!",
        text = "That table doesn\'t exist."
      )
    } else{
      write.csv2(data.frame(data$to_download_stacked_bar), file, row.names = TRUE )
    }
  }
)

download_instructions_modal <- modalDialog(
  title = "Instructions to Download Data",
  easyClose = TRUE,
  next_label = NULL,
  tagList(
    tags$div( 
      p("Download any of the input data used (i.e. the data you originally uploaded or the default US Census county/FARS/OSM data) or the analysis outputs. You can join these networks back to your original roads dataset in GIS using the unique ID specified earlier.")
    )
  ),
  footer = tagList(
    actionButton(ns("ok"), "OK")
  )
)
 