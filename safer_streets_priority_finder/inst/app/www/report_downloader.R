downloader_function <- function (
  connection,
  user_id, 
  run_id
) {
  
   # source(file.path(getwd(), 'R', 'fct_db_utils.R'), local = TRUE)
   # source(file.path(getwd(), 'R', 'fct_helpr_funs.R'), local = TRUE)
   # source(file.path(getwd(), 'R', 'fct_proj_funcs.R'), local = TRUE)
   # source(file.path(getwd(), 'R', 'fct_dashboard.R'), local = TRUE)
  
  
  crash_density <- test_if_table_exists(connection=connection, schema=DBI::dbQuoteLiteral(connection, 'sliding_windows_outputs'), table=DBI::dbQuoteLiteral(connection, glue::glue('sw_sliding_windows_{user_id}_{run_id}')))
  if (crash_density){

    crash_density_results <- fetch_spatial_table(connection = connection,
                                                 columns= 'id,
                                                                    road_name, 
                                                                    road_fclass as usdot_fun_class_mapped, 
                                                                    bike_score as bicycle_crash_score, 
                                                                    ped_score as pedestrian_crash_score,
                                                                    other_score,
                                                                    ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                                 schema = 'sliding_windows_outputs',
                                                 table =  paste0('sw_sliding_windows_', user_id, '_', run_id),
                                                 geom_type = 'LINESTRING',
                                                 is_wkt = T
    )
    crash_density_results <- transform_with_epsg(crash_density_results, 4326)
    
    # get top ten pedestrian, bicycle, and other crashes 
    top_10_ped <- get_dangerous_locations(c=connection, the_input='ped_score', user_id=user_id, run_id=run_id)
    top_10_ped <- top_10_ped[!is.na(names(top_10_ped))]
    top_10_ped <- top_10_ped %>% filter(`Crash Score` > 0)
    t10_ped <- top_10_ped[1:3]  
    if (nrow(t10_ped) > 0){
      top_ten_map_ped <- transform_with_epsg(sf::st_as_sf(top_10_ped, wkt='geom'), 4326)
      top_10_ped <- t10_ped
    } else {
      top_ten_map_ped <- NULL
      top_10_ped <- NULL
    }
    
    top_10_bike <- get_dangerous_locations(c=connection, the_input='bike_score', user_id=user_id, run_id=run_id)
    top_10_bike <- top_10_bike[!is.na(names(top_10_bike))]
    top_10_bike <- top_10_bike %>% filter(`Crash Score` > 0)
    t10_bike <- top_10_bike[1:3]  
    if (nrow(t10_bike) > 0){
      top_ten_map_bike <- transform_with_epsg(sf::st_as_sf(top_10_bike, wkt='geom'), 4326)
      top_10_bike <- t10_bike
    } else {
      top_ten_map_bike <- NULL
      top_10_bike <- NULL
    }
    
    
    top_10_other <- get_dangerous_locations(c=connection, the_input='other_score', user_id=user_id, run_id=run_id)
    top_10_other <- top_10_other[!is.na(names(top_10_other))]
    top_10_other <- top_10_other %>% filter(`Crash Score` > 0)
    t10_other <- top_10_other[1:3] 
    if (nrow(t10_other) > 0){
      top_ten_map_other <- transform_with_epsg(sf::st_as_sf(top_10_other, wkt='geom'), 4326)
      top_10_other <- t10_other
    } else {
      top_ten_map_other <- NULL
      top_10_other <- NULL
    }
  } else {
    crash_density_results <- NULL
    top_10_ped <- NULL
    top_ten_map_ped <- NULL
    top_10_bike <- NULL
    top_ten_map_bike <- NULL
    top_10_other <- NULL
    top_ten_map_other <- NULL
    
    
    
  }
  
  # get model estimates 
  
  model_estimates <- test_if_table_exists(connection=connection, schema=DBI::dbQuoteLiteral(connection, 'model_outputs'), table=DBI::dbQuoteLiteral(connection, return_table_name('hin_output_roads', user_id, run_id)))
  if (model_estimates){
    
    # currently returning geom in unrecognizable format. best option is to paramterize the get_fit_data function 
    m_results  <- fetch_spatial_table(connection = connection,
                                      schema = 'model_outputs',
                                      table =  return_table_name('hin_output_roads', user_id, run_id),
                                      geom_type = 'LINESTRING',
                                      is_wkt = T,
                                      clauses = 'WHERE  length > 20'
    )
    model_results <- transform_with_epsg(m_results, 4326)
    bk_data <- model_results[ which( model_results$rt_bike_cost_1y >= 120563), ] %>% arrange(rt_bike_cost_1y)
    pd_data <- model_results[ which( model_results$rt_ped_cost_1y >= 120563), ] %>% arrange(rt_ped_cost_1y)

    barc_data_bike <- get_fit_data(connection=connection,
                                   table=m_results, 
                                   est_columns = c('e_cr_bike_k', 'e_cr_bike_a', 'e_cr_bike_b', 'e_cr_bike_c', 'e_cr_bike_o'), 
                                   column_rename = c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'),
                                   user_id = user_id, 
                                   run_id = run_id,
                                   mode = 'Bicycle Crash')
    barc_data_bike[['Estimated']] <- round(as.numeric(barc_data_bike[['Estimated']], 2))
    barc_data_ped <- get_fit_data(connection=connection,
                                  table=m_results,
                                  est_columns = c('e_cr_ped_k', 'e_cr_ped_a', 'e_cr_ped_b', 'e_cr_ped_c', 'e_cr_ped_o'), 
                                  column_rename = c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'),
                                  user_id = user_id, 
                                  run_id = run_id,
                                  mode = 'Pedestrian Crash')
    barc_data_ped[['Estimated']] <- round(as.numeric(barc_data_ped[['Estimated']], 2))

    
  } else {
    model_results_bike <- NULL
    model_results_ped <- NULL
    bk_data <- NULL
    pd_data <- NULL
    barc_data_ped <- NULL
    barc_data2_ped <- NULL
    barc_data_bike <- NULL
    barc_data2_bike <- NULL
  }
  sa_exists <- test_if_table_exists(connection=connection, schema=DBI::dbQuoteLiteral(connection, 'local_user_data'), table=DBI::dbQuoteLiteral(connection, return_table_name('study_area', user_id, run_id)))
  if (sa_exists){
    
    study_area_d <- fetch_spatial_table(connection = connection,
                                        columns= 'ST_AsEWKT((ST_Dump(geom)).geom) as geom', 
                                        schema = 'local_user_data',
                                        table =  paste0('study_area_', user_id, '_', run_id),
                                        geom_type='POLYGON',
                                        is_wkt=TRUE
    )
    study_area_d <- transform_with_epsg(study_area_d, 4326)
    
  } else {
    study_area_d <- NULL
  }
  
  cr_exists <- test_if_table_exists(connection=connection, schema=DBI::dbQuoteLiteral(connection, 'local_user_data'), table=DBI::dbQuoteLiteral(connection, return_table_name('crashes', user_id, run_id)))     
  mode_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'crash_o_mode_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id))
  sev_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'crash_o_serv_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id))
  
  if (cr_exists && !is.null(mode_column) && !is.null(sev_column)){
    crashes <- get_crashes(c=connection, user_id=user_id, run_id=run_id)
    total_crashes <- nrow(crashes)
    
    # get crash counts 
    total_crashes = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\'')) 
    total_crashes_in_analysis = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('TRUE '))  
    other_crashes = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Other Crash\' AND  in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\'')) 
    bike_crashes =  get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Bicycle Crash\' AND in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\'')) 
    ped_crashes = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('usdot_mode_mapped = \'Pedestrian Crash\' AND in_sa_{user_id}_{run_id} AND severity_mapped != \'Omit From Analysis\' AND usdot_mode_mapped != \'Omit From Analysis\'')) 
    omitted_by_sev_mode = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('in_sa_{user_id}_{run_id} AND (severity_mapped = \'Omit From Analysis\' OR usdot_mode_mapped = \'Omit From Analysis\') ')) 
    omitted_by_sa = get_count(connection, table=return_table_name('crashes', user_id, run_id), schema=DBI::dbQuoteIdentifier(connection, 'local_user_data'), attr='geom', method='COUNT', where_clause = glue::glue('NOT in_sa_{user_id}_{run_id} ')) 
    
    crash_counts <- data.frame(`Crash Type`=c('Total Bicycle Crashes Included', 'Total Pedestrian Crashes Included', 'Total Other Crashes Included', 'Total Crashes Omitted by Severity or Mode', "Total Crashes Outside Study Area", 'Total Crashes', 'Total Crashes Included in Analyses'),
                               `Count`=c(bike_crashes, ped_crashes, other_crashes, omitted_by_sev_mode, omitted_by_sa, total_crashes_in_analysis, total_crashes))
    
    # download crash data by mode chart
    # this counts the number of crashes by mode 
    crash_by_mode <- crashes %>% 
      dplyr::group_by(crashes) %>%
      dplyr::summarise(n = dplyr::n()) %>% 
      dplyr::mutate(perc = round(as.numeric(n/total_crashes)*100, 2))
    
    
    # create table of crashes by mode
    crash_by_mode_table <- as.data.frame(crash_by_mode)
    colnames(crash_by_mode_table) <- c("Crashes", "Total Crashes", "Percent of Total")
    crash_by_mode_table["Percent of Total"] <- sapply(crash_by_mode_table["Percent of Total"], gsub, pattern = ",", replacement= ".")
    df <- data.frame(one='Total',
                     two=sum(crash_by_mode_table[2]),
                     three=100.0
    )
    colnames( df ) <- c("Crashes", "Total Crashes", "Percent of Total")
    crash_by_mode_table <- rbind(crash_by_mode_table, df)
    

    # create table of crashes by functional classification for charts 
    fclass_crashes <- get_crashes_sev_fclass(crashes, 'All Crashes')
    
    # create table of crashes by functional classification for table
    fclass_crashes_table <- fclass_crashes
    colnames( fclass_crashes_table ) <- c('Functional Classification', 'Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)')
    
    # create table of crashes by severity
    crashes_by_severity_chart <- crashes %>% 
      dplyr::group_by(severity_mapped) %>%
      dplyr::summarise(n = dplyr::n()) %>% 
      dplyr::mutate(perc = round(as.numeric(n/total_crashes)*100, 2))
    
    # create table of crashes by severity
    crashes_by_severity_table <- as.data.frame(crashes_by_severity_chart)
    colnames(crashes_by_severity_table) <- c("Severity","Count","Percent of Total")
    crashes_by_severity_table["Percent of Total"] <- sapply(crashes_by_severity_table["Percent of Total"], gsub, pattern = ",", replacement= ".")
    df <- data.frame(one='Total',
                     two=sum(crashes_by_severity_table[2]),
                     three=100.0
    )
    colnames(df) <- c("Severity","Count","Percent of Total")
    crashes_by_severity_table <- rbind(crashes_by_severity_table, df)
    
    # create table of crashes by severity and mode for stacked bar chart
    count1 <- crashes %>% 
      filter(crashes == 'Bicycle Crashes') %>%  
      group_by(severity_mapped) %>%      
      dplyr::summarise(n = dplyr::n()) 
    count2 <- crashes %>% 
      filter(crashes == 'Pedestrian Crashes') %>%  
      group_by(severity_mapped) %>%      
      dplyr::summarise(n = dplyr::n()) 
    count3 <- crashes %>% 
      filter(crashes == 'Other Crashes') %>%  
      group_by(severity_mapped) %>%      
      dplyr::summarise(n = dplyr::n())  
    
    empty <- as.data.frame( c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'))
    colnames(empty) <- c("severity_mapped")
    
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
    crash_by_severity_stacked_chart <- table %>% 
      select(-c("Bicycle_count","Pedestrian_count", "Other_count"))
    crash_by_severity_stacked_chart[is.na(crash_by_severity_stacked_chart)] <- 0
    
    # create table of crashes by year
    to_download_crash_by_year <- crashes %>%
      dplyr::group_by(crash_year, severity_mapped) %>%
      replace(is.na(.), 0) %>%
      dplyr::summarise(count = dplyr::n())
    to_download_crash_by_year <- to_download_crash_by_year %>% tidyr::pivot_wider(names_from=c(severity_mapped), values_from=count) %>% 
      replace(is.na(.), 0)
    to_download_crash_by_year[c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)')[!(c('Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)') %in% colnames(to_download_crash_by_year))]] = 0
    names(to_download_crash_by_year)[names(to_download_crash_by_year) == "crash_year"] <- "Year"

    # get crosswalk tables 
    crash_cost_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                         schema = DBI::dbQuoteIdentifier(connection, 'local_user_data'),
                                                         table = DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
                                                         values = c('severity_mapped', 'crashes_costs_usdot'))
    names(crash_cost_crosswalk_t)[names(crash_cost_crosswalk_t)=="crashes_costs_usdot"] <- "Crash Cost"
    names(crash_cost_crosswalk_t)[names(crash_cost_crosswalk_t)=="severity_mapped"] <- "Severity"
    
    crash_mode_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                         schema = DBI::dbQuoteIdentifier(connection, 'local_user_data'),
                                                         table = DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
                                                         values = c(glue::glue('{mode_column}'), 'usdot_mode_mapped'))
    names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)=="severity_mapped"] <- "Standard Mode"
    names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)==glue::glue("{mode_column}")] <- "Your Dataset's Mode"
    names(crash_mode_crosswalk_t)[names(crash_mode_crosswalk_t)=="usdot_mode_mapped"] <- "Standard Mode"
    
    crash_sev_crosswalk_t <- get_crosswalk_table_points(connection=connection, 
                                                        schema = DBI::dbQuoteIdentifier(connection, 'local_user_data'),
                                                        table = DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id)),
                                                        values = c(glue::glue('{sev_column}'), 'severity_mapped'))
    names(crash_sev_crosswalk_t)[names(crash_sev_crosswalk_t)=="severity_mapped"] <- "Standard Severity"
    names(crash_sev_crosswalk_t)[names(crash_sev_crosswalk_t)==glue::glue("{sev_column}")] <- "Your Dataset's Severity"
    
  } else {
    crash_cost_crosswalk_t <- NULL
    crash_mode_crosswalk_t <- NULL
    crash_sev_crosswalk_t <- NULL
    crash_counts <- NULL
    to_download_crash_by_year <- NULL
    crash_by_mode <- NULL
    crash_by_mode_table <- NULL
    fclass_crashes <- NULL
    crashes_by_severity_chart <- NULL
    crashes_by_severity_table <- NULL
    crash_by_severity_stacked_chart <- NULL
    fclass_crashes_table <- NULL
  }
  
  rd_exists <- test_if_table_exists(connection=connection, schema=DBI::dbQuoteLiteral(connection, 'local_user_data'), table=DBI::dbQuoteLiteral(connection, return_table_name('roads', user_id, run_id)))
  fclass_column <- get_user_account_value(connection=connection, column=DBI::dbQuoteIdentifier(connection, 'roads_fun_c_col'), user_id=user_id, run_id=DBI::dbQuoteLiteral(connection, run_id)) 
  if (rd_exists && !is.null(fclass_column)){
    road_fclass_crosswalk_t <- get_crosswalk_table_lines(connection=connection, 
                                                         schema = DBI::dbQuoteIdentifier(connection, 'local_user_data'),
                                                         table = DBI::dbQuoteIdentifier(connection, return_table_name('roads', user_id, run_id)),
                                                         values = c(glue::glue('{fclass_column}'), 'usdot_fun_class_mapped'))
    names(road_fclass_crosswalk_t)[names(road_fclass_crosswalk_t)=="usdot_fun_class_mapped"] <- "Standard Functional Class"
    names(road_fclass_crosswalk_t)[names(road_fclass_crosswalk_t)==glue::glue("{fclass_column}")] <- "Your Dataset's Functional Class"
    road_fclass_crosswalk_t[['Total Miles']] <- round(as.numeric(road_fclass_crosswalk_t[['Total Miles']]), 2)
    
  } else {
    road_fclass_crosswalk_t <- NULL
  }
  
  
  if (cr_exists && rd_exists) {
    roads_length <- get_roads(c=connection, user_id=user_id, run_id=run_id)
    # create table of crashes by functional classification 
 
    to_download_crash_by_fclass <- get_crashes_sev_fclass(crashes, 'All Crashes')
    to_download_crash_by_fclass <-to_download_crash_by_fclass %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(tot = sum(c(k, a, b, c, o)))
    to_download_crash_by_fclass$total <- sum(to_download_crash_by_fclass$tot)
    to_download_crash_by_fclass <- to_download_crash_by_fclass %>% 
      dplyr::mutate(perc = round(as.numeric(tot/total)*100, 2)) %>% 
      dplyr::select(-c(k, a, b, c, o, total)) %>% 
      dplyr::left_join(roads_length, by = c("fclass_mapped" = "usdot_fun_class_mapped") ) %>% 
      dplyr::mutate(density = round(as.numeric(tot/miles), 2)) %>% 
      dplyr::mutate_if(is.numeric, ~round(., 2)) %>% 
      dplyr::mutate_if(is.numeric, ~gsub(",", ".", .))  
    colnames(to_download_crash_by_fclass) <- c("Functional Classification","Total Crashes","Percent of Total Crashes", "Total Miles", "Crashes Per Mile")

  } else {
    crash_by_severity_stacked_chart <- NULL
  }
  
  rmarkdown::render(
    input = file.path(getwd(), "report_file.Rmd"),
    output_file = file.path(getwd(), "built_report_rm.pdf"),
    
    params = list(
      
      study_area = study_area_d,
      
      crash_counts=crash_counts,
      
      road_fclass_crosswalk = road_fclass_crosswalk_t, 
      crash_cost_crosswalk = crash_cost_crosswalk_t, 
      crash_mode_crosswalk = crash_mode_crosswalk_t, 
      crash_sev_crosswalk = crash_sev_crosswalk_t, 
      
      mode_table = crash_by_mode_table,
      mode_chart = crash_by_mode,
      fclass_chart = fclass_crashes,
      fclass_table = fclass_crashes_table,
      
      severity_chart = crashes_by_severity_chart,
      severity_table = crashes_by_severity_table,
      severity_stacked = crash_by_severity_stacked_chart,
      
      year_table = to_download_crash_by_year,
      
      top_10_segs_table_ped = top_10_ped,
      top_10_segs_map_ped = top_ten_map_ped,
      
      top_10_segs_table_bike = top_10_bike,
      top_10_segs_map_bike = top_ten_map_bike,
      
      top_10_segs_table_other =top_10_other,
      top_10_segs_map_other = top_ten_map_other,
      
      crash_density = crash_density_results,
      
      model_results_bike = bk_data,
      model_results_ped = pd_data,
      
      model_results_severity_ped=barc_data_ped,
      model_results_severity_bike=barc_data_bike,
      
      run_id=run_id
    )
  )
  
  # send file to S3 here, then uncomment blocks below
  
  # files1 <- list(list.files(pattern = "\\_rm.html$"))
  # do.call(file.remove, files1)
  # 
  # files2 <- list(list.files(pattern = "\\_rm.png$"))
  # do.call(file.remove, files2)
  # 
  # files3 <- list(list.files(pattern = "\\_rm.pdf$"))
  # do.call(file.remove, files3)
  
  
}
