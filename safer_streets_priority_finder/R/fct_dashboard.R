
# function to collect crash estimates from model results 
cr_collector <- function(columns, data_input) {
tryCatch({
  b <- as.data.frame(data_input) %>% 
    dplyr::select(columns) %>%
    replace(is.na(.), 0) %>%
    dplyr::summarise_all(sum)

  return(b)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# function to create model fit table 
get_fit_data <- function(connection, table, est_columns, column_rename, user_id, run_id, mode){
  tryCatch({
  col_num = 5
  type = 'Type'
  min_col=1

  est_b <- cr_collector(est_columns, table)

  names(est_b) <- column_rename
  est_b <- est_b %>% dplyr::mutate(dplyr::across(everything(), as.character))
  est_b <- est_b %>% tidyr::pivot_longer(cols=c(min_col:col_num))
  mode=DBI::dbQuoteLiteral(connection, mode)
 
  q <- glue::glue('
                  SELECT 
                    severity as name,
                    COUNT(*)
                  FROM model_output_scratch.hin_crashes_{user_id}_{run_id}
                  WHERE mode = {mode}
                  GROUP BY severity
                ;
              ')
  crashes <- DBI::dbGetQuery(connection, q)

  crashes$name[crashes$name=='k'] <- 'Fatality (K)'
  crashes$name[crashes$name=='a'] <- 'Incapacitating Injury (A)'
  crashes$name[crashes$name=='b'] <- 'Non-Incapacitating Injury (B)'
  crashes$name[crashes$name=='c'] <- 'Possible Injury (C)'
  crashes$name[crashes$name=='o'] <- 'Property Damage Only (O)'
  
  if (nrow(crashes) < 1) {
    crashes <- data.frame(name= c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)",  "Possible Injury (C)", "Property Damage Only (O)"), 
                          count=c(0, 0, 0, 0, 0)) 
  }
  
  # add any missing columns 
  for (i in column_rename) {
    if (!(any(crashes[1]==i))) {
      crashes[nrow(crashes) + 1,] = c(i,0)
    }
  }

  barc_data_fun <- merge(est_b, crashes, by='name')
  names(barc_data_fun) <- c(type, 'Estimated', 'Observed')
  return(barc_data_fun)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# function to create table of crashes by functional classification 
get_crashes_sev_fclass <- function(table, filter){
  tryCatch({
 if (filter != 'All Crashes') {
   f <- table %>% 
     dplyr::filter(usdot_mode_mapped == filter)
 } else {
   f <- table 
 }

 f <- f %>%
          dplyr::group_by(fclass_mapped, severity_mapped) %>%
          replace(is.na(.), 0) %>%
          dplyr::summarise(count = dplyr::n()) 
  f <- f %>% tidyr::pivot_wider(names_from=c(severity_mapped), values_from=count) %>% 
    replace(is.na(.), 0)  
   g <- c("Fatality (K)", "Incapacitating Injury (A)", "Non-Incapacitating Injury (B)", "Possible Injury (C)", "Property Damage Only (O)" )
   f[g[!(g %in% colnames(f))]] = 0
   f <- f %>% dplyr::select(g)
   colnames(f) <- c("fclass_mapped", "k","a","b","c","o")
  return(f)  
   }, 
 error = function(cond){
   c <- toString(cond)
   Add_Modal(title = 'Something Went Wrong.', body=c)
 })
}

# download top ten crashes, does not filter out zero values 
get_dangerous_locations <- function(c, the_input, user_id, run_id){
  tryCatch({
  input_table=DBI::dbQuoteIdentifier(c, the_input)
  q <- glue::glue('
                 WITH summary AS 
                 (
                  SELECT road_name, road_fclass, {input_table}, ST_AsEWKT(geom) as geom, 
                         ROW_NUMBER() OVER(PARTITION BY road_name 
                                               ORDER BY {input_table} DESC) AS rank
                  FROM sliding_windows_outputs.sw_sliding_windows_{user_id}_{run_id}
                  )
                   SELECT *
                      FROM summary
                   WHERE rank = 1
                   ORDER BY {input_table} DESC
                   LIMIT 10
                    ;
                  ')
  
  f <- DBI::dbGetQuery(c,q)
  
  f[1][is.na(f[1])] <- 'Unknown Road Name'
  colnames(f ) <- c("Name","Functional Class","Crash Score", 'geom')
  return(f)   
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# downloads crashes
get_crashes <- function(c, user_id, run_id){
  tryCatch({
  q <- glue::glue('SELECT tdg_id_{user_id}_{run_id}, 
                              usdot_mode_mapped, 
                              CASE 
                                WHEN usdot_mode_mapped = \'Pedestrian Crash\' THEN \'Pedestrian Crashes\' 
                                WHEN usdot_mode_mapped = \'Bicycle Crash\' THEN \'Bicycle Crashes\' 
                                WHEN usdot_mode_mapped = \'Other Crash\' THEN \'Other Crashes\' 
                                WHEN usdot_mode_mapped = \'Vehicular Crash\' THEN \'Vehicle Crashes\' 
                                WHEN usdot_mode_mapped = \'Omit From Analysis\' THEN \'Omited Crashes\'
                                ELSE \'NA\' 
                                END as crashes,
                              severity_mapped, 
                              crashes_costs_usdot,
                              {DBI::dbQuoteIdentifier(c, as.character(get_crash_year(c, user_id, run_id)))} as crash_year,
                              fclass_mapped
                      FROM local_user_data.crashes_{user_id}_{run_id}
                      WHERE in_sa_{user_id}_{run_id} 
                      AND in_sa_{user_id}_{run_id} IS NOT NULL
                      AND usdot_mode_mapped != \'Omit From Analysis\'
                      AND severity_mapped != \'Omit From Analysis\'
                      AND fclass_mapped != \'\' 
                      AND fclass_mapped IS NOT NULL;')
  crashes <- DBI::dbGetQuery(c, q)
  return(crashes) 
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# downloads roads 
get_roads <-function(c, user_id, run_id){
  tryCatch({
  l <- glue::glue('SELECT rd.usdot_fun_class_mapped, 
                      SUM(ST_LENGTH(rd.geom)/1609.34) as miles 
                      FROM local_user_data.roads_{user_id}_{run_id} rd, 
                      local_user_data.study_area_{user_id}_{run_id} sa
                      WHERE rd.usdot_fun_class_mapped != \'Omit From Analysis\' 
                      AND ST_INTERSECTS(rd.geom, sa.geom)
                      GROUP BY rd.usdot_fun_class_mapped;')
  
  roads <- DBI::dbGetQuery(c, l)
  return(roads)
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# downloads model results 
get_modeloutputs <- function(c, bike_est_sev_cols, bike_his_sev_cols, ped_est_sev_cols, ped_his_sev_cols, user_id, run_id){
  tryCatch({
  es <- glue::glue('SELECT * FROM model_outputs.hin_output_roads_{user_id}_{run_id}')
  est_table <- DBI::dbGetQuery(c,es)
  est_table[c(bike_est_sev_cols, bike_his_sev_cols, ped_est_sev_cols, ped_his_sev_cols)[!(c(bike_est_sev_cols, bike_his_sev_cols, ped_est_sev_cols, ped_his_sev_cols) %in% colnames(est_table))]] = 0
  return(est_table)  
  }, 
error = function(cond){
  c <- toString(cond)
  Add_Modal(title = 'Something Went Wrong.', body=c)
})
}

