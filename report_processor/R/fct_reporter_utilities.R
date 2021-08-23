

update_account_info <- function (
  connection,
  user_id=NULL,
  run_id=NULL,
  column=NULL,
  new_value=NULL

){
  q <- glue::glue("
                    UPDATE gen_management.accounts
                    SET {column} = {new_value}
                    WHERE user_id = {user_id}
                    AND run_id = {run_id};
                    "
  )
  print(q)
  DBI::dbGetQuery(connection, q)
}

return_table_name <- function (
  table_name=NULL, # name of table
  user_id=NULL, # ID of user
  run_id=NULL #ID of run
) {
  table_name <-  toString(table_name)
  user_id    <-  toString(user_id)
  run_id     <-  toString(run_id)
  return(toString(glue::glue('{table_name}_{user_id}_{run_id}')))
}


check_for_psql <- function(){
  if (!exists("pool") || !DBI::dbIsValid(pool)) {
    .GlobalEnv$pool <- pool::dbPool(
      drv =  RPostgreSQL::PostgreSQL(),
      dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
      host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
      user = Sys.getenv("SSPF_AMAZON_USERNAME"),
      password = Sys.getenv("SSPF_AMAZON_PASSWORD")
    )
  }
}

get_desired_report_info <- function(
) {
  check_for_psql()

  report_needed <- DBI::dbGetQuery(pool, glue::glue('SELECT user_id, run_id, report_requested_time FROM gen_management.accounts WHERE report_status = \'report_requested\' ORDER BY report_requested_time ASC LIMIT 1;'))
  report_needed <- as.data.frame(report_needed)

  if ( length(report_needed) == 0 ) {
    return(NULL)
  } else {
    return(report_needed)
  }
}

get_data_for_processing <- function(df, where_min, target_columns){
  list <- list()
  min <- min(df[[where_min]])
  print(min)
  for ( i in target_columns ) {
    list[[i]] <- df[[i]][df[[where_min]]==min]
  }
  if (length(list) == 0) {
    return(NULL)
  } else {
    return(list)
  }
}

long_job <- function(connection, user_id, run_id){
  tryCatch({
      print(paste0('Report generation started at, ', Sys.time()))

      # generate report and saver it to aws s3
      downloader_function(connection, user_id, run_id)
      
      Sys.sleep(3) # give application a second to catch up 
      
      # send an email to user saying report is ready for download
      runid <- DBI::dbQuoteLiteral(connection, run_id)
      login_names_q <- glue::glue('SELECT o_username FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {runid};')
      login_name <- DBI::dbGetQuery(connection, login_names_q)[1,1]
      
      email_access_q <- glue::glue('SELECT email FROM gen_management.accounts WHERE user_id = {user_id} AND run_id = {runid};')
      email_access <- DBI::dbGetQuery(connection, email_access_q)[1,1]
      
      tryCatch({
        print(paste0('Sending notification to, ', email_access, '.'))
        email <- emayili::envelope(
                                  to = email_access,
                                  from = Sys.getenv("SSPF_EMAIL_ADDRESS"),
                                  subject = paste0("SSPF - Report Ready for Download"),
                                  text = paste0('This is an automated message to notify you the Safer Streets Priority Finder analysis report is ready to download for study area ', run_id,'. Thanks for using the Safer Streets Priority Finder!')
        )
        
        smtp_report <- emayili::server(host = "mail.hover.com",
                                port = 465,
                                username = Sys.getenv("SSPF_EMAIL_ADDRESS"),
                                password = Sys.getenv("SSPF_EMAIL_PASSWORD"),
                                reuse=F)
        smtp_report(email, verbose = FALSE)
      }, error = function(cond){
        print(cond)
      })

      # update accounts table
      q = glue::glue(
        "
        UPDATE gen_management.accounts
        SET
          report_status = 'report_ready',
          report_finished_time = NOW()
        WHERE
          user_id = {user_id} AND run_id = {runid}
        ;
        "
      )
      DBI::dbGetQuery(connection, q)



      print(paste0('Report generation finished at, ', Sys.time()))
  }, error = function(cond) {
    print(cond)
  })
}

do_work <- function (
) {
  df <- get_desired_report_info()
  if (!is.null(df)) {
    list <- get_data_for_processing(df, 'report_requested_time', c('user_id', 'run_id'))
    check_for_psql()
    update_account_info(connection=pool, user_id=list$user_id, run_id=DBI::dbQuoteLiteral(pool, list$run_id), column = DBI::dbQuoteIdentifier(pool, 'report_status'), new_value=DBI::dbQuoteString(pool, glue::glue('building_report')))
    long_job(connection=pool, user_id=list$user_id, run_id=list$run_id)
    } else {
    print(paste0('No reports requested at, ', Sys.time()))
  }
}
