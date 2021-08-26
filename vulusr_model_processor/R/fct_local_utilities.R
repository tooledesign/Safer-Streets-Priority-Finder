

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
  DBI::dbGetQuery(connection, q)
}

are_year_formatted_sortof <- function(N){
  tryCatch({
    if (any((floor (log10 (abs (N))) + 1) != 4)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, error = function(cond){
    print(cond)
    return(FALSE)
  }
  )
}



get_crash_year <- function(
  connection,
  user_id,
  run_id
) {
  tryCatch({
    crash_year <- NULL
    run_id <- DBI::dbQuoteLiteral(connection, run_id)
    query <- glue::glue(" SELECT crash_o_year_col
                                FROM gen_management.accounts
                               WHERE user_id = {user_id}
                                 AND run_id = {run_id}
                               ;")
    crash_year <- DBI::dbGetQuery(connection, query)

    if ( !is.null(crash_year) ) {
      return(crash_year)
    } else {
      return(FALSE)
    }
  }, error = function(cond){
    return(FALSE)
  })
}


get_max_year <- function(
  connection,
  user_id,
  run_id
) {
  crash_year_col <- DBI::dbQuoteIdentifier(connection, as.character(get_crash_year(connection, user_id, run_id)))
  if (crash_year_col != FALSE  ){
    crash_table <- DBI::dbQuoteIdentifier(connection, return_table_name('crashes', user_id, run_id))
    run_id <- DBI::dbQuoteLiteral(connection, run_id)
    q <- glue::glue("
                    SELECT MAX({crash_year_col})
                    FROM local_user_data.{crash_table}
                    WHERE {crash_year_col} IS NOT NULL;
                    "
    )

    max_year <- DBI::dbGetQuery(connection, q)
    valid <- are_year_formatted_sortof(as.integer(max_year))
    if ( valid ) {
      return(as.integer(max_year))
    } else {
      return(FALSE)
    }
  } else {
    return (FALSE)
  }
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

get_any_needed_model_info <- function(
) {
  check_for_psql()

  models_needed <- DBI::dbGetQuery(pool, glue::glue('SELECT user_id, run_id, time_since_model_desired FROM gen_management.accounts WHERE model_status = \'model_needed\' ORDER BY time_since_model_desired DESC LIMIT 1;'))
  models_needed <- as.data.frame(models_needed)

  if ( length(models_needed) == 0 ) {
    return(NULL)
  } else {
    models_needed$time_since_int <- as.integer(models_needed$time_since_model_desired)
    return(models_needed)
  }
}

# m <- get_any_needed_model_info()

get_data_for_processing <- function(df, where_min, target_columns){
  list <- list()
  min <- min(df[[where_min]])
  for ( i in target_columns ) {
    list[[i]] <- df[[i]][df[[where_min]]==min]
  }
  if (length(list) == 0) {
    return(NULL)
  } else {
    return(list)
  }
}
# list <- get_min_data(m, 'time_since_int', c('user_id', 'run_id'))

long_job <- function(connection, user_id, run_id){
  tryCatch({
      print(paste0('Model started at, ', Sys.time()))

    # run_inputs <-  hin_0_update_inputs(user_id=user_id, run_id=run_id, con=connection, stan_model_file=file.path(getwd(), 'stan_model', 'markup_of_full_model1_rls_2021_02_03.stan'))
      run_inputs <-  hin_0_update_inputs(user_id=user_id, run_id=run_id, con=connection)

      # This runs several functions needed to create each user's model. If it fails, the user's model is placed back at the start of the line and the processor retries.
      hin_run_all(con=connection, inputs=run_inputs, user_id=user_id, run_id=run_id)

      # # delete dlls
      # dso_filenames <- dir(tempdir(), pattern=.Platform$dynlib.ext)
      # filenames  <- dir(tempdir())
      # for (i in seq(dso_filenames))
      #   dyn.unload(file.path(tempdir(), dso_filenames[i]))
      # for (i in seq(filenames))
      #   if (file.exists(file.path(tempdir(), filenames[i])) & nchar(filenames[i]) < 42) # some files w/ long filenames that didn't like to be removeed
      #     file.remove(file.path(tempdir(), filenames[i]))

      print(paste0('Model done at, ', Sys.time()))
  }, error = function(cond) {
    print(cond)
  })
}

docker_status <- function(
) {
  check_for_psql()
  status <- DBI::dbGetQuery(pool, glue::glue('SELECT is_fresh FROM gen_management.docker_status;'))
  return(status[1,1])
}

set_docker_status <- function(
) {
  check_for_psql()
  DBI::dbGetQuery(pool, glue::glue('UPDATE gen_management.docker_status SET is_fresh = FALSE;'))
}

do_work <- function (
) {
  #check to see if the docker container is fresh
  status <- docker_status()
  if (status==TRUE) {
    df <- get_any_needed_model_info()
    if (!is.null(df)) {
      list <- get_data_for_processing(df, 'time_since_int', c('user_id', 'run_id'))
      check_for_psql()
      long_job(connection=pool, user_id=list$user_id, run_id=list$run_id)
      # set a flag to indicate docker container is no longer fresh
      set_docker_status()
      } else {
      print(paste0('No models wanted at, ', Sys.time()))
    }
  }
  else {
    print(paste0('Docker container is stale. ', Sys.time()))
  }
}
