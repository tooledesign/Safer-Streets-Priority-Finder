
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
    print("There was an error with, get_crash_year()")
    print(cond)
    return(FALSE)
  })
}

attribute_check <- function(
  data,
  attributes
) {
  tryCatch({
  if( attributes %in% names(data) ) {
    TRUE
  } else {
    FALSE
  }
  }, 
  error = function(cond){
    print("There was an error with, attribute_check()")
    print(cond)
  })
}
