
# returns true if study exists 
test_for_run <- function(connection, username, run_id){
  tryCatch({
    username <- DBI::dbQuoteString(connection,username)
    run_id <- DBI::dbQuoteString(connection,run_id)
    q <- glue::glue('SELECT (EXISTS (SELECT FROM gen_management.accounts WHERE username = {username} AND run_id = {run_id} ))::INTEGER;')
    exists <- DBI::dbGetQuery(connection, q)[1,1] 
    if (exists  == 0 ) {
      return('FALSE')
    } else if (exists == 1) {
      return('TRUE')
    } else {
      return('ERROR')
    }
  }, 
  error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  })
}

# returns user id associated with login credentials 
get_user_id <- function(connection, username, password){
  tryCatch({
        username <-  DBI::dbQuoteString(connection, username)
        password <-  DBI::dbQuoteString(connection, password)
        user_id <- 'not connected'
        user_id <- DBI::dbGetQuery(connection, glue::glue('SELECT DISTINCT user_id 
                                                          FROM gen_management.accounts 
                                                          WHERE username = {username} 
                                                          AND password = {password};'))

        if (is.null(toString(user_id)) || is.na(toString(user_id)) || toString(user_id) == ""){
          return(-999)
        } else if (user_id == 'not connected') {
          return('not connected')
        } else {
          return(toString(user_id)) 
      }
    }, error = function(cond){
      c <- toString(cond)
      Add_Modal(title = 'Something Went Wrong.', body=c)
      }
  )
}

# returns study id associated with login credentials 
get_run_id <- function(connection, username, run_id){
  tryCatch({
  
      username <-  DBI::dbQuoteString(connection, username)
      run_id <-  DBI::dbQuoteLiteral(connection, run_id)
      run_id <- DBI::dbGetQuery(connection, glue::glue('SELECT DISTINCT run_id 
                                                        FROM gen_management.accounts 
                                                        WHERE username = {username} 
                                                        AND run_id = {run_id};'))
      
      # test for duplicate ids 
      if (length(run_id) == 1){
        return(toString(run_id))
      } else if (length(run_id) > 1){
         return('More than one run id for this user. This should not happen.')
      } else {
        return(NULL)
      }
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# returns email address associated with login credentials 
get_email_address <- function(connection, username){
  tryCatch({
    
      username <-  DBI::dbQuoteString(connection, username)
      email <- NULL
      q <- glue::glue('SELECT DISTINCT email 
                       FROM gen_management.accounts 
                       WHERE username = {username};')
      email <- DBI::dbGetQuery(connection, q)
      
      if (is.null(email)) {
        return(FALSE)
      } else {
        return(toString(email)) 
      }
  }, error = function(cond){
    
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
    
  }
  )
}

# adds new user account
add_user <- function(connection, username, run_id, email, password, original_username, original_run_id) {
  tryCatch({
    username <- DBI::dbQuoteString(connection, username)
    q <- glue::glue('SELECT (EXISTS (SELECT FROM gen_management.accounts WHERE username = {username}))::INTEGER;')
    exists <- DBI::dbGetQuery(connection, q)[1,1] 
    if (exists == 1){
      print('This username already exists, please choose something else.')
      return('Username already exists')
    } else if (exists == 0){
      run_id <- DBI::dbQuoteString(connection, run_id)
      email <- DBI::dbQuoteString(connection, email)
      password <- DBI::dbQuoteString(connection, password)
      original_username <- DBI::dbQuoteString(connection, original_username)
      original_run_id <- DBI::dbQuoteString(connection, original_run_id)
      q <- glue::glue("INSERT INTO gen_management.accounts ( username, password, user_id, run_id, email, o_username, o_run_id) VALUES ({username}, {password}, EXTRACT(EPOCH FROM NOW()), {run_id}, {email}, {original_username}, {original_run_id});")
      DBI::dbGetQuery(connection, q)
      print(glue::glue('Added username: {username}'))
      return('User added')
    } else {
      return('Error occured')
    }
    }, error = function(cond){
      c <- toString(cond)
      Add_Modal(title = 'Something Went Wrong.', body=c)
      return('Error occured')
      }
  )
  }

# adds new study
add_run <- function (connection,  username, run_id, password, original_run_id ){
  tryCatch({
    username <- DBI::dbQuoteString(connection,username)
    run_id <- DBI::dbQuoteString(connection,run_id)
    password<-DBI::dbQuoteString(connection, password)
    email <- get_email_address(connection=connection, username = username)
    email <- DBI::dbQuoteString(connection, email)
    q <- glue::glue("SELECT DISTINCT o_username
                        FROM gen_management.accounts 
                        WHERE username={username};")
    o_un <- DBI::dbGetQuery(connection, q)[,1]
    original_username <- DBI::dbQuoteString(connection, o_un)
    original_run_id <- DBI::dbQuoteString(connection, original_run_id)
    user_id <- get_user_id(connection=connection, username = username, password=password)
    q <- glue::glue("INSERT INTO gen_management.accounts ( username, user_id, password, run_id, email, o_username, o_run_id) VALUES ({username},{user_id},{password},{run_id},{email},{original_username},{original_run_id});")
    DBI::dbGetQuery(connection, q)
  }, error = function(cond){
    c <- toString(cond)
    Add_Modal(title = 'Something Went Wrong.', body=c)
  }
  )
}

# populates a fancy alert
shiny_warming_alert <- function(title, text, showConfirmButton=TRUE, showCancelButton=FALSE, size="s", timer=0, type="warning") {
  shinyalert(
    title = title,
    text = text,
    size = size, 
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = type,
    showConfirmButton = showConfirmButton,
    showCancelButton = showCancelButton,
    confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4",
    cancelButtonText = "Cancel",
    timer = timer,
    imageUrl = "",
    animation = TRUE
  )
}
