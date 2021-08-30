#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @import sf
#' @import bs4Dash
#' @import shinyjs
#' @import shinyalert

# primary server file for the Safer Streets Priority Finder Tool 

app_server <- function( input, output, session ) {
  
  # set local environment in docker container 
  setwd('/srv/shiny-server/App')

  # load scripts 
  source(file.path(getwd(), 'env_variables.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'opener_setup.R'), local = TRUE)
  source(file.path(getwd(), 'inst', 'app', 'www', 'welcome_login_modal.R'), local = TRUE)
  
  
  
  # prompts user with login modal after three seconds 
  shinyjs::delay(3000, showModal(create_login_modal))

  # as soon as the user logs in, this observer will populate several features in the application 
  observe({
    if ( !is.null(data$user_id) && !is.null(data$run_id) ) {
      
      # sterilizing user names and run ids
      u=DBI::dbQuoteLiteral(connection, data$user_id)
      r=DBI::dbQuoteLiteral(connection, data$run_id)
      q <- glue::glue("SELECT o_username, o_run_id 
                        FROM gen_management.accounts 
                        WHERE user_id={u} AND run_id={r};")
      info <- DBI::dbGetQuery(connection, q)
      
      # populating empty UIs 
      output$logout_header_area <- renderUI(
        tags$div(id = 'logout',  style = "padding-right:10px", 
                 tags$div(id = 'top_right',
                   fluidRow(
                           # The first two are informative badges with the login credentials 
                           bs4Dash::bs4Badge(span(info[1], style="color:white; font-size:.84rem"), status="secondary", rounded=F),
                           br(),
                           bs4Dash::bs4Badge(span(info[2], style="color:white; font-size:.84rem"), status="secondary", rounded=F),

                           # social media tags 
                           shiny::a(shiny::actionButton(inputId="contact_devs", icon = icon("envelope"), label="", class="share_communication_top"),  
                                    href = "mailto:saferstreetspriorityfinder@tooledesign.com?subject=SSPF Developer Message", target="_blank", rel="noopener noreferrer"),
                           shiny::a(shiny::actionButton(inputId="facebook_share", icon = icon("facebook-square"), label="", class="share_communication_top"), 
                                    href = "https://www.facebook.com/sharer/sharer.php?u=https%3A//www.saferstreetspriorityfinder.com/", target="_blank"),
                           shiny::a(shiny::actionButton(inputId="twitter_share", icon = icon("twitter"), label="", class="share_communication_top"),
                                    href = "https://twitter.com/intent/tweet?text=Check%20out%20this%20great%20web%20application!%20The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20https%3A//www.saferstreetspriorityfinder.com/", target="_blank"),
                           shiny::a(shiny::actionButton(inputId="linkedin_share", icon = icon("linkedin"), label="", class="share_communication_top"),
                                    href = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//www.saferstreetspriorityfinder.com/&title=Safer%20Streets%20Priority%20Finder&summary=The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20&source=https%3A//www.saferstreetspriorityfinder.com/", target="_blank"),
                           shiny::a(shiny::actionButton(inputId="github_link", icon = icon("github"), label="", class="share_communication_top"),
                                    href = "https://github.com/tooledesign/Safer-Streets-Priority-Finder/", target="_blank"),
                           # logout button 
                           shiny::actionButton("logout_model_drop", icon = icon("sign-out"), label="", class="share_communication_top")
                           )
        ))
      ) 
    } else {
      # If the username and run id are not available, render nothing 
      output$logout_header_area <- renderUI({})
    }
  })
  
  
  
  ### Create Account, this module creates a new login username/scenario identification.
  # checks required 
  # 1. check for missing fields 
  # 2. check for spaces, none allowed 
  # 3. check for existing username
  # 4. check for special characters in the run id, only underscores, letters, and numbers are currently allowed. This parameter is required because the run ID is used throughout the application as a unique identifier for table names, indexes, etc
  
  # 1. observe event, create login (new user) 
  observeEvent(input$modal_button_create_login, {
    
    # check for null values across the inputs (username, run_id, email, password)
    if ( is.null(input$chosen_username) || is.na(input$chosen_username) ||  input$chosen_username == "" ||
         is.null(input$chosen_run_id) || is.na(input$chosen_run_id) || input$chosen_run_id == "" ||
         is.null(input$chosen_email) || is.na(input$chosen_email) || input$chosen_email == "" ||
         is.null(input$chosen_password) || is.na(input$chosen_password) || input$chosen_password == ""
    ) {        
      shiny_warming_alert(
        title = "Whoa!",
        text = "You've missed something! Please fill out all the fields."
      )
      
      # 2. if passes first check, check for spaces, none allowed 
    } else if (grepl('\\s+',input$chosen_username) || grepl('\\s+',input$chosen_run_id) || grepl('\\s+',input$chosen_email) || grepl('\\s+',input$chosen_password) ){
      shiny_warming_alert(
        title = "No Spaces Allowed!",
        text = "Sorry, please don't use any spaces."
      )
      
      # 3. if passes second check, check for special characters, only underscores allowed.  
    } else if (grepl('[^[:alnum:]\\-\\_\\s]', input$chosen_run_id)) {
      shinyalert(title = 'No special characters allowed.', text='Sorry, please only use letters and integers for the study name.', type='warning')
      
    } else {
      
      # after all checks on inputs have passed, the tool now checks for an existing username 
      # the add_user() function returns a true/false, true if username already exists 
      
      # call user id 
      user_added <- FALSE
      
      # Check for existing username 
      user_added <- add_user(connection=connection, 
                             username = tolower(input$chosen_username), 
                             run_id=tolower(input$chosen_run_id),
                             email=tolower(input$chosen_email),
                             password=tolower(input$chosen_password),
                             original_username=input$chosen_username,
                             original_run_id=input$chosen_run_id
      )
        if (user_added == 'Username already exists'){
          shiny_warming_alert( title = "Oh no!", text = "That username already exists!")
        } else if (user_added == 'Error occured'){
          shiny_warming_alert( title = "Error", text = "It looks like the SSPF ran into an error. You're likely disconnected from the database. Try reloading the page." )
          
          } else if (user_added == 'User added') {
          # checks 1-4 have passed, create username login
       
            data$user_id <- get_user_id(connection=connection, username=tolower(input$chosen_username), password=tolower(input$chosen_password))
            data$run_id <- get_run_id(connection=connection, username=tolower(input$chosen_username), run_id=tolower(input$chosen_run_id)) 
            removeModal()
 
            waiter::waiter_show(
              color='rgba(175, 175, 175, 0.85)',
              html = tagList(
                tags$div(waiter::spin_1()),
                tags$br(),
                tags$div(HTML("Loading ..."))
              )
            )
            
            # call the servers for each module 
            callModule(mod_load_data_server, "load_data_ui_1", connection=connection, user_id=data$user_id, run_id=data$run_id)
            callModule(mod_reporter_server, "reporter_ui_1", connection=connection, user_id=data$user_id, run_id=data$run_id)
            callModule(mod_build_sliding_windows_server, "build_sliding_windows_ui_1", connection=connection, user_id=data$user_id, run_id=data$run_id)
            callModule(mod_build_model_server, "build_model_ui_1", connection=connection, user_id=data$user_id, run_id=data$run_id)
            callModule(mod_visualize_data_reporter_server, "visualize_data_reporter_ui_1", connection=connection, user_id=data$user_id, run_id=data$run_id)
         
            # update time the user last logged in 
            DBI::dbGetQuery(connection, glue::glue(" UPDATE gen_management.accounts SET last_login = NOW() WHERE user_id = {data$user_id} AND run_id = \'{data$run_id}\'; "))

            
            login_configs <- callModule(mod_login_config_server, "login_config_ui_1", 
                                        connection=connection, 
                                        user_id=data$user_id, 
                                        run_id=data$run_id, 
                                        model_prodcution_status=model_production_status(connection, data$user_id, data$run_id))
            
            output$login_status <- login_configs$login_status  
            output$model_prod_status <- login_configs$model_prod_status  
            
            # delay slightly to let module's time to load 
            shinyjs::delay(1200, waiter::waiter_hide())
      }}  
  })
  
  # General Login
  # checks required 
  # 1. check for missing fields 
  # 2. checks for existing username 
  # 3. checks for scenario id 
  observeEvent(input$modal_button_login, {
    # check for missing fields 
    if ( is.null(input$username) || is.na(input$username) ||  input$username == "" ||
         is.null(input$password) || is.na(input$password) || input$password == ""
    ) {        
      shiny_warming_alert(title = 'Fields missing.', text='Please identify your username and password.')
      
      # 2. check for username/passwrod combination 
    } else {
      user_id <- get_user_id(connection=connection, username=tolower(input$username), password=tolower(input$password))
      if ( user_id == -999 ) {
        shiny_warming_alert(title = 'Incorrect Login', text='It looks like that username and password combination doesn\'t exist.')
      } else if (length(user_id) == 0) {
        shiny_warming_alert(title = 'Something went wrong', text='It looks like you\'re session disconnected from the database. Please reload the page.')
      } else { # check for run ID 
        
        # set user_id 
        data$user_id <- user_id
        
        # get scenarios 
        q <- glue::glue("
                    SELECT DISTINCT o_run_id as \"Scenario IDs\"
                    FROM gen_management.accounts 
                    WHERE user_id = {data$user_id};
                    ")
        
        scenarios <- as.vector(DBI::dbGetQuery(connection, q)[,1])
 
        # update select input 
        updatePickerInput(session, "scenario_input",
                          choices = c('Select a Study', scenarios)
        )
        shinyjs::runjs(code = paste0('$("#to_login").removeClass("leaflet_block");'))
        shinyjs::runjs(code = paste0('$("#to_login").addClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#scenario_choice").removeClass("leaflet_none");'))
        shinyjs::runjs(code = paste0('$("#scenario_choice").addClass("leaflet_block");'))
        
      }
    } 
  })
  
  # observer attached to 
  observeEvent(input$scenario_login, {
    #check for run id 
    if (input$scenario_input == 'Select a Study') {
      shiny_warming_alert(title = 'Select a study!', text='')
    } else {
      ip <- DBI::dbQuoteLiteral(connection, input$scenario_input)
      q <- glue::glue("
                    SELECT DISTINCT run_id
                    FROM gen_management.accounts 
                    WHERE o_run_id = {ip}
                      AND user_id  = {data$user_id};
                    ")
      rid <- DBI::dbQuoteLiteral(connection,as.vector(DBI::dbGetQuery(connection, q)[,1]))
      run_id <- get_run_id(connection=connection, username=tolower(input$username), run_id=tolower(rid))  
      user_id <- get_user_id(connection=connection, username=tolower(input$username), password=tolower(input$password))
      if (is.null(run_id)){
        shiny_warming_alert(title = 'Whoa!', text='That study doesn\'t exist!')
      } else {
        # All checks passed, let's login! 
        removeModal()
        data$user_id <- user_id
        data$run_id <- run_id
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
     
        # Call modules after logged in 
        callModule(mod_load_data_server, "load_data_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_reporter_server, "reporter_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_build_sliding_windows_server, "build_sliding_windows_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_build_model_server, "build_model_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_visualize_data_reporter_server, "visualize_data_reporter_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        DBI::dbGetQuery(connection, glue::glue(" UPDATE gen_management.accounts SET last_login = NOW() WHERE user_id = {user_id} AND run_id = \'{run_id}\'; "))
        DBI::dbGetQuery(connection, glue::glue(" UPDATE gen_management.accounts SET total_logins = total_logins + 1 WHERE user_id = {user_id} AND run_id = \'{run_id}\'; "))   
        login_configs <- callModule(mod_login_config_server, "login_config_ui_1",  
                                    connection=connection, 
                                    user_id=data$user_id, 
                                    run_id=data$run_id, 
                                    model_prodcution_status=model_production_status(connection, data$user_id, data$run_id))
        output$login_status <- login_configs$login_status  
        output$model_prod_status <- login_configs$model_prod_status  
        shinyjs::delay(1200, waiter::waiter_hide())
    }}
  })
  
  # Add Scenario
  # checks required 
  # 1. check for missing fields 
  # 2. checks for spaces
  # 3. checks for scenario id 
  observeEvent(input$modal_button_add_scenario, {
    
    # checks for nulls
    if (  is.null(input$additional_scenario) || is.na(input$additional_scenario) || input$additional_scenario == "" ) {        
      shinyalert(title = 'Fields missing.', text='Please identify a new study name.', type='warning')
      
    # check for spaces 
    } else if (grepl('\\s+',input$additional_scenario)){
      shinyalert(title = 'No spaces allowed.', text='Sorry, there should be no spaces.', type='warning')
      
    # check for username  
    } else if (grepl('[^[:alnum:]\\-\\_\\s]', input$additional_scenario)) {
      shinyalert(title = 'No special characters allowed', text='Sorry, please only use letters and integers in the study name.', type='warning')
      
      } else {
        
        # check for account, if none - flag 
       run <- test_for_run(connection = connection, username = tolower(input$username), run_id = tolower(input$additional_scenario))
 
       if (run == 'TRUE'){
         shiny_warming_alert( title = "Whoa!", text = "That study already exists!" )
         
       }  else if (run == 'ERROR'){
         shiny_warming_alert( title = "Error", text = "It looks like the SSPF ran into an error. You're likely disconnected from the database. Try reloading the page." )         
       }  else if (run == 'FALSE'){
        
        # Adds new run to user account
        add_run(connection = connection, 
                username = tolower(input$username), 
                run_id = tolower(input$additional_scenario),
                password=tolower(input$password),
                original_run_id=input$additional_scenario)
        
        # get account information 
        run_id <- get_run_id(connection=connection, username=tolower(input$username), run_id=tolower(input$additional_scenario))
        user_id <- get_user_id(connection=connection, username=tolower(input$username), password=tolower(input$password))
        removeModal()
        data$run_id <- run_id
        data$user_id <- user_id
        
        waiter::waiter_show(
          color='rgba(175, 175, 175, 0.85)',
          html = tagList(
            tags$div(waiter::spin_1()),
            tags$br(),
            tags$div(HTML("Loading ..."))
          )
        )
        # calls modules 
        callModule(mod_load_data_server, "load_data_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_reporter_server, "reporter_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_build_sliding_windows_server, "build_sliding_windows_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_build_model_server, "build_model_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        callModule(mod_visualize_data_reporter_server, "visualize_data_reporter_ui_1", connection=connection, user_id=user_id, run_id=run_id)
        
        DBI::dbGetQuery(connection, glue::glue(" UPDATE gen_management.accounts SET last_login = NOW() WHERE user_id = {user_id} AND run_id = \'{run_id}\'; "))
        DBI::dbGetQuery(connection, glue::glue(" UPDATE gen_management.accounts SET total_logins = total_logins + 1 WHERE user_id = {user_id} AND run_id = \'{run_id}\'; "))
        
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'total_logins'), new_value='total_logins + 1')
        update_account_info(connection=connection, user_id=user_id, run_id=data$run_id_sql_formatted, column = DBI::dbQuoteIdentifier(connection, 'last_login'), new_value='NOW()')   
        
        login_configs <- callModule(mod_login_config_server, "login_config_ui_1", 
                         connection=connection, 
                         user_id=data$user_id, 
                         run_id=data$run_id, 
                         model_prodcution_status=model_production_status(connection, data$user_id, data$run_id))
        
       output$login_status <- login_configs$login_status  
       output$model_prod_status <- login_configs$model_prod_status  
       
       # delay slightly to let module's time to load 
       shinyjs::delay(1200, waiter::waiter_hide())
        }  
    }
  })
}
  
