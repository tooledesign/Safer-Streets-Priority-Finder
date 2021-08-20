
logout_model <- modalDialog(
  title = "Logout",
  easyClose = FALSE,
  footer = NULL,
  next_label = NULL,
  # create login 
  tagList(
    tags$div(class = 'login_well',
             p("Are you sure you want to logout?"),
    ),
    tags$div(
      id = "logout_buttons", class = 'login_buttons leaflet_block',
      actionButton("logout", "Yes, logout", class = 'btn btn-primary'),
      actionButton("dont_logout", "No, don\'t logout", class = 'btn btn-primary')
    )
  )
)

observeEvent(input$logout_model_drop, {
  showModal(logout_model)
})

observeEvent(input$logout, {
  session$reload()
})

observeEvent(input$dont_logout, {
  removeModal()
})


create_login_modal <- modalDialog(
  title = "Let's Get Started",
  easyClose = FALSE,
  footer = NULL,
  next_label = NULL,
  size = 'm',
  # create login 
  tagList(
    tags$div(id='welcome_login', class = 'login_well login_block',
             h4('Welcome to the Safer Streets Priority Finder!'),
             p("The Safer Streets Priority Finder enables you to analyze the risk to bicyclists and pedestrians on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:"),
             fluidRow(
               col_12(
                 tags$ul(
                   tags$li(
                     "Explore descriptive statistics related to your crash data"
                   ),
                   tags$li(
                     "Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network"
                   ),
                   tags$li(
                     "Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently"
                   )
                 )
               )
             ),
             p(tags$i("PLEASE NOTE: This tool is currently in beta phase. Some analyses may take several minutes to run when there are concurrent users. Thank you for your patience!")),
             hr(),
             tags$div(class='login_buttons',
                      actionButton("login_start", "Let's Go!", class = 'btn btn-primary login_buttons')
             )), 
    tags$div(
      tags$div(class = 'login_well login_buttons',
               p("Create an account to save your first study or log in to access your past studies or add a new study to your existing account."),
      ),
      id = "access_buttons", class = 'login_buttons leaflet_none',
      tags$div(id='login_buttons',
               actionBttn(
                 inputId = "create_account_button",
                 label = "Create Account", 
                 style = "fill",
                 color = "default",
                 size = 's'
               ),
               actionBttn(
                 inputId = "to_login_button",
                 label = "Login", 
                 style = "fill",
                 color = "default",
                 size = 's'
               ),
               actionBttn(
                 inputId = "more_information",
                 label = "More Information", 
                 style = "fill",
                 color = "default",
                 size = 's'
               ))
    ),
    tags$div(id='create_login', class = 'leaflet_none login_well',
             p("Please choose a username and new study name."),
             tags$div(id = 'small_intro', tags$small('This information is used so that you can customize and access your analysis.')),
             tags$div(class="inputs_entry_modals",
                      tags$div(class="mb-2 inputs_entry_modals", textInput("chosen_username", label = 'Username')),
                      tags$div(class="mb-2 inputs_entry_modals", passwordInput("chosen_password", label = 'Password')),
                      tags$div(class="mb-2 inputs_entry_modals", textInput("chosen_run_id", label = 'Study Name')),
                      tags$div(class="mb-2 inputs_entry_modals", textInput("chosen_email", label = 'Email Address'))
             ),
             hr(),
             tags$div(class='login_buttons',
                      actionButton("create_login_go_back", "Go Back", class = 'btn btn-primary'),
                      actionButton("modal_button_create_login", "Create Login", class = 'btn btn-primary')
             )
    ), 
    
    tags$div(id='to_login', class = 'leaflet_none login_well',
             title = "Login to Review Your Analysis",
             easyClose = TRUE,
             footer = NULL,
             next_label = NULL,
             p("Please enter your username and study name."),
             tags$div(class="inputs_entry_modals",
                      tags$div(class="mb-2 inputs_entry_modals", textInput("username", label = 'Username')),
                      tags$div(class="mb-2 inputs_entry_modals", passwordInput("password", label = 'Password'))
             ),
             hr(),
             tags$div(class='login_buttons',
                      actionButton("login_go_back", "Go Back", class = 'btn btn-primary'),
                      actionButton("modal_button_login", "Next", class = 'btn btn-primary ')
             )
    ), 
    tags$div(id='login_information', class = 'leaflet_none login_well',
             title = "Login Information",
             easyClose = TRUE,
             footer = NULL,
             next_label = NULL,
             h4('Welcome to the Safer Streets Priority Finder!'),
             p("This site enables you to analyze the risk to vulnerable road users (bicyclists and pedestrians) on your community’s roads. You can use your local road, crash, and study area data or select from nationally available datasets to:"),
             fluidRow(
               col_12(
                 tags$ul(
                   tags$li(
                     "Explore descriptive statistics related to your crash data"
                   ),
                   tags$li(
                     "Develop a Sliding Windows Analysis using historical crash data to inform a High Injury Network"
                   ),
                   tags$li(
                     "Develop a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently"
                   )
                 )
               )
             ),
             h4("First-Time users"),
             tags$ul(
               tags$li(
                 "The user needs a username, password, email address, and a study name to create a login."
               ),
               tags$li(
                 "Please be advised that the password is not encrypted. We suggest that you avoid using any password that is used for your other secure logins for security reasons."
               ),
               tags$li(
                 "There is currently not an option to retrieve a lost username, password, or study name."
               )
             ),
             h4("Returning users"),
             p(
               "Returning users can use their username, password, and study name to: load input data, modify existing data, overwrite existing results, and access existing results"
             ),
             tags$ul(
               tags$li(
                 "A user can access the results of the study during a different session."
               ),
               tags$li(
                 "A user can have multiple studies associated with their username. The inputs and results of one study will not affect that of another study."
               )
             ),
             tags$div(class='login_buttons',
                      actionButton("info_go_back", "Go Back", class = 'btn btn-primary')
             )
    ), 
    tags$div(id='scenario_choice', class = 'leaflet_none login_well',
             title = "Login to Review Your Analysis",
             easyClose = TRUE,
             footer = NULL,
             next_label = NULL,
             p("Please enter your study area."),
             tags$div(class="inputs_entry_modals",
                      pickerInput(
                        inputId = "scenario_input",
                        label = "Select a Study: ",
                        choices = c("Select a Study")
                      ),
             ),
             hr(),
             tags$div(class='login_buttons',
                      actionButton("scenario_go_back", "Go Back", class = 'btn btn-primary'),
                      actionButton("create_scenario", "Create a New Study", class = 'btn btn-primary'),
                      actionButton("scenario_login", "Login", class = 'btn btn-primary ')
             )
    ), 
    tags$div(id='add_scenario', class = 'leaflet_none login_well',
             title = "Add An Additional Study",
             easyClose = TRUE,
             footer = NULL,
             next_label = NULL,
             p("Please enter your username and new study."),
             tags$div(class="inputs_entry_modals",
                      textInput("additional_scenario", label = 'Study ID')
             ),
             hr(),
             tags$div(class='login_buttons',
                      actionButton("add_scenario_go_back", "Go Back", class = 'btn btn-primary '),
                      actionButton("modal_button_add_scenario", "Add a New Study and Login", class = 'btn btn-primary ')
             )
    )
  )
)

observeEvent(input$login_start, {
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#welcome_login").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#welcome_login").addClass("leaflet_none");'))
})
observeEvent(input$create_account_button, {
  shinyjs::runjs(code = paste0('$("#create_login").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#create_login").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_none");'))
})
observeEvent(input$create_login_go_back, {
  shinyjs::runjs(code = paste0('$("#create_login").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#create_login").addClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_block");'))
})
observeEvent(input$info_go_back, {
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#login_information").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#login_information").addClass("leaflet_none");'))
})
observeEvent(input$more_information, {
  shinyjs::runjs(code = paste0('$("#login_information").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#login_information").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_none");'))
})
observeEvent(input$to_login_button, {
  shinyjs::runjs(code = paste0('$("#to_login").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#to_login").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_none");'))
})
observeEvent(input$login_go_back, {
  shinyjs::runjs(code = paste0('$("#to_login").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#to_login").addClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_block");'))
})
observeEvent(input$add_scenario_button, {
  shinyjs::runjs(code = paste0('$("#add_scenario").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#add_scenario").addClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#access_buttons").addClass("leaflet_none");'))
})
observeEvent(input$create_scenario, {
  shinyjs::runjs(code = paste0('$("#scenario_choice").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#scenario_choice").addClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#add_scenario").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#add_scenario").addClass("leaflet_block");'))
})
observeEvent(input$add_scenario_go_back, {
  shinyjs::runjs(code = paste0('$("#add_scenario").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#add_scenario").addClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#scenario_choice").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#scenario_choice").addClass("leaflet_block");'))
})
observeEvent(input$scenario_go_back, {
  shinyjs::runjs(code = paste0('$("#scenario_choice").removeClass("leaflet_block");'))
  shinyjs::runjs(code = paste0('$("#scenario_choice").addClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#to_login").removeClass("leaflet_none");'))
  shinyjs::runjs(code = paste0('$("#to_login").addClass("leaflet_block");'))
})