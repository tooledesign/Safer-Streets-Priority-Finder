# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency

usethis::use_package( "shinyWidgets")
usethis::use_package( "leaflet")
usethis::use_package( "sf")
usethis::use_package( "rgdal" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "shinyjs" )
usethis::use_package( "DT" )  
usethis::use_package( "tidyr" )
usethis::use_package( "promises" )
usethis::use_package( "future" )
usethis::use_package( "bs4Dash" )
usethis::use_package( "shinyalert" )
usethis::use_package( "callr" )
usethis::use_package( "plotly" )
usethis::use_package( "emayili" )
## Add modules ----
## Create a module infrastructure in R/

golem::add_module( name = "instructions" ) # instructions
golem::add_module( name = "faq" ) # frequently asked questions
golem::add_module( name = "overview" ) # overview
golem::add_module( name = "load_data" ) # Builds server side of settings page 
golem::add_module( name = "reporter" ) # dashboard
golem::add_module( name = "visualize_data_reporter" ) # builds map on data dashboard

golem::add_module( name = "build_sliding_windows" ) # builds sliding windows features
golem::add_module( name = "build_model" ) # builds model

golem::add_module( name = "visualize_sliding_windows" ) # builds sliding windows map features
golem::add_module( name = "visualize_model_results" ) # Visualizes model results

golem::add_module( name = "manage_crash_upload" ) # Manages crash upload
golem::add_module( name = "manage_roads_upload" ) # Manages roads upload
golem::add_module( name = "manage_study_area_upload" ) # manages study area of selection

golem::add_module( name = "login_config" ) # Manages login
golem::add_module( name = "delete_model" ) # returns warning modal for removing data


## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpr_funs" ) 
golem::add_fct( "db_utils" )
golem::add_fct( "login_funcs" )
golem::add_fct( "proj_funcs" )
golem::add_fct( "hin_tool" )
golem::add_fct( "map_data" )
golem::add_fct( "submit_local_data" )
golem::add_fct( "preload_local_data" )
golem::add_fct( "variable_selection_warnings" )
golem::add_fct( "process_nationally_avaiable_data" )
golem::add_fct( "handle_local_data_nulls" )
golem::add_fct( "dashboard" )
golem::add_fct( "sliding_windows_tool" )

golem::add_utils( "load_scripts" )
golem::add_utils( "theme" )
## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "customjs" )
golem::add_js_handler( "handlers" )

# Use this file as needed for css
golem::add_css_file( "overall_css" )
golem::add_css_file( "custom_login_page" )


## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "global_data", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("safer_streets_priority_finder")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

