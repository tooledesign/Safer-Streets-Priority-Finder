#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_load_data_ui <- function(id){
  ns <- NS(id)
  tagList(
 
      bs4TabSetPanel(
        id = ns("data_explorer_tab_select"), 
        side='left',
          # The id lets us use input$tabset1 on the server to find the current tab

          bs4Dash::bs4TabPanel(tabName = HTML("Load Study Area&nbsp;<i class='fa fa-arrow-right' aria-hidden='true'></i>"), 
                   mod_manage_study_area_upload_ui(ns("manage_study_area_upload_ui_1"))
          ),
          bs4Dash::bs4TabPanel(tabName = HTML("Load Roads&nbsp;<i class='fa fa-arrow-right' aria-hidden='true'></i>"), 
                               mod_manage_roads_upload_ui(ns("manage_roads_upload_ui_1"))
          ),
          bs4Dash::bs4TabPanel(tabName = HTML("Load Crashes&nbsp;<i class='fa fa-arrow-right' aria-hidden='true'></i>"), 
                   mod_manage_crash_upload_ui(ns("manage_crash_upload_ui_1"))
          ),
          bs4Dash::bs4TabPanel(tabName = HTML("Confirm Input Data&nbsp;"), 
                   mod_confirm_inputs_ui(ns("confirm_inputs_ui_1"))
          )
  ),
  tags$div(br()),
  tags$div(br())
  )
}
    
#' load_data Server Function
#'
#' @noRd 
mod_load_data_server <- function(input, output, session, connection, user_id, run_id){
  ns <- session$ns

  callModule(mod_manage_study_area_upload_server, "manage_study_area_upload_ui_1", connection=connection, user_id=user_id, run_id=run_id)
  callModule(mod_manage_roads_upload_server, "manage_roads_upload_ui_1", connection=connection, user_id=user_id, run_id=run_id)
  callModule(mod_manage_crash_upload_server, "manage_crash_upload_ui_1", connection=connection, user_id=user_id, run_id=run_id)
  callModule(mod_confirm_inputs_server, "confirm_inputs_ui_1", connection=connection, user_id=user_id, run_id=run_id)
  
}
    
## To be copied in the UI
# mod_load_data_ui("load_data_ui_1")
    
## To be copied in the server
# callModule(mod_load_data_server, "load_data_ui_1")
 
