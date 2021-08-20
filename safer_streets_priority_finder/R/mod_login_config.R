#' login_config UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_login_config_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' login_config Server Functions
#'
#' @noRd 
mod_login_config_server <- function(input, output, session, connection, user_id, run_id, model_prodcution_status){
  ns <- session$ns
 
  model_status_data <- list()
  if (model_prodcution_status == 'model_needed') {
    
    model_status_data$title = 'Model in production queue'
    model_status_data$icon =  'arrows-h'
    model_status_data$gradientColor =  'warning'
    
  } else if (model_prodcution_status == 'no_model_desired') {
    
    model_status_data$title = 'No model requested'
    model_status_data$icon =  'asterisk'
    model_status_data$gradientColor = 'info'
    
  } else if (model_prodcution_status == 'model_estimation_completed') {
    
    model_status_data$title = 'Model completed'
    model_status_data$icon =  'check-circle-o'
    model_status_data$gradientColor = 'success'
    
  } else if (model_prodcution_status == 'model_needed_running' || model_prodcution_status == 'model_currently_running' ) {
    
    model_status_data$title = 'Model in production'
    model_status_data$icon =  htmltools::HTML('<i class="fa fa-cog fa-spin fa-fw">')
    model_status_data$gradientColor = 'warning'
    
  } else {
    model_status_data$title = 'Error Something Went Wrong'
  }
  
  model_status_data$login_status <- renderUI({
    bs4InfoBox(
      tabName = 'currenly_loged_in',
      title = 'Currently Logged In',
      value = NULL,
      icon = 'check-circle-o',
      iconElevation = 4,
      status = NULL,
      gradientColor = 'success',
      width = 12,
      elevation = NULL
    )
  })
  model_status_data$model_prod_status <- renderUI({
    bs4InfoBox(
      tabName = 'model_prod_status_ui',
      title = model_status_data$title,
      value = NULL,
      icon = model_status_data$icon,
      iconElevation = 4,
      status = NULL,
      gradientColor = model_status_data$gradientColor,
      width = 12,
      elevation = NULL
    )
  })
  
  return(model_status_data)
  
}
    
## To be copied in the UI
# mod_login_config_ui("login_config_ui_1")
    
## To be copied in the server
# mod_login_config_server("login_config_ui_1")
