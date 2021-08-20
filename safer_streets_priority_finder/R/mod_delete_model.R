#' delete_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_delete_model_ui <- function(id, confirm_button, cancel_button, text){
  ns <- NS(id)
  modalDialog(
    title = "Warning, About to Delete Analysis Results",
    easyClose = FALSE,
    footer = NULL,
    next_label = NULL,
    # create login 
    tagList(
      tags$div(class = 'well',
               p(text),
      ),
      tags$div(
        id = ns("modal_center_buttons"), class = 'login_buttons leaflet_block',
        actionButton(confirm_button, "Yes, delete my data.", class = 'btn btn-primary'),
        actionButton(cancel_button, "No, do not delete my data.", class = 'btn btn-primary')
      )
    )
  )
}
    
#' delete_model Server Functions
#'
#' @noRd 
mod_delete_model_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_delete_model_ui("delete_model_ui_1")
    
## To be copied in the server
# mod_delete_model_server("delete_model_ui_1")
