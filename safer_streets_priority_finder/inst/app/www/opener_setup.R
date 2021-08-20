
list.files(getwd())
options(shiny.maxRequestSize = 20*1024^2)
data <- reactiveValues(
  user_id = NULL,
  run_id = NULL
)
###########################
### Call db connection
###########################

# open a pooled connection when the session starts 
# This framework is beneficial for an app that may experience spikes in use because the number of connections within a pool will grow according to the demand
 
shinybusy::show_spinner()
connection <- create_postgresql_connection(
    dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
    host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
    user = Sys.getenv("SSPF_AMAZON_USERNAME"),
    password = Sys.getenv("SSPF_AMAZON_PASSWORD")
)
 
shinyjs::delay(500, shinybusy::hide_spinner())

observe({
  invalidateLater(500)
   if(!dbGetInfo(connection)$valid){
     removeModal()
     print('disconnected from psql database, disconnecting from server')
     session$close()
   }
})

# close the local pool connection when the session ends 
session$onSessionEnded(function(){
  removeModal()
  print(paste0('Returned pool connection at: ', Sys.time()))
  pool::poolClose(connection)
})


output$app <- renderUI({
  tryCatch({
    page(data$user_id, data$run_id)
  }, error = function(cond){
    print(cond)
  })
})   