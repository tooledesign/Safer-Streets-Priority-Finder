
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
print(paste0('Creating pool connection at: ', Sys.time()))
connection <- create_postgresql_connection(
    dbname = Sys.getenv("SSPF_AMAZON_DATABASE"),
    host = Sys.getenv("SSPF_AMAZON_HOST_ADDRESS"),
    user = Sys.getenv("SSPF_AMAZON_USERNAME"),
    password = Sys.getenv("SSPF_AMAZON_PASSWORD")
)
 
shinyjs::delay(500, shinybusy::hide_spinner())

print_time <- reactiveValues(v=TRUE)
observe({
  invalidateLater(1000)
   if(!connection$valid){
     if (print_time$v) {
       print(paste0('Pool connection is not valid at: ', Sys.time()))
     }
     shinyjs::runjs(code = paste0('$("#shiny-modal").removeClass("show");'))
     shinyalert(
       title = 'Disconnected',
       text = 'Database has been disconnected. Please reload the page.',
       size = "s", 
       closeOnEsc = TRUE,
       closeOnClickOutside = FALSE,
       html = FALSE,
       type = "warning",
       showConfirmButton = T,
       showCancelButton = F,
       confirmButtonText = "OK",
       confirmButtonCol = "#AEDEF4",
       timer = 0,
       callbackJS = "function(x) { location.reload(); }"
     )
     print_time$v <- FALSE
     #session$close()
   }
})

# close the local pool connection when the session ends 
session$onSessionEnded(function(){
  print(paste0('Disconnected from server at: ', Sys.time()))
  if (connection$valid){
    print(paste0('Returned pool connection at: ', Sys.time()))
    pool::poolClose(connection)
  }
})


output$app <- renderUI({
  tryCatch({
    page(data$user_id, data$run_id)
  }, error = function(cond){
    print(cond)
  })
})   