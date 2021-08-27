#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
setwd('/srv/shiny-server/App')
source(file.path(getwd(), 'env_variables.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_local_utilities.R'), local = FALSE)
source(file.path(getwd(), 'R', 'fct_hin_tool.R'), local = FALSE)
#  
# source(file.path('/srv', 'shiny-server', 'App','env_variables.R'), local = TRUE)
# source(file.path('/srv', 'shiny-server', 'App', 'R', 'fct_hin_tool.R'), local = FALSE)
# source(file.path('/srv', 'shiny-server', 'App', 'R', 'fct_local_utilities.R'), local = FALSE)
t <- TRUE
while (t) {
  do_work()
  Sys.sleep(60)
}


ui <- fluidPage(
)


server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
