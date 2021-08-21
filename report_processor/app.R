#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
setwd('/srv/shiny-server/App')
source(file.path(getwd(), 'env_variables.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_reporter_utilities.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_dashboard.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_db_utils.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_helpr_funs.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_proj_funcs.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_s3_file_operations.R'), local = TRUE)
source(file.path(getwd(), 'R', 'fct_report_downloader.R'), local = TRUE)


t <- TRUE
while (t) {
  do_work()
  Sys.sleep(60)
}

ui <- fluidPage()

server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
