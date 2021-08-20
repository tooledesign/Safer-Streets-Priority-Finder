library(shiny)
library(shinydashboard)
# Define UI for app that draws a histogram ----

ui <- fluidPage(
  
  dashboardPage(
    
    dashboardHeader(title = "TEST reactive DT"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("See data", tabName = "db")
      ),
      selectInput("rb1", label = "Select data", 
                   choices = list("IRIS" = "iris", "CARS" = "cars"),
                   selected = "iris")
    ),
    
    dashboardBody(
      tabItems(
        tabItem(tabName = "db",
                h4("Show selected dataset"),
                fluidRow(DT::dataTableOutput('tbl2'))
        )
      )
    )
  )  
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  output$value <- renderPrint({ input$rb1 })
  
  data <- reactive({
    switch(input$rb1,
           "iris" = iris,
           cars)
  })
  
  action <- DT::dataTableAjax(session, cars)
  widget <- DT::datatable(cars, 
                      class = 'display cell-border compact',
                      filter = 'top',
                      options = list(ajax = list(url = action))
  )
  
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(data())
  })
  
}

shinyApp(ui, server)