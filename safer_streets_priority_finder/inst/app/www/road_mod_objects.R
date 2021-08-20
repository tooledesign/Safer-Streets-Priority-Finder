#######################################################################################
delete_analysis_results_rd <- mod_delete_model_ui("delete_model_ui_3", confirm_button = ns("delete_data_rd"), cancel_button = ns("do_not_delete_data_rd"), text="To upload new roads data, we need to delete your crash data, Sliding Windows Analysis, and the Safer Streets Model where they exist. By uploading new data, you will delete those results. Do you want to proceed with deleting your results and upload new data? Remember, you can always create a new study if you'd like to analyze multiple places.")

roads_instructions_modal <- modalDialog(
  title = "Uploading Roads Data; Instructions",
  easyClose = TRUE,
  next_label = NULL,
  tagList(
    tags$div( 
      p("Please select the unique id, functional class and road name attributes from your roads data. In addition, you'll need to assign the functional class values in your data to the functional class categories listed. This step is necessary due to the variation in naming conventions used by jurisdictions."),
      p("Note that dual carriageways -- divided roads that are represented by two linestrings in the data -- may result in crash risk being underestimated on divided roads. This is because crashes are split between the two lines representing the street -- versus an undivided road where the street is represented by a single line. Users should be wary of interpreting results for dual centerlines.")
    )
  ),
  footer = tagList(
    actionButton(ns("ok"), "OK")
  )
)
properly_formatted_roads_instructions <- modalDialog(
  title = "Properly Formatted Road Data",
  easyClose = TRUE,
  next_label = NULL,
  size="l",
  tagList(
    fluidRow(
      col_12(
        p("The data should be in line-type shapefile format. While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, .prj."),
        
        p("The shapefile must have the following attributes (attribute names can be different from those given below):"), ##DUPLICATE WITHIN APP##
        tags$ul(
          tags$li(HTML("<strong>Unique ID</strong> - a unique integer value")),
          tags$li(HTML("<strong>Road Name</strong> - a text value")),
          tags$li(HTML("<strong>Functional Classification</strong> - must be assigned to 'Expressway' 'Major Arterial', 'Minor Arterial', 'Major Collector', 'Minor Collector', 'Local Road'. Any limited-access highways should be considered Expressways. If you'd like to omit Expressways, you should assign them to the 'Omit from Analysis' category.")),
        ),
      )),
    fluidRow(
      col_12(
        p("Properly Formatted Road Data"),
        tags$div(img(src="https://user-images.githubusercontent.com/17485460/116926072-d780e380-ac0e-11eb-9465-4043d312676a.png", alt="properly formatted road data"), align = "center", width="80vw"),
      )
    )
  ),
  footer = tagList(
    actionButton(ns("ok"), "OK")
  )
)




# render data table with reactive object 
# the table reacts to the files the users uploads, and subsequently updates downstream objects 
# Observe differences in the data$fun_class_reactive reactive object and repopulate the rendered datatable. This caused me a lot of headaches, here are a few gotchas 
# 1. new/old datatable need to have the same number of columns 
# 2. data needs to be managed server side (server = true in DT::renderDataTable)
# and of course, creating reactive and rendering tables should occur outside observers 
output$fun_class_map_table <- DT::renderDataTable(
  data$fun_class_reactive, escape = FALSE, selection = 'none', server = TRUE,
  options = list(dom = 't', paging = TRUE, ordering = FALSE, stateSave = FALSE),
  callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
)

# creates a proxy datatable that can be updated with new data as the reactive datatable reacts to user inputs 
proxy_table <- DT::dataTableProxy('fun_class_map_table')



# create reactive datatable to feed into the DT::renderDataTable below 
fun_class_reactive <- reactive({
  
  if (input$func_class_variable != 'Please upload your roads data' && !is.null(data$the_roads)){
    tryCatch({
      table_r <- as.data.frame(unique(na.omit(as.data.frame(data$the_roads)[input$func_class_variable]))[, 1])
      data$fun_class_count <- nrow(table_r)
      if (data$fun_class_count < 20) {
        for (i in 1:nrow(table_r)) {
          table_r[['Standard Functional Class Value']][i] <- as.character(selectInput(ns(paste0("sel", i)), "", choices = data$fun_class, width = "200px"))
        }
        names(table_r)[1] <-  "Your Dataset's Functional Class Value"
        table_r
      } else { data$table_empty } 
    }, error = function(cond){data$table_empty}
    )} else { data$table_empty }
})

# map to reactiveValues 
observe({
  data$fun_class_reactive <- fun_class_reactive()
})