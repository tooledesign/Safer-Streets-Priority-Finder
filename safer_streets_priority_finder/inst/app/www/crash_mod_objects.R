
delete_analysis_results_cr <- mod_delete_model_ui("delete_model_ui_2", confirm_button = ns("delete_data_cr"), cancel_button = ns("do_not_delete_data_cr"), text="To upload new crash data, we need to delete your Sliding Windows Analysis and the Safer Streets Model where they exist. By uploading new data, you will delete those results. Do you want to proceed with deleting your results and upload new data? Remember, you can always create a new study if you'd like to analyze multiple places.")


crashes_instructions_modal <- modalDialog(
  title = "Uploading Crash Data; Instructions",
  easyClose = TRUE,
  next_label = NULL,
  tagList(
    tags$div( 
      p("You'll need to indicate which columns in your data store the unique id, crash year, severity, and crash mode. This step is necessary due to the variation in naming conventions used by jurisdictions."),
    )
  ),
  footer = tagList(
    actionButton(ns("crashes_instructions_close"), "OK")
  )
)


properly_formatted_crashes_instructions <- modalDialog(
  title = "Properly Formatted Crash Data",
  easyClose = TRUE,
  next_label = NULL,
  size="l",
  tagList(
    fluidRow(
      col_12(
        p("The data should be in line-type shapefile format. While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, .prj."),
        
        p("The shapefile must have each of the following attributes and characteristics (attribute names can be different from those given below):"), ##DUPLICATE WITHIN APP##
        tags$ul(
          tags$li(HTML("<strong>Report ID</strong> - A unique ID for the crash to ensure one location record per crash. If your dataset has multiple records per crash, you may be working with a 'person', 'unit', or 'vehicle' table. Please restructure your data or ask your crash data provider for help getting a dataset with one row per crash.")),
          tags$li(HTML("<strong>Crash year</strong> - four digit integer format. At this time, the tool requires five years of crash data to be uploaded.")),
          tags$li(HTML("<strong>Severity</strong> - must be mapped to the KABCO scale as follows: 'Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury  (C)', 'Property Damage Only (O)'. Severity should represent the most severely injured person or worst outcome associated with the crash." )),
          tags$li(HTML("If your data has crashes with null or unknown severity values: for pedestrian and bicycle crashes, if nulls are a small percentage of total crashes, consider assigning these to Non-Incapacitating Injury (B). For motor vehicle crashes missing severities (especially if a large percentage are missing), consider assigning these to Property Damage Only (PDO).  Higher severity crashes (especially fatal crashes) are less likely to have missing or incomplete data. At the same time, missing or incomplete data are still pretty common, and the crashes themselves are statistically rare, so making an assumption about severity lets us keep missing or incomplete records in the dataset.")),
          tags$li(HTML("<strong>Crash mode</strong> - the mode in your data must be assigned to bicycle, pedestrian, or other mode. Identification of mode must occur in one column. E.g., if you have separate columns to indicate bicyclist involved or pedestrian involved, you will need to restructure your data to use a single mode column before using it with the tool. For crashes involving multiple modes, it is most common to assign the entire crash either the mode of the most severely injured person, or the mode of the most vulnerable road user (typically in this order: pedestrian, bicycle, motorcycle, motor vehicle). Note that the 'other mode' value in the tool can be used flexibly for your analysis. For example, if you want to look only at motorcyclists and not other motor vehicles, and your mode column includes this level of detail, you can assign motorcyclists to the 'other' value and omit motor vehicle crashes from the analysis.")),
        ))),
    fluidRow(
      col_12(
        p("Properly Formatted Crash Data"), ##DUPLICATE WITHIN APP##
        tags$div(tags$img(src="https://user-images.githubusercontent.com/17485460/116925202-bc61a400-ac0d-11eb-98d2-9f6f94a61efe.png", alt="properly formatted crash data"), align = "center"),
      )
    )
  ),
  footer = tagList(
    actionButton(ns("modal_crash_pro_format_close"), "OK")
  )
)

crash_costs_more_information <- modalDialog(
  title = "Crash Cost Values",
  easyClose = TRUE,
  next_label = NULL,
  size="s",
  tagList(
    fluidRow(
      col_12(
        p("For user uploaded crash data, users can rely on default crash costs or customize crash costs. The default values reflect national 2020 crash costs that were developed for this tool with a discount rate applied. Information on how the default crash costs were developed is included in the Overview/Methodology tab."),
        p("Currently, the tool does not build in any functionality to localize the default costs to states. When localized data is needed, it may be preferable for the user to customize crash costs based on crash cost information published by their State DOT."),
        p("You can enter custom crash costs and/or custom discount rates by clicking on the 'Update Default Crash Costs' button. Discount rates are used to reflect today’s value of costs projected over a time horizon of five years.")
      ))
  ),
  footer = tagList(
    actionButton(ns("close_cost_info"), "OK")
  )
)


observeEvent(input$modal_crash_instructions, {
  showModal(crashes_instructions_modal)
})
observeEvent(input$crashes_instructions_close, {
  removeModal()
})
observeEvent(input$modal_crash_pro_format, {
  showModal(properly_formatted_crashes_instructions)
})
observeEvent(input$modal_crash_pro_format_close, {
  removeModal()
})
observeEvent(input$modal_crash_costs_format, {
  showModal(crash_costs_more_information)
})
observeEvent(input$close_cost_info, {
  removeModal()
})


########################################################################################
###### Setup Reactives  
########################################################################################

# the following creates a reactive dataframe of severity values for mapping local values to standardized values 
# 1) Ensures severity variables was selected by user in browser
# 2) Checks to ensure their aren't more than 20 unique variables
sev_reactive <- reactive({
  if (input$crash_sev_variable != 'Please upload your crash data' && !is.null(data$crashes)){
    tryCatch({
      table_c <- as.data.frame(unique(na.omit(as.data.frame(data$crashes)[input$crash_sev_variable]))[, 1])
      data$sev_id_count <- nrow(table_c)
      
      if (data$sev_id_count < 20) {
        for (i in 1:nrow(table_c)) {
          table_c[['Standard Mode Values']][i] <- as.character(selectInput(ns(paste0("sev", i)), "", choices = data$sev_levels, width = "200px"))
        }
        names(table_c)[1] <-  "Your Dataset's Severity Value"
        table_c
      } else { data$table_empty }
    }, error = function(cond){ data$table_empty }
    )} else { data$table_empty }
})

# make reactive values 
observe({
  data$sev_reactive <- sev_reactive()
})

# the following creates a reactive dataframe of mode values for mapping local values to standardized values 
# 1) Ensures severity variables was selected by user in browser
# 2) Checks to ensure their aren't more than 20 unique variables
mode_reactive <- reactive({
  if (input$mode_variable != 'Please upload your crash data' && !is.null(data$crashes)){
    tryCatch({
      table_m <- as.data.frame(unique(na.omit(as.data.frame(data$crashes)[input$mode_variable]))[, 1])
      data$mode_count <- nrow(table_m)
      
      if (data$mode_count < 20) {
        for (i in 1:nrow(table_m)) {
          table_m[['Standard Mode Values']][i] <- as.character(selectInput(ns(paste0("mode", i)), "", choices = data$mode_levels, width = "200px"))
        }
        names(table_m)[1] <-  "Your Dataset's Mode Value"
        table_m
      } else { data$table_empty }
    }, error = function(cond){ data$table_empty }
    )} else { data$table_empty }
})

# make reactive values 
observe({
  data$mode_reactive <- mode_reactive()
})

# rinse and repeat, the following creates one of two options 
# 1) If the user wants to change the default crash costs, a table with numeric inputs is created, tryCatch used to catch any problems  
# 2) A default table of KABCO costs is supplied  
kabco_costs <- reactive({
  if (input$update_defaults > 0){
    tryCatch({
      unadjusted_costs <- c(13129982.87, 755040.43, 233272.52, 139765.81, 12862.87)
      table_k <- data$default_kabco[1]
      for (i in 1:nrow(table_k)) {
        table_k[['Cost']][i] <- as.character(numericInput(ns(paste0("kab", i)), label= "", value = as.numeric(unadjusted_costs[i]), width = "200px"))
      }
      names(table_k)[1] <-  "KABCO Value"
      table_k
    }, error = function(cond){
      table_k <- data$default_kabco
      table_k
    })
  } else {
    table_k <- data$default_kabco
    table_k
  }
})

# make reactive values 
observe({
  kabco_costs()
  data$kabco_costs <- kabco_costs()
})

# render severity data table with reactive object 
output$cr_severity_map_table <- DT::renderDataTable(
  data$sev_reactive, escape = FALSE, selection = 'none', server = TRUE,
  options = list(dom = 't', paging = FALSE, ordering = FALSE, stateSave = FALSE),
  callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
)

# creates proxy table  
proxy_table_cr <- DT::dataTableProxy('cr_severity_map_table')

# render mode data table with reactive object
output$cr_mode_map_table <- DT::renderDataTable(
  data$mode_reactive, escape = FALSE, selection = 'none', server = TRUE,
  options = list(dom = 't', paging = FALSE, ordering = FALSE, stateSave = FALSE),
  callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
)

# creates proxy table  
proxy_table_cr_mode <- DT::dataTableProxy('cr_mode_map_table')

# render crash cost data table with reactive object 
output$cr_cost_map_table <- DT::renderDataTable(
  data$kabco_costs, escape = FALSE, selection = 'none', server = TRUE,
  options = list(dom = 't', paging = FALSE, ordering = FALSE, stateSave = FALSE),
  callback = JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-container');
          });
          Shiny.unbindAll(table.table().node());
          Shiny.bindAll(table.table().node());")
)

# creates proxy table  
proxy_table_cr_costs <- DT::dataTableProxy('cr_cost_map_table')

