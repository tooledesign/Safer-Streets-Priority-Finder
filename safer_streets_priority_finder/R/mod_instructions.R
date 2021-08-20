#' instructions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_instructions_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
	
      h2("Instructions"),
	  
      
	  
      p(
		tags$a(href="#inst-1", "1. Tool Workflow", style = "color: #007bff;"),
		br(),
		tags$a(href="#inst-2", "2. Data Requirements", style = "color: #007bff;"),
		br(),
		tags$a(href="#inst-3", "3. Data Prep Instructions", style = "color: #007bff;"),
		br(),
		HTML('&emsp;'), tags$a(href="#inst-4", "3.1 Study Area", style = "color: #007bff;"),
		br(),
		HTML('&emsp;'), tags$a(href="#inst-5", "3.2 Roads", style = "color: #007bff;"),
		br(),
		HTML('&emsp;'), tags$a(href="#inst-6", "3.3 Crashes", style = "color: #007bff;"),
		br(),
		HTML('&emsp;&emsp;'), tags$a(href="#inst-7", "3.3.1 Crash Cost Values", style = "color: #007bff;"),
	  ),

h2("1. Tool Workflow", id="inst-1"),



p("To use the SSPF, you’ll either need to prepare and upload your own local data (recommended for best results) or use the default datasets available in the tool. For users uploading their own data, the data will need to be prepared to match the specific format requirements, which are outlined below. "),
p("Once your data is properly formatted, or if you are using default data, you can proceed through the tool's key steps:"),
tags$ul(
  tags$li("Load Data Tab. Upload local data or select default data and verify accuracy of data inputs. At any point, you can check the 'Confirm Input Data' tab of this page to verify which datasets have already been uploaded."),
  tags$li("Analysis Tab. Develop one or both of the available analysis. 1) Build the Sliding Windows Analysis (the precursor to a standard High Injury Network) for any modes you have uploaded. 2) Begin the process to build the Safer Streets Model for bicyclists and pedestrians, though it takes a half day or more to build the model. You’ll need to navigate back to this tab once the model is finished to see the results."),
  tags$li("Dashboard Tab. Explore descriptive statistics and visualizations of the input data and analysis results. You can also download a PDF report, as well as the data outputs on this tab."),
  tags$li("Map Visualization Tab. Map the input data and analysis results. You can also upload and visualize your own contextual datasets here." )
),



h2("2. Data Requirements", id="inst-2"),

p("Each study requires three user-provided or default data inputs - study area, crash data, and road data. If you do not have anyone or more of these datasets, you have the option to use the tool’s default data. The default data draws from nationally available sources, as follows:"),
tags$ul(
  tags$li("Study Area:", HTML("<a href=\"https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html\" target=\"_blank\" style=\"color: #007bff;\">US Census County Boundaries</a>")),
  tags$li("Crashes:", HTML("<a href=\"https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars\" target=\"_blank\" style=\"color: #007bff;\">NHTSA’s Fatality Analysis Reporting System (FARS) 2015-2019 data</a>")),
  tags$li("Roads:", HTML("<a href=\"https://www.openstreetmap.org/\" target=\"_blank\" style=\"color: #007bff;\">Open Street Map (OSM)</a>"))
),

p("When determining whether to upload your own data or use the default data, please consider the following:"),
tags$ul(
  tags$li("Study Area: Using your own study area versus the default does not have a strong bearing on the analysis results. Using your own study area may be helpful if you would like to look at an area smaller than a county. Note that study areas larger than one county cannot be processed at this time. If you are interested in a larger study area, you may create additional scenarios in the tool to run the analysis for each county separately."),
  tags$li("Crashes: Using FARS data may produce unexpected or unhelpful results. If you have access to crash data containing more than just fatal crashes, we recommend you use it. Because the default FARS data only includes fatalities, it is not ideal for the purposes of the analyses within the SSPF. If you have access to any kind of geocoded crash data, we strongly recommend using this data, even if it requires some time on the front end to clean this data up to meet the data requirements listed below."),
  tags$li("Roads: A downside to using the default OSM data is that it will not have your local roadway network IDs, so it may be more difficult to integrate outputs with your local roadway network dataset. When you upload your own roadway network data, you will be able to join the analysis results back to your road network later on. You should also consider whether you are fine with the OSM data’s allocation of functional classes on your road network. WARNING: We make no guarantees about the reliability of OSM data.")
),


p("Note: The analyses may take a long time to run depending on the size of the input data. In addition, if multiple users are using the SSPF concurrently, analyses will take longer to process."),
p("If you plan to upload your own data, please follow the instructions below to prepare your data. If you plan to use the default data, proceed to the Load Data tab and follow the workflow there."),

h2("3. Data Preparation Instructions", id="inst-3"),

h3("3.1 Study Area", id="inst-4"),

p("The data must be in a polygon-type shapefile format. While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, and .prj."),


h3("3.2 Road Data", id="inst-5"),


p("The data should be in line-type shapefile format. While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, .prj."),

####mapping roads data

p("As you follow the tool's upload steps, you'll need to indicate which columns in your data store the unique id, functional class and road name attributes. In addition, you'll need to assign the functional class values in your data to the functional class categories listed. This step is necessary due to the variation in naming conventions used by jurisdictions."),


p("The shapefile must have the following attributes (attribute names can be different from those given below):"), 
tags$ul(
	tags$li("Unique ID - a unique integer value"),
	tags$li("Road Name - a text value"),
	tags$li("Functional Classification - must be assigned to 'Expressway' 'Major Arterial', 'Minor Arterial', 'Major Collector', 'Minor Collector', 'Local Road'. Any limited-access highways should be considered Expressways. If you'd like to omit Expressways, you should assign them to the 'Omit from Analysis' category."),
),

p("Properly Formatted Road Data"), 
p(tags$div(img(src="https://user-images.githubusercontent.com/17485460/116926072-d780e380-ac0e-11eb-9465-4043d312676a.png", alt="properly formatted road data"), align = "left")),

p("Note that dual carriageways -- divided roads that are represented by two lines in the data -- may have lower than expected results when modeled. This is because crashes are split between the two lines representing the street -- versus an undivided road where the street is represented by a single line. Users should be wary of interpreting results for dual centerlines."),
p("The tool will guide you through the process of addressing any NULL functional class values, which will need to be omitted. Alternatively, if you have local knowledge that most or all of the null or unknown functional class values are usually related to a specific type of street (e.g., undesignated residential streets), you may assign them to the functional class that best fits local conditions."),

h3("3.3 Crashes", id="inst-6"),

p("The data must be in a point-type shapefile format. While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, and .prj."),

p("The shapefile must have each of the following attributes and characteristics 
(attribute names can be different from those given below):"), 
tags$ul(
  tags$li("Report ID - A unique ID for the crash to ensure one location record per crash. If your dataset has multiple records per crash, you may be working with a 'person', 'unit', or 'vehicle' table. Please restructure your data or ask your crash data provider for help getting a dataset with one row per crash."),
  tags$li("Crash year - four digit integer format. At this time, the tool requires five years of crash data to be uploaded."),
  tags$li("Severity - must be mapped to the KABCO scale as follows: 'Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury  (C)', 'Property Damage Only (O)'. Severity should represent the most severely injured person or worst outcome associated with the crash." ),
  tags$li("If your data has crashes with null or unknown severity values: for pedestrian and bicycle crashes, if nulls are a small percentage of total crashes, consider assigning these to Non-Incapacitating Injury (B). For motor vehicle crashes missing severities (especially if a large percentage are missing), consider assigning these to Property Damage Only (PDO).  Higher severity crashes (especially fatal crashes) are less likely to have missing or incomplete data. At the same time, missing or incomplete data are still pretty common, and the crashes themselves are statistically rare, so making an assumption about severity lets us keep missing or incomplete records in the dataset."),
  tags$li("Crash mode - the mode in your data must be assigned to bicycle, pedestrian, or other mode. Identification of mode must occur in one column. E.g., if you have separate columns to indicate bicyclist involved or pedestrian involved, you will need to restructure your data to use a single mode column before using it with the tool. For crashes involving multiple modes, it is most common to assign the entire crash either the mode of the most severely injured person, or the mode of the most vulnerable road user (typically in this order: pedestrian, bicycle, motorcycle, motor vehicle). Note that the 'other mode' value in the tool can be used flexibly for your analysis. For example, if you want to look only at motorcyclists and not other motor vehicles, and your mode column includes this level of detail, you can assign motorcyclists to the 'other' value within the tool and omit motor vehicle crashes from the analysis."),
),

p("Properly Formatted Crash Data"), 
p(tags$div(tags$img(src="https://user-images.githubusercontent.com/17485460/116925202-bc61a400-ac0d-11eb-98d2-9f6f94a61efe.png", alt="properly formatted crash data"), align = "left")),

p("Improperly Formatted Crash Data"),
p(tags$div(tags$img(src="https://user-images.githubusercontent.com/17485460/116925026-81f80700-ac0d-11eb-90a2-ed14b44248ad.png", alt="improprely formated crash data"), align = "left", width = '80%')),

p("The tool will guide you through the process of addressing any NULL values in your crash data, which will need to be omitted in most cases (except as outlined above for nulls in crash severity values."),

h4("3.3.1 Crash Cost Values", id="inst-7"), 

p("For user uploaded crash data, users can rely on default crash costs or customize crash costs. The default values reflect national 2020 crash costs that were developed for this tool. Information on how the default crash costs were developed is included in the Overview/Methodology tab."),

p("Currently, the tool does not build in any functionality to localize the default costs to states. When localized data is needed, it may be preferable for the user to customize crash costs based on crash cost information published by their State DOT."),

p("If you choose to customize crash costs, you'll need to specify a discount rate to reflect today’s value of costs projected over a time horizon of five years. The default discount rate is set at 3%. You can also adjust the discount rate applied to the default crash costs.")

  )
  )
}
    
#' instructions Server Functions
#'
#' @noRd 
mod_instructions_server <- function(input, output, session){
    ns <- session$ns

}
    
## To be copied in the UI
# mod_instructions_ui("instructions_ui_1")
    
## To be copied in the server
# mod_instructions_server("instructions_ui_1")
