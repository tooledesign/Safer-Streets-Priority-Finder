#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
  fluidPage(

  h2("Overview and Methodology"),

      # h3("Table of Contents", id="overview-toc"),
  p(
	  tags$a(href="#overview-1","Overview", style="text-indent: 25px; color: #007bff;"),
	  tags$br(),
	  tags$a(href="#overview-2", "Methodology", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-3", "Sliding Windows Analysis", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-4", "Safer Streets Model", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-5", "Modeling Framework", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-6", "Crash Cost Values", style = "color: #007bff;"),
	  br(),
	  tags$a(href="#overview-7", "Data Sources", style = "color: #007bff;"),
	  br(),
	  tags$a(href="#overview-8", "Tool Infrastructure", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-9", "R", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-10", "Amazon EC2", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="overview-11", "Docker", style = "color: #007bff;"),
	  br(),
	  HTML('&emsp;'), tags$a(href="#overview-12", "PostgreSQL Relational Database", style = "color: #007bff;")),


  h1("Overview", id="overview-1"),
  #tags$a(href="#overview-toc", "Return to top"),
  p("The Safer Streets Priority Finder (SSPF) is a free, interactive, open-source tool available in the United States that is intended to help transportation practitioners identify a roadway network that is similar to a High Injury Network for bicyclists and pedestrians. The final Safer Streets Model goes further than a typical High Injury Network by not only taking into consideration areas where a disproportionate share of fatal and serious injury crashes have already occurred, but also areas that have other kinds of factors present that may indicate potential risk of crashes."),
    p("The SSPF is an expansion of a prior effort-the Pedestrian Fatality Risk Pilot beta tool, developed by the USDOT (i.e., Pilot Tool). The Pilot Tool is based on a tract-level statistical model of pedestrian fatalities for the entire United States. As described in ",

      HTML("<a href=\"https://www.transportation.gov/sites/dot.gov/files/docs/mission/office-policy/transportation-policy/328686/effects-roadway-and-built-environment-characteristics-pedestrian-fatality-risk-mansfield-et-al.pdf\" target=\"_blank\" style=\"color: #007bff;\">Mansfield, et al. (2018)</a>"),
      ", this model takes into account various factors to estimate risk for pedestrian fatalities, including Vehicle Miles Traveled density by roadway functional classification, intersection density, employment density, residential population density, activity mix index, and sociodemographics."),

    p("Validation of the Pilot Tool was conducted on data from the City of New Orleans to assess how closely correlated the model outputs are with other pedestrian crash severities beyond fatalities and other modes beyond pedestrians. 
       The model was found to perform well enough for other outcomes and was thus applied for bicyclist and pedestrian crashes of all severities in the modeling framework described below."),

  p("The SSPF incorporates the Pilot Tool along with crash and roadway network data into a
     Bayesian statistical framework called the Safer Streets Model which assigns estimated risk values for bicyclists and pedestrians to road
     segments for different crash severities. These values are converted to crash cost estimates based on
     costs associated with each crash severity. The model reflects conditions over a five-year time period. 
     The diagram below shows how the different data and model components work together to estimate risk."),
  tags$div(tags$img(src="https://tooledesign.egnyte.com/dd/C4tZnM7Kjx/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="Methodology Flow Chart", width = '100%'), style='max-width:816px;', align = "left"),

p("The SSPF also develops a Sliding Windows Analysis, which provides the information used in a typical High Injury Network (based on historical crash data alone). 
   The Sliding Windows Analysis does NOT use data from the Pilot Tool or statistical modeling to estimate risk in places where crashes haven't happened yet."),

p("For more detailed information on model development and calibration beyond what is included below please see the technical report documenting development of the SSPF: (link forthcoming)."),
p("Please note that the Safer Streets Model is still in Beta phase. We have documented known limitations."),

h1("Methodology", id="overview-2"),
#tags$a(href="#overview-toc", "Return to top"),
h2("Sliding Windows Analysis", id="overview-3"),

p("This analysis creates crash density estimates along street corridors throughout the study area, weighted by crash severity, which enables the highest crash-density sections to be identified for each mode individually. 
   Crash density corridors are identified by applying a 1/2-mile moving window aggregation to the street network in the study area. The 1/2-mile moving windows are created to form corridors using the roadway street name. 
   In this approach, a virtual 'window' is moved along each street in 1/10 mile increments, counting the number of crashes by severity and mode that occurred within each successive 1/2-mile segment."),

tags$div(img(src="https://tooledesign.egnyte.com/dd/QMwVK9HLRS/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="sliding windows visual", width = '100%'), style='max-width:855px;'),
p("The Sliding Windows score weights the most severe crashes more heavily than lower severity crashes. The Sliding Windows score is calculated by multiplying the number of Fatal (K) and Incapacitating Injury (A) crashes by 3, and multiplying the number of Non-Incapacitating Injury (B) crashes by 1. Once the weights are established and applied to the crashes, the total number of crashes are aggregated along a corridor while incorporating the crash severity weighting. Possible Injury
(C) crashes and Property Damage Only (O) crashes are not reflected. The output from the Sliding Windows analysis can be used as a standalone output separate from the Safe Streets Model output described below. This output can be used as the basis of a typical High Injury Network that reflects crash history (once a threshold for the highest tier of streets is set).
  For more information about High Injury Networks, see the Vision Zero Network's",
  HTML("<a href=\"https://visionzeronetwork.org/hin-for-the-win/\" target=\"_blank\" style=\"color: #007bff;\">HIN for the Win</a>.")),


h2("Safer Streets Model (Beta)", id="overview-4"),
#tags$a(href="#overview-toc", "Return to top"),

p("The Safer Streets Model brings the segmented road network window segments into a Bayesian statistical framework to estimate crash risk throughout the system. This framework allows us to incorporate external information about how many crashes we might expect to see (called a Bayesian prior), alongside the observed crash history."),

p("The model estimates crash risk rates per mile for each road segment and each crash mode (pedestrian and bicyclist only at this time) and severity . These values are then converted to crash cost estimates based on the costs assigned to each crash severity by the user or from the SSPF default costs."),

p("Note: The Safer Streets Model has been validated on several cities and outperforms traditional Sliding Window analysis, though it remains in Beta phase at this time. No type of model or analysis is able to perfectly predict crashes or risk. Please see the FAQ section on the model for additional details on known limitations."),

h4("Modeling Framework", id="overview-5"),
#tags$a(href="#overview-toc", "Return to top"),

p("A Bayesian model is used to estimate risk, informed by two key pieces of information (Bayesian priors) in addition to user-submitted crash data:"),
tags$ol(
  tags$li("Estimate of the number of crashes within a Census tract, from the Pilot Tool model."),

tags$li("National average rate of fatal crashes per mile on a roadway based on its functional class, developed from national fatal crash data (Fatal Accident Reporting System, or FARS). Functional class is used as a proxy for roadway design elements that are associated with both the risk of crash occurring and the risk of a crash's outcome being severe (e.g., motor vehicle travel speeds, number of lanes, motor vehicle AADT, etc.) )")
),

p("For each tract in the study area, the tool combines the estimated number of crashes with the actual, observed number of crashes in the tract. The model uses a Gamma-Poisson distribution based on this combined input to come up with an updated estimate of crashes for every tract."),
p("For each window segment in the study area, the tool combines the national average crash rate with the observed number of crashes on each window segment to come up with an updated estimate of crash risk rate per mile."),
p("Given the updated estimate of crashes in each tract, and the updated risk rate per mile for each window segment, the model then uses a Beta-Binomial distribution to allocate estimated crashes to window segments within each tract."),
p("Observed number of crashes are the primary input, but these two additional inputs (priors) help us understand possible risk in areas where crashes haven't been observed yet. In other words, in areas with a lot of crashes, the model results may look very similar to the simple sliding window analysis. But in areas without a lot of crashes, the model results may identify some streets that could benefit from safety improvements even if crashes haven't happened there yet."),
p("Model validation results are documented in [link forthcoming]. In general, the model tends to perform as well or better than a simple sliding window analysis in most conditions. No type of model or analysis is able to perfectly predict crashes or risk. Please see the FAQ section on the model for additional details on known limitations."),

h2("Crash Cost Values", id="overview-6"),
#tags$a(href="#overview-toc", "Return to top"),
p("Standard crash cost rates are built into the tool as a default. 
  The default crash costs include comprehensive crash costs in order to provide a full picture of the societal costs resulting from crashes. 
  These include both tangible and intangible costs. The default costs are based on NHTSA's 2015 Maximum Abbreviated Injury Scale (MAIS) person-injury unit costs, which were updated to crash unit costs expressed in 2020 values using NHTSA's National Automotive Sampling System (NASS) General Estimates System (GES) and 
  Crash Report Sampling System (CRSS) data as well as indices published by the Bureau of Labor Statistics, including Consumer Price Index (CPI) and Median Usual Weekly Earnings (MUWE). 
  This process follows the guidance set forth in Chapter 6 of ",
  HTML("<a href=\"https://safety.fhwa.dot.gov/hsip/docs/fhwasa17071.pdf\" target=\"_blank\" style=\"color: #007bff;\">FHWA's Crash Costs for Highway Safety Analysis (2018),</a>"),
  "however crash costs were not localized to states and are available only at the national level."), 

p("The default crash costs reflect a discount rate of 3% to reflect today's value of costs projected over a five year time horizon, which users can adjust. The table below shows the default crash cost values in the tool, as well as the original values derived based on the process above, without a discount rate applied."),

tags$div(img(src="https://tooledesign.egnyte.com/dd/ilpAneHsKZ/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="sliding windows visual", width = '100%'), style='max-width:391px;'),

p("Alternatively, users who have access to local crash cost rates or data (e.g., published by their state department of transportation) can customize the crash cost values according to each KABCO severity level. Users may also adjust the discount rate applied to user-input crash cost values so that costs can be projected over five year time periods."),



p("For those relying on default FARS crash data, crash costs cannot be customized."),

p("The crash cost rates selected by the user in the load data step are applied to the mode and severity crash estimates from the model to create a weighted aggregate score for each mode. 
   For example, the pedestrian crash risk cost score is calculated by multiplying the model output for fatal pedestrian crashes on each window by the cost rate for fatal crashes, repeating this for each additional severity level's modeled output and cost rate, and summing the values into a single pedestrian score. 
   These cost rates calculated for each segment represent the annual expected societal cost per mile due to crashes from the model, given the trends do not change. 
   For further investigation beyond what is offered within the tool's interface, these costs are also presented as an estimated cost per window segment per year, by multiplying the cost rate per mile by the segment length. 
   For example, a user may wish to sum this estimated cost per window segment across multiple segments in a corridor or a study area."), 

p("The outputs are calibrated so that the total crash cost in the study area for a given mode is the same for observed and model estimated crashes. While the overall crash costs for a given mode will be consistent with that of observed crashes, individual severity cost totals breakdown will not be the same. In general, the model and subsequent calibration process may overestimate high severity crashes (which tend to be less frequent but much more costly) relative to low severity crashes."),


h1("Data Sources", id="overview-7"),
#tags$a(href="#overview-toc", "Return to top"),

p("The tool's primary data sources are listed below:"),

tags$ul(
	tags$li(
      	  HTML("<a href=\"https://www.transportation.gov/sites/dot.gov/files/docs/mission/office-policy/transportation-policy/328686/effects-roadway-and-built-environment-characteristics-pedestrian-fatality-risk-mansfield-et-al.pdf\" target=\"_blank\" style=\"color: #007bff;\">Ted Mansfield et. al's Pedestrian Fatality Risk Model</a>"),
      			"(i.e. Pilot Tool model) is used as an input to the Bayesian statistical modeling framework. The data sources used in that model are documented",
      	  HTML("<a href=\"https://github.com/USDOT-OST/Pedestrian-Fatality-Risk-Project\" target=\"_blank\" style=\"color: #007bff;\">here.</a>"),
      			),
	tags$li("Crash data: User input if available, if not, the user can default to nationally available FARS data. FARS data only includes information on fatal crashes, which will limit the analysis to only include fatal crashes. Since fatal pedestrian and bicyclist crashes are statistically rare, this may result in some outputs - especially the crash density output - looking incomplete or strange. We recommend using local crash data rather than FARS data if at all possible."),
  tags$li("Road network data: User input if available, if not, the user can default to nationally available data from Open Street Map (OSM)."),
  tags$li("National average rate of fatal crashes per mile by mode and roadway functional class, calculated from FARS and",
          HTML("<a href=\"https://www.fhwa.dot.gov/policyinformation/statistics/2016/pdf/hm220.pdf\" target=\"_blank\" style=\"color: #007bff;\">USDOT's Highway Statistics estimating public roadway miles categorized by functional class.</a>"),
)),


h1("Tool Infrastructure", id="overview-8"),
#tags$a(href="#overview-toc", "Return to top"),

h2("R", id="overview-9"),
#tags$a(href="#overview-toc", "Return to top"),

tags$ul(
tags$li("R was used for programming the tool logic and process flow."),
tags$li("R Shiny was used to develop the interactive application."),
tags$li("The RStan package was used for development of the Bayesian statistical model.")
),


h2("Amazon EC2", id="overview-10"),
#tags$a(href="#overview-toc", "Return to top"),

p("The public facing tool and risk model processor live on a cloud server hosted on Amazon Web Services (AWS)."),

h2("Docker", id="overview-11"),
#tags$a(href="#overview-toc", "Return to top"),

p("The public facing tool and the risk model processor are deployed from Docker containers within AWS EC2 instances."),

h2("PostgreSQL Relational Database", id="overview-12"),
#tags$a(href="#overview-toc", "Return to top"),

p("All the tool data resides on a PostgreSQL Relational Database hosted on an AWS Relational Database Service (RDS) instance.")
  ))
}  

#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_overview_ui("overview_ui_1")

## To be copied in the server
# mod_overview_server("overview_ui_1")
