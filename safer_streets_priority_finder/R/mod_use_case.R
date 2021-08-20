#' use case UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_use_case_ui <- function(id){
  ns <- NS(id)
  tagList(
  fluidPage(

h3("Use Case", id="use-case-toc"),
p(
tags$a(href="#uc-intro", "1. Introduction", style = "color: #007bff;"),
br(),
tags$a(href="#uc-example", "2. Use Case Examples", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#uc-example-nola", "2.1 City of New Orleans - Pedestrian Safety Action Plan Implementation and Grant Applications", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-nola-user", "2.1.1 Jurisdiction or User", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-nola-problem", "2.1.2 Problem Identification and Analytic Objectives", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-nola-data", "2.1.3 Data Sources and Preparation", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-nola-results", "2.1.4 Analysis results", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-nola-conclusion", "2.1.5 Conclusions and Applications", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#uc-example-rta", "2.2 New Orleans RTA: Transit System Rider Safety and Operator Training", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-rta-user", "2.2.1 Jurisdiction or User", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-rta-problem", "2.2.2 Problem Identification and Analytic Objectives", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-rta-data", "2.2.3 Data Sources and Preparation", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-rta-results", "2.2.4 Analysis results", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-rta-conclusion", "2.2.5 Conclusions and Applications", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#uc-example-lincoln", "2.3 Lincoln Parish: Active Transportation Network Development & Health Equity Impact Evaluation", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-lincoln-user", "2.3.1 Jurisdiction or User", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-lincoln-problem", "2.3.2 Problem Identification and Analytic Objectives", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-lincoln-data", "2.3.3 Data Sources and Preparation", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-lincoln-results", "2.3.4 Analysis results", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#uc-example-lincoln-conclusion", "2.3.5 Conclusions and Applications", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#disclaimers", "Disclaimers", style = "color: #007bff;")
),

h2("1. Introduction", id="uc-intro"),

p("This tool was developed based on identified need by the City of New Orleans and New Orleans Regional Transit Authority to better understand both the likelihood of serious crashes involving people walking and bicycling on the City of New Orleans’s road network, as well as a means by which to quantitatively rank segments (and thus, potential interventions) as well as to estimate the costs of those crashes, in the context of a robust suite of pending and proposed Complete Streets interventions. Thus, the primary users of this tool are anticipated to be municipal government agencies or departments (e.g. planning, public works, and/or transportation) engaged directly in project prioritization and implementation."),
p("The outputs of this tool may be used directly within the online application, substantially reducing the technical burden associated with parsing and visualizing complex datasets, or exported for more advanced analysis and applications. Thus, this product is intended to be valuable to a wide range of jurisdictions, from small communities with limited staff capacity and little previous experience with crash data analysis or spatial datasets, to large cities with robust transportation departments and advanced GIS capabilities."),
p("Although the primary unit of analysis for which this tool is designed is the county level (or smaller), secondary potential users of this tool include personnel at state DOTs, who might aggregate results across multiple counties and use this tool to identify priority projects on state routes, or encourage its use in the development of local applications for funding as an accessible, uniform methodology for augmenting the use of past crash history to support project need. Similarly, regional governments or entities such as MPOs, Highway Safety Coalitions, etc. may be able to apply the tool at a region-wide level (given data constraints) in support of TIP or SHSP development, and as a public engagement tool."),
p("In addition, the use of this tool is open to the general public, including non-governmental advocacy groups who previously may not have had access to or expertise working with spatial data to support safety initiatives. Even in states and jurisdictions where crash data is not yet available (see",
HTML("<a href=\"./crashdatasources\" target=\"_blank\" style=\"color: #007bff;\">here.</a>"),
"for a list of open data portals identified), the process of extracting and mapping - let alone modeling - this data is outside the technical capacity and scope of many such organizations. By keeping data requirements to a minimum (and facilitating the use of national default datasets that require no user manipulation), the ability to efficiently map crashes and visualize areas more likely to experience future incidents involving people walking and bicycling unlocks new opportunities to bolster campaigns in support of safer streets."),
p("Finally, by using open-source software and publicly sharing underlying code, this tool is also expected to serve future developers and researchers interested in expanding, adapting, or updating the data, outputs, or interface for other applications and uses. "),

h2("2. Use Case Examples", id="uc-example"),

p("This section highlights examples of how this tool may be used by a sample of the stakeholder types listed above to address outstanding safety questions or concerns in their community. Examples are drawn from local partners who have participated in the tool’s development."),

h3("2.1 City of New Orleans - Pedestrian Safety Action Plan Implementation and Grant Applications", id="uc-example-nola"),

h4("2.1.1 Jurisdiction or User:", id="uc-example-nola-user"),

p("The City of New Orleans (Mayor’s Office of Transportation + Department of Public Works)"),

h4("2.1.2 Problem Identification and Analytic Objectives:", id="uc-example-nola-problem"),

p("As the lead agency involved in this project, the City of New Orleans led the development of this tool, in order to address a well-documented pedestrian safety problem. In 2012, the City of New Orleans was designated as an FHWA pedestrian safety focus city, which led to the drafting of a pedestrian safety action plan. In 2015, this designation was updated to include a focus on bicycle safety as well. While this effort identified crash “hot spots” that focused on severe and fatal injuries, these findings have resulted in prioritized implementation for 20 intersections.  However, this effort has not led to a systemic approach to improving safety outcomes or an ability to identify and prioritize specific roadway segments and appropriate countermeasures that will have the greatest impact on human lives. Meanwhile, the City of New Orleans is currently engaged in the implementation of a rapid-build protected bikeway network, and is interested in developing evidence-based tools for project prioritization. The City of New Orleans previously conducted preliminary assessments toward the development of a High Injury Network (HIN), but found that this method failed to account for factors likely to impact future risk (particularly in the context of ongoing, rapid changes to roadway networks)."),
p("In short, the City of New Orleans sought a means to screen for traffic safety problems across the entire street network and prioritize opportunities for high impact investment.  More importantly, the City of New Orleans wanted a way to quantify the “bottom line” value of a proposed intervention versus the cost of no intervention. This approach would require linking the cost of investments to the mitigated costs of future injuries and fatalities prevented. Having numerical values for the costs associated with injury and loss of life would provide an important input for cost-benefit analyses and serve to highlight the impact of maintaining status-quo conditions. Finally, the City of New Orleans sought to better support decision-making, both for ranking and prioritizing internally-funded projects, as well as to support state and federal grant applications."),

h4("2.1.3 Data Sources and Preparation", id="uc-example-nola-data"),

p("Although Louisiana doesn’t publicly share crash data, as a government agency, the City of New Orleans has access to the geocoded DOTD-maintained crash records within its jurisdiction. The City of New Orleans’ GIS department also maintains an updated street centerlines shapefile, including functional classification data. The City of New Orleans’ boundary is also the boundary for Orleans Parish, the default unit of geography for analysis within the tool."),
p("The City of New Orleans users prepare the data by joining tables from the relational database and ensuring that the data requirements to run the tool are met. Any records with missing or invalid data are removed, checking to ensure that the data is sufficient in quantity and quality to achieve the intended analytic goals (for example, that a relatively small number of records are excluded for missing variables)."),

h4("2.1.4 Analysis results", id="uc-example-nola-results"),

h5("Step 1: Visualizing a High-Injury Network for Pedestrians and Bicyclists based on Past Crash History", id="uc-example-nola-results-1"),

p("The tool’s Sliding Windows Analysis feature provides an initial look at historical crash density per mile by mode. This snapshot of areas where crashes previously occurred, and their concentration, weighted by severity, provides planners with an instant visualization of the road network to compare to past crash analyses. Planners are using these results to focus crash reduction strategies on street segments that have higher than desired crash densities.  This focus on targeted street segments enables planners to understand street design commonalities that may contribute to higher crash densities.  These might include wide streets, multilane intersections, high traffic generators, lack of medians, or lack of sidewalks and bike lanes.  This information can then be used to determine appropriate engineering countermeasures.  The City of New Orleans’ law enforcement agencies are using this information to inform traffic enforcement and awareness programs using a “data-driven” approach."),


p("The City of New Orleans’ previous approach to identifying problematic location focused on “hot spot” intersections versus street segments.  This resulted in a hopscotch approach instead of a systemic approach to addressing risks that may be present for several blocks of a street. A comparison of the two approaches, as shown below, demonstrates that there is overlap between the two approaches, but the crash density information illustrated in the Sliding Windows Analysis enables planners to more easily find streets that require prioritization."),
  
# These should be formatted to be side by side

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/WZPDBgJo0L/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola ped hotspot", width = "100%"), style='max-width:926px;')),
p(tags$div(img(src="https://tooledesign.egnyte.com/dd/NVGzHwjuot/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola ped crash density screenshot", width = "100%"), style='max-width:1094px;')),

p("In addition, the tool allows for quick visual comparison of how sliding windows vary by mode: while some corridors stand out for a high frequency of serious crashes for a specific mode, other corridors stand out for being relatively crash-dense for all modes. These visualizations can support mode-specific planning efforts or multi-modal approaches to roadway redesigns. These distinctions can be important in engaging the community and decisionmakers on safety priorities as well as targeted awareness campaigns for vulnerable road users. For example, these results have created new collaboration opportunities with key stakeholders such as the New Orleans Regional Transit Authority because some of the higher pedestrian crash densities  in the Sliding Windows Analysis correspond with high-ridership fixed route corridors where transit users may be at higher risk for traffic injuries. The Transit Authority can use this information to adjust station locations, improve connections to and from station locations, and increase awareness by transit users for traffic risks."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/NVGzHwjuot/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola ped crash density screenshot", width = "100%"), style='max-width:1094px;')),

p("Similarly, City planners have found the bicyclist Sliding Windows results for New Orleans to be helpful in assessing where targeted interventions are needed to reduce severe-injury crashes. As with pedestrian crashes, the City of New Orleans previously used “hot spot” analyses to derive priority locations for bicyclist crash reduction strategies.  This approach limited the City’s ability fully leverage street improvement projects where bike facilities and other design interventions could be integrated.  Planners are using the bicyclist Sliding Windows results to determine where protected bike lanes can be implemented to drive down severe injuries.  This work is largely focused on the downtown and downtown-adjacent neighborhoods of New Orleans, but will expand to other areas of focus."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/MR6kmm1euX/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola bike crash density screenshot", width = "100%"), style='max-width:1102px;')),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/1UTLKXLBjH/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola other crash density screenshot", width = "100%"), style='max-width:1104px;')),

h5("Step 2: Modeling Relative Risk and Crash Costs", id="uc-example-nola-results-2"),

p("The City of New Orleans is in the early stages of developing a local road safety plan that is consistent with FHWA guidance and consistent with Vision Zero initiatives across the US.  The City of New Orleans also intends to move from reactive to proactive approaches that look not only at past crash histories but toward more predictive measures. This includes factoring in the cost of no action into policy decisions.  Using the tool, the City of New Orleans was able to generate an estimation of costs associated with future pedestrian and bicyclist injuries and deaths if no action is taken to mitigate traffic hazards.  The City of New Orleans is using these costs to project aggregated costs across the city to project estimated costs forward.  This information is being used to engage the community, local and state transportation agency leaders, and elected officials on the effects of inaction.  This information is also being used to inform project delivery goals and performance metrics in the areas most impacted."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/1pGOa009pO/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola ped crash costs screenshot", width = "100%"), style='max-width:1094px;')),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/wP9ZCjtGPF/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola bike crash costs screenshot", width = "100%"), style='max-width:1099px;')),

h4("2.1.5 Conclusions and Applications", id="uc-example-nola-conclusion"),

p("Based on the model results, the City of New Orleans was able to identify a concise list of street segments where changes in the built environment to improve walking and bicycling safety are most likely to have a measurable impact on future outcomes. In this case, several of these corridors have already recently undergone safety-oriented changes or are slated for future improvement. Thus, this analysis provides a useful baseline for future evaluation of project impacts. For other corridors, the model results provide a valuable roadmap for selecting and proposing proven crash countermeasures, allocating or securing funding, and engaging the community, local leadership, and elected officials."),

h3("2.2 New Orleans RTA: Transit System Rider Safety and Operator Training", id="uc-example-rta"),


h4("2.2.1 Jurisdiction or User:", id="uc-example-rta-user"),

p("New Orleans Regional Transit Authority"),

h4("2.2.2 Problem Identification and Analytic Objectives:", id="uc-example-rta-problem"),

p("The New Orleans Regional Transit Authority (RTA) also collaborated on the development of this tool, with the intent that it can be used to evaluate a) areas where pedestrian safety enhancements along the transit network are most likely to benefit transit riders, and b) to explore additional analytic uses of the model itself, such as mapping and evaluating crashes involving transit vehicles in order to identify and prioritize systemic safety issues. "),

h4("2.2.3 Data Sources and Preparation", id="uc-example-rta-data"),

p("For analysis of pedestrian safety, the RTA used results of the analysis completed by the City of New Orleans. For modeling crashes involving transit vehicles, the agency maintains a database of incident reports which includes location and a form of severity. "),

h4("2.2.4 Analysis results", id="uc-example-rta-results"),

p("Although the tool- and its underlying models- are designed to focus on vulnerable road users, built in capabilities for modeling Sliding Windows for “other” vehicles, and identifying top crash locations, mean that a user can also visualize and evaluate any other sub-set of crashes (e.g. s: motorcycles, crashes involving alcohol, etc.) by simply defining the subset of interest as “other” and excluding all other crashes from the analysis."),

p("The built-in map visualization tool, moreover, allows the user to upload additional shapefiles (such as transit routes or stops) in order to visually assess the relationship of crash outcomes to features of interest, or the results can be exported for further analysis. For instance, the RTA can use transit ridership as an additional input - mapped against high-crash locations by proximity and filtered to identify only high-ridership segments - to identify agency priorities in discussion with City departments about future investments. The figure below illustrates an example of the tool’s mapping application, with transit stops layered in relation to a high-crash segment of interest."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/hlJSTNlrkJ/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="nola transit stops and ped crash costs screenshot", width = "100%"), style='max-width:762px;')),

p("The results of the model are of immediate consequence to the agency. Streets that were identified as having the highest level of risk for pedestrians coincide with some of the most important and heavily trafficked routes in the RTAs system. It will require immediate action to begin the process of reducing risk on these corridors."),

h4("2.2.5 Conclusions and Applications", id="uc-example-rta-conclusion"),

p("After reviewing the results of this model, the RTA now has a roadmap of areas to advocate for safety improvements for its ridership. Drawing on the outputs from this model, as well as additional data in the form of ridership levels and demographics of the neighborhood a stop is located in, they will be able to develop a framework to ensure a safe and equitable pedestrian environment for our riders. As future routes and improvements are planned, pedestrian walksheds can be overlaid with crash maps to ensure stops are placed in a manner that reduces any other barriers to ridership for the general public. This information can be used by the agency in approaching other city and state level agencies to identify issues and push for improvements, as well as in grant applications to help finance these initiatives."),

p("Additionally, layering information of streets with a notably high crash density with the areas where bus and streetcar operators experience crashes will allow the agency to better distinguish which crashes may have been due to operator error and which may be due to unsafe design. These Sliding Windows maps can also be used in training, helping to prepare operators to be more wary in these areas."),

p("Importantly, the Crash Cost model and underlying assumptions about contributing factors to risk have not been validated on “off-label” uses of the tool such as transit vehicle crashes. Outputs should be considered exploratory in nature and treated as such."),

h3("2.3 Lincoln Parish: Active Transportation Network Development & Health Equity Impact Evaluation", id="uc-example-lincoln"),

h4("2.3.1 Jurisdiction or User:", id="uc-example-lincoln-user"),

p("City of Ruston & Lincoln Parish, Louisiana"),

h4("2.3.2 Problem Identification and Analytic Objectives:", id="uc-example-lincoln-problem"),

p("The City of Ruston has committed to the development of an interconnected network of active transportation and recreation facilities, leveraging a variety of partners and funding sources, that, together, are intended to ensure equitable access to opportunities for physical activity, as one tool in improving health equity outcomes. The City of Ruston’s overall vision for the active transportation network, centered on the “spine” of the Rock Island Greenway, provides a robust and flexible framework for incremental growth, however, as connections to off-street facilities are developed within the existing street network (a combination of new or expanded sidewalks, shared-use trails and on-street bike lanes) and the city advances initiatives to encourage their use, new safety concerns are emerging."),
p("The new infrastructure is explicitly intended to address equity gaps and racial disparities in access to opportunities for physical activity, creating connections to lower-income and zero-vehicle households. The City of Ruston wants to ensure that these connections are safe, and prioritize future investments in areas which are likely to see serious crashes in the future if nothing is done.  There is relatively limited pedestrian and bicycle crash history on much of the local street network, particularly in locations where no dedicated infrastructure previously existed. An absence of crashes, however, does not necessarily indicate that the area is safe for people walking and bicycling: on the contrary, some of these areas are understood to have been avoided by vulnerable users due to the perception that they are too dangerous to walk or bike. Thus, alternative approaches to evaluating safety both before and after new infrastructure likely to impact user volumes is completed – particularly in the context of a small, rural jurisdiction with relatively few crashes overall – is needed."),

h4("2.3.3 Data Sources and Preparation", id="uc-example-lincoln-data"),

p("Lincoln Parish provided a local road centerlines file, which includes slightly greater detail (new roads, driveways, and alleys) than the default OSM data. Crash data was obtained from the Louisiana Department of Transportation and Development. In low-population, low-crash areas in particular, testing indicates that the use of FARS data yields minimally useful results. Relational database tables needed to be joined, and some QA/QC of data was required in order to address errors in geocoding of crash locations, in order to ensure as few records would be excluded as possible, given the low total number of pedestrian and bicycle-involved crashed during the 5-year study period."),

h4("2.3.4 Analysis results", id="uc-example-lincoln-results"),

p("Sliding Windows Analysis reveals the limitations of evaluation based on crash history alone: for bicyclists in particular, most segments experienced zero crashes, and only one corridor had a Bicycle Sliding Windows score greater than one (notably, there were zero fatal or serious injury bicycle crashes in Lincoln Parish during the 5-year analysis period). There were over three times as many pedestrian crashes, most of which were concentrated in the relatively more walkable downtown area. Here, areas which experienced serious or fatal crashes show up starkly in outlying areas of the parish, but it’s unclear whether these are isolated incidents or systemic issues given the limited data. Overlaying bicycle and pedestrian Sliding Windows with motor vehicle-only crashes, however, begins to illuminate this."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/xLAeGQ5u3X/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="lincoln ped crash density screenshot", width = "100%"), style='max-width:1092px;')),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/rayKGgv9A1/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="lincoln bike crash density screenshot", width = "100%"), style='max-width:1091px;')),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/rayKGgv9A1/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="lincoln other crash density screenshot", width = "100%"), style='max-width:1089px;')),

p("The model results definitely lend a bit more nuance to the picture, relative to the Sliding Windows alone. Please note, the below results were obtained prior to integration of the model cost calibration, so the images reflect costs that are higher than the actual model estimation. Because the estimated crash costs results are lower than what can be visualized in the browser, the user is prompted to view results by downloading them and loading into a GIS software. For bicycles, new corridors emerge as potential areas for improvement based on the built environment characteristics of and surrounding those roadways.  Several of these segments overlap with corridors where bike facilities have been proposed, but not yet implemented.  For pedestrians, however, fewer corridors stand out as likely to result in high relative crash costs, and all of these are outside the downtown area where current investments in active transportation are being made. This likely reflects relatively low speed limits and low traffic volumes within city limits, indicating that, particularly with the provision of sidewalks to make it more convenient to traverse downtown and surrounding neighborhoods, few serious crashes are likely to occur on the new facilities. On the other hand, the segments which do emerge as potential areas of concern should be evaluated more closely to identify whether they make sense as next-steps to expand access to that walkable network."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/90Ay6wmU4w/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="lincoln ped crash costs screenshot", width = "100%"), style='max-width:1098px;')),

p(tags$div(img(src="https://tooledesign.egnyte.com/openpublicdocument.do?forceDownload=false&preview=true&thumbNail=true&w=1200&h=1200&type=proportional&forceDownload=false&link_id=3kpBrxHYKq&entryId=6222fe34-b2c4-4508-9c15-8cbbb3233557&cb=1626459842835", alt="lincoln bike crash costs screenshot", width = "100%"), style='max-width:1098px;')),

p("Finally, comparison of the estimated crash costs to the current bikeway network (much of which was completed in the last three years, complicating analysis of crash history), indicates that only one segment on the existing network is at a notably elevated risk. However, an additional corridor – N. Chatauqua Road— connecting directly to an access point for the Rock Island Greenway, represents an important missing link for users of that facility and the network as a whole. Moreover, there are segments that appear as likely to experience high crash costs in the future where no crashes were reported during the five-year period: as bicycle volumes grow in the region, these may represent important opportunities to address the root causes of crash risk before anyone is injured or killed."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/pel4Qsr5z9/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="lincoln bike crash costs with facilities screenshot", width = "100%"), style='max-width:919px;')),

h4("2.3.5 Conclusions and Applications", id="uc-example-lincoln-conclusion"),

p("The results of preliminary use of this tool are being used to inform a health equity evaluation on the extent to which recent and planned investments in new infrastructure. As a next step, the Parish’s crash data will be binned as it becomes available into intervals representing pre-and post-intervention conditions, as a tool (in combination with bike and pedestrian counts, surveys, and other methods) for understanding correlations between safety outcomes and the City of Ruston’s investments. Moreover, the tool, and its built in, interactive dashboard, offer a quick, easy way for the City of Ruston’s small planning and public works staff to integrate previously labor-intensive crash analysis into annual reporting and grant application development, providing new ways to measure and promote success."),

h2("Disclaimers", id="disclaimers"),

p("This document and the information contained herein is prepared solely for the purpose of identifying, evaluating and planning safety improvements on public roads which may be implemented utilizing federal aid highway funds; and is therefore exempt from discovery or admission into evidence pursuant to 23 U.S.C. 409.  Contact the Louisiana Department of Transportation and Development’s Traffic Safety Office at (225) 379-1871 before releasing any information."), 


)
)
  
}
    
#' faq Server Functions
#'
#' @noRd 
mod_use_case_server <-  function(input, output, session){
    ns <- session$ns
}
    
## To be copied in the UI
# mod_use_case_ui("use_case_ui_1")
    
## To be copied in the server
# mod_use_case_server("use_case_ui_1")
