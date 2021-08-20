#' faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_faq_ui <- function(id){
  ns <- NS(id)
  tagList(
  fluidPage(
 # add text here

# h2("FAQ's"),

h3("Frequently Asked Questions", id="faq-toc"),
p(
tags$a(href="#faq-1", "1. Basics", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-2", "1.1 Who are the intended users of the Safer Streets Priority Finder (SSPF)?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-3", "1.2 Do I need to know how to use GIS if I want to use the SSPF?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-4", "1.3 Do I need to be familiar with traffic safety or systemic safety analysis to use the SSPF?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-5", "1.4 What internet browsers are compatible with the SSPF?", style = "color: #007bff;"),
br(),
tags$a(href="#faq-6", "2. Data Inputs", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-7", "2.1 What data do I need to use the SSPF?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-8", "2.2 How will my results vary based on whether I use default data versus my own local data?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-9", "2.3 Where can I find crash data with additional severity levels beyond fatal crashes?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-10", "2.4 In what cases should I run a new study?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-11", "2.5 How were the default crash costs developed, and when should I use default costs versus inputting custom costs?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-12", "2.6 Where can I find local crash costs for my state?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-13", "2.7 For users uploading their own data", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-14", "2.7.1 What geography should my study area encompass?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-15", "2.7.2 What is the largest file size I can upload?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-16", "2.7.3 How are dual carriageways handled?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-17", "2.8 Assigning data inputs to information used in the tool", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-18", "2.8.1 How do I assign my roadway data's functional classification to the options available if there isn't a one to one match?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-19", "2.8.2 How are freeways/roadways with restricted access handled?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-20", "2.8.3 How are multi-use paths handled?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-21", "2.8.4 What crash severity scale is accepted?", style = "color: #007bff;"),
br(),
tags$a(href="#faq-22", "3. Tool Analysis Outputs", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-23", "3.1 What are the tool's primary outputs, and how do they differ?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-24", "3.2 How do the analyses available in the SSPF fit into broader Vision Zero principles?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-25", "3.3 How do the analyses available in the SSPF fit into the range of data analysis techniques commonly used to address traffic safety issues?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-26", "3.4 What do we mean by pedestrian and bicyclist risk?", style = "color: #007bff;"),
br(),
HTML('&emsp;'), tags$a(href="#faq-27", "3.5 Safer Streets Model", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-28", "3.5.1 How does the Safer Streets Model forward our understanding of pedestrian and bicycle safety?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-29", "3.5.2 How long does the Safer Streets Model take to run?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-30", "3.5.3 How was the model validated?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-31", "3.5.4 How do I interpret the cost estimates provided by the Safer Streets Model?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-32", "3.5.5 How should I interpret models that utilize only FARS data?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-33", "3.5.6 How does the model account for a range of crash observations?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-34", "3.5.7 How well does the model work in rural areas?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-35", "3.5.8 How should I interpret model fit charts within the dashboard?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-36", "3.5.9 Why does my model indicate crash risk even in areas where there were no historic crashes?", style = "color: #007bff;"),
br(),
HTML('&emsp;&emsp;'), tags$a(href="#faq-37", "3.5.10 How do I use the model outputs?", style = "color: #007bff;"),
br(),
),

h2("1. Basics", id="faq-1"),

h3("1.1 Who are the intended users of the Safer Streets Priority Finder (SSPF)?", id="faq-2"),
p("The SSPF was designed for use by planners and engineers working for municipal government agencies or departments (city, county, regional, state, federal, etc.) who have a basic familiarity with GIS data and traffic safety and are engaged directly in project prioritization and implementation. Advocates and interested members of the public are also welcome to use it. For agencies that wish to share the outputs or results broadly with the public, we recommend exporting the results (e.g., reports, images, shapefiles) and hosting them on the agency's website. This tool is not intended to be used for interactive public engagement."),
    
h3("1.2 Do I need to know how to use GIS if I want to use the SSPF?", id="faq-3"),
p("This tool was designed assuming minimal familiarity with GIS. If you have access to datasets that are already properly formatted, or if you are using the default datasets, no additional GIS knowledge is needed to load data, run analyses, or visualize results within the tool. Users will need to be familiar enough with the data they are uploading to indicate which fields correspond to the information used by the tool. Some GIS familiarity is helpful for people who need to reformat their data, or who wish to export their results for further analysis."), 

h3("1.3 Do I need to be familiar with traffic safety or systemic safety analysis to use the SSPF?", id="faq-4"),
p("Baseline familiarity with traffic safety or systemic safety analysis is helpful for context in understanding and applying the SSPF outputs. Please use caution when interpreting data outputs. There are limitations with the available outputs that have been documented throughout the SSPF and are important to review."), 

h3("1.4 What internet browsers are compatible with the SSPF?", id="faq-5"),
p("The SSPF can be used in Chrome, Safari, Firefox, and the newest version of Edge."),

h2("2. Data inputs", id="faq-6"),

h3("2.1 What data do I need to use the SSPF?", id="faq-7"),
p("The analysis can be conducted either using 1) local data uploaded by the user or 2) nationally-available default data. If using user-uploaded data, study area, roadway network, and crash data is required."),

h3("2.2 How will my results vary based on whether I use default data versus my own local data?", id="faq-8"),
p("We strongly encourage uploading local crash data when it is available with crash severity details, as the nationally-available FARS crash data used as the default is limited to fatal crashes. Because of the limited number of pedestrian and bicyclist fatalities in any one area, results based on default data will be sparse and should be used with caution. The Sliding Windows analysis using FARS data may look very sparse or unhelpful, as this analysis relies on severe and minor injury crashes to help understand connected corridors of higher crash risk. The model outputs may not be as visibly limited by relying on FARS data, but the results from models run on fatal crashes only have not been validated."),

h3("2.3 Where can I find crash data with additional severity levels beyond fatal crashes?", id="faq-9"),
p("In the course of developing and testing this tool, a scan of state-level publicly available datasets which may be suitable for use as an alternative to FARS data was completed. The results of this scan are summarized", 

HTML("<a href=\"./crashdatasources\" target=\"_blank\"style=\"color: #007bff;\">here,</a>"), "as well as a few City-level open data portals identified during the process. While many governmental agencies can readily access detailed crash data from state DOTs, either through a web portal or by request, general public/non-governmental access to spatial data necessary to utilize this tool varies widely. Many states have developed open data portals where crash data may be visualized and/or downloaded in spatial or tabular formats, while others only provide summary statistics about recent crash histories in a jurisdiction. This scan does not represent a comprehensive list of state-level data sources; users are encouraged to seek additional resources not listed here for their state or local jurisdiction. In addition, the availability of spatial data does not necessarily imply that available datasets will include all attributes necessary for use with this tool. For some datasets, extensive data processing and/or specialized technical capabilities may be required to derive a shapefile suitable for use. This table is included as a starting point for identifying local crash data."),


h3("2.4 In what cases should I run a new study?", id="faq-10"),
p("A new study would capture changes to any of the following: study area, time period (crash data), or data source (roadway or crash data). Additional studies can be used to run an analysis larger than one county, or when the county of interest is large and is processing slowly--in these cases splitting the study area into smaller sections will improve processing time. To do this, you will need to open your input datasets in GIS software and split them into separate study portions."),

h3("2.5 How were the default crash costs developed, and when should I use default costs versus inputting custom costs?", id="faq-11"),
p("See the Overview and Methodology tab for more information on how the crash costs were developed. The default crash costs are intended to reflect comprehensive societal costs for each crash severity for the U.S., using methods recommended by FHWA. These costs are not localized for states--they do not reflect state-specific injury to crash incidences used to develop crash unit costs from person-injury unit costs, and will not reflect state-specific cost of living adjustment factors. For localized crash costs, the user will need to input their own costs (see below for information on how to find local crash costs). Users also have the option to customize the discount rate which is applied to reflect today's value of costs projected over a time horizon of five years. "),

h3("2.6 Where can I find local crash costs for my state?", id="faq-12"),
p("Typically, this is something published by state DOTs. A survey of state crash costs by severity is included on page 75 of",
  HTML("<a href=\"https://safety.fhwa.dot.gov/hsip/docs/fhwasa17071.pdf\" target=\"_blank\"style=\"color: #007bff;\">FHWA's Crash Costs for Highway Safety Analysis (2018),</a>"),
  "but the data in this document is incomplete or outdated for some states."),

h3("2.7 For users uploading their own data", id="faq-13"),


h4("2.7.1 What geography should my study area encompass?", id="faq-14"),
p("Counties are currently the largest available jurisdiction the SSPF is intended to accommodate. If you wish to study a larger area, you can separately analyze several counties and compile the results post-hoc in GIS. If your county is large or contains a lot of crashes or roadway segments, or if the tool is running slowly, you may need to split your study area into pieces. Note that the limiting factor is the number of features (e.g., number of crashes, number of road segments) and not absolute size of the geographic area."),
    
h4("2.7.2 What is the largest file size I can upload?", id="faq-15"),
p("For roads data, the maximum allowable length of roadway miles that can be uploaded is 26,700 miles. For crash data, the maximum allowable number of crashes is 150,000. If your data exceeds these limits, you will need to clip the data prior to uploading it."),
     
h4("2.7.3 How are dual carriageways handled?", id="faq-16"),
p("Many agencies digitize their divided roadways as dual centerlines or dual carriageways (i.e., two lines). In this case, crashes may be attached to one side of the roadway or another. This may reduce the density of crashes per mile (i.e., underestimate risk) on these portions of the network, since the mileage for this type of road is represented twice. This may result in crash risk being underestimated on divided roads in both the Sliding Windows analysis and the Safer Streets Model. Advanced GIS users may wish to download and transform the analysis outputs on dual carriageways to correct for this issue, though doing so is at your own risk and the results have not been validated using this approach. For two \"matched\" dual carriageway segments (i.e., segments representing either direction of the same stretch of roadway), you can sum the output from each side, and use the sum as the corrected output for both segments. Users should be wary when interpreting model results for streets with dual centerlines."),

h3("2.8 Assigning data inputs to information used in the tool", id="faq-17"),

h4("2.8.1 How do I assign my roadway data's functional classification to the options available if there isn't a one to one match?", id="faq-18"),
p("If a one to one match is not available, choose the closest option that corresponds with your data's functional classification. The functional class is used to inform the relative crash risk based on national data."),

p(
	"FHWA definitions of functional classifications can be found ",
	HTML("<a href=\"https://www.fhwa.dot.gov/planning%20/processes/statewide/related/highway_functional_classifications/section03.cfm#Toc33687298\" target=\"_blank\" style=\"color: #007bff;\">here.</a>")
	),
	
p("Note that the SSPF uses the term ", tags$i("Major Arterial"), "instead of the FHWA's term", tags$i("Other Principal Arterial"), ". Also note that our data show greater average risk per mile for pedestrians and bicyclists nationwide on minor arterials than major arterials, possibly because of a combination of roadway design factors, destination access, and exposure."),

p("The model factors in pedestrian and bicyclist risk according to functional class as follows:"),

p(img(src="https://user-images.githubusercontent.com/15150997/117335587-2a95a900-ae50-11eb-9cfe-7184eac4f430.png", alt="road class and risk", width = "35%")),


h4("2.8.2 How are freeways/roadways with restricted access handled?", id="faq-19"),
p("If you want to exclude these from the analysis, you can remove them from your dataset before uploading or group them as a separate functional class, which can then be assigned to the ", tags$i("omit"), "category."),
p("If you want them analyzed separately, assign them to the freeway functional class."),

h4("2.8.3 How are multi-use paths handled?", id="faq-20"),
p("You can remove them from your dataset before uploading or group them as a separate functional class, which can then be assigned to the", tags$i("omit"), " category."),

h4("2.8.4 What crash severity scale is accepted?", id="faq-21"),
p("The SSPF accepts crash severity according to the KABCO scale, which is used in most police crash reports. The SSPF does not currently accommodate other crash severity categories (such as injury/non-injury or MAIS)."),
p("MMUCC 5th Edition provides definitions for each KABCO level and is available", 
  HTML("<a href=\"https://www.nhtsa.gov/mmucc-1\" target=\"_blank\" style=\"color: #007bff;\">here.</a>")),
p("Note that excluding 'O' or 'PDO' crashes, especially motor vehicle PDO crashes, will not meaningfully impact the model results. If you have a large crash dataset with a lot of motor vehicle PDO crashes, consider dropping these from the dataset before uploading to make the tool run faster."),
              
h2("3. Tool Analysis Outputs", id="faq-22"),

h3("3.1  What are the tool's primary outputs, and how do they differ?", id="faq-23"),
p("There are two discrete outputs from the SSPF--a Sliding Windows Analysis and a Safer Streets Model. In most cases practitioners will use one of these outputs, but not both. The Sliding Windows Analysis output reflects historical crashes, while the Safer Streets Model output reflects estimated crashes. The table below provides an overview of both analyses. For more specific details on how to use the Safer Streets Model outputs, see FAQ 3.5.10."),

p(tags$div(img(src="https://tooledesign.egnyte.com/dd/p56th7PHEq/?thumbNail=1&w=1200&h=1200&type=proportional&preview=true", alt="analysis overview table", width = '100%'), style='max-width:1047px;')),

h3("3.2  How do the analyses available in the SSPF fit into broader Vision Zero principles?", id="faq-24"),
p("Vision Zero is a movement and strategy based in the recognition that traffic deaths are unacceptable and preventable, which seeks to eliminate traffic deaths and severe injuries for all roadway users. Cities throughout the United States have made Vision Zero commitments. Elemental to Vision Zero is a data-driven, proactive, and systems-based approach to safety. The SSPF offers communities a starting point for safety analyses to provide an understanding of pedestrian and bicycle crash risk along their roadway network which can be used to prioritize safety investments. As part of Vision Zero, many communities create a High Injury Network (HIN). The outputs from this tool, both from the sliding windows analysis and the Safer Streets model, can be used as inputs to creating a HIN."),

h3("3.3 How do the analyses available in the SSPF fit into the range of data analysis techniques commonly used to address traffic safety issues?", id="faq-25"),
p("A variety of data analysis techniques are commonly used by agencies to address traffic safety issues. These include hot spot analyses, Sliding Windows analyses/High Injury Networks, and systemic safety analyses. Each of these analyses, along with the SSPF’s related capabilities, are discussed below."), 
p("Although hot spot analysis is relatively simple, it may miss clusters of crashes along a corridor, is limited in its effectiveness for bicyclist and pedestrian crashes, and doesn’t account for the fact that hotspots move around a network over time. Beyond visualizing crash data, the SSPF is not intended to provide hot spot analysis capabilities."),
p("The SSPF provides a Sliding Windows analysis, which can be used to develop a High Injury Network. Sliding Windows analyses addresses some of the shortcomings of hot spot analyses by highlighting patterns of safety problems at the corridor level, which can then be used to inform prioritization of safety treatments. Typically, Sliding Windows analyses have a higher barrier to entry due to the more complex GIS and coding skills needed to develop them. This tool lowers the barrier to entry by automating this process. High Injury Networks are based on sliding windows, but identify a subset of streets with the highest density of severe crashes, allowing jurisdictions to prioritize investment and focus on the most critical safety issues."), 
p("Systemic safety analysis involves reviewing the characteristics of locations where crashes occurred to understand the risk factors present at locations where crashes have happened. Once risk factors have been identified, the network can be screened for risk factors to understand where crashes are more likely to occur in the future, whether or not there have been historical crashes in these locations. Safety Performance Functions (SPFs), which are a key systemic safety analysis method, require significant data and GIS/coding expertise to develop. The Safer Streets Model is not as robust as a safety performance function and should not be seen as a replacement for bicyclist- and pedestrian-specific safety performance functions. It does not link results to specific roadway configurations on the network. However, by using external data from national functional class fatality rates and the national Pedestrian Fatalities Model, the Safer Streets Model adds a proactive element to the analysis above and beyond simple Sliding Windows analysis. It is a useful entrypoint to systemic analysis for those with limited GIS and coding skills or those without extensive roadway data."),

h3("3.4 What do we mean by pedestrian and bicyclist risk?", id="faq-26"),
p("Because pedestrian and bicyclist exposure data is not widely available, proxies, like those used within the Safer Streets Model are useful. This is in line with the third definition of pedestrian and bicyclist risk outlined in the three possible definitions of risk in FHWA’s", 
  HTML("<a href=\"https://safety.fhwa.dot.gov/ped_bike/tools_solve/fhwasa18032/step3.cfm\" target=\"_blank\" style=\"color: #007bff;\">Guide for Scalable Risk Assessment for Pedestrians and Bicyclists.</a>")),

h3("3.5 Safer Streets Model", id="faq-27"),

h4("3.5.1 How does the Safer Streets Model forward our understanding of pedestrian and bicycle safety?", id="faq-28"),
p("While the Sliding Window analysis output network forms the basis of a typical High Injury Network, the Safer Streets Model goes further than a typical High Injury Network by factoring what else we know about risk from the Pilot Tool's model and national functional class averages in addition to historical crashes. Pedestrian and bicyclist crashes, while too common, are statistically rare. In places where nobody walks or bikes, you’re unlikely to see pedestrian or bicyclist crashes. The absence of crashes does not imply an absence of risk. The modeling framework provides a way to understand risk even where there isn’t a crash history, but with a lower data barrier to entry than systemic analysis and network screening. The model incorporates crash costs per severity level to enable outputs to be linked to the planning process. "),

h4("3.5.2 How long does the Safer Streets Model take to run?", id="faq-29"),
p("The model's processing time will depend on the number of other users with a model in queue and the size of your data, but in most cases it will take between several hours, and in many cases, will take a full day to run. You'll receive an email notification once you begin the model production process and another email once the model is complete and ready for review. You can close the webpage and the model will continue to process. Please check your junk email inbox if you do not see the email notifications."),


h4("3.5.3 How was the model validated?", id="faq-30"),
p("The model was validated by comparing the performance of the Bayesian Model outputs with those of Sliding Window Analysis outputs at estimating crashes that are out-of-sample (i.e. different time period than those of input crashes).  Validation was carried out for three study areas – City of New Orleans, Lincoln Parish (LA), and Lowell (MA). Validation consisted of examining what percent of out-of-sample severe crashes happened within top-scoring road segments. In other words, how often were severe crashes happening on roads that had higher risk scores from the model, versus severe crashes happening on lower scoring road segments. In all three study areas, the Bayesian Model out-performed Sliding Window Analysis  (i.e., high scoring roads from the model captured a larger share of out-of-sample severe crashes than high scoring roads from the Sliding Window analysis). In cases with large number of observed crashes, the difference in the two outputs is smaller but the model does significantly better when observed crash data is very sparse."),

h4("3.5.4 How do I interpret the cost estimates provided by the Safer Streets Model?", id="faq-31"),
p("The crash cost rates selected by the user in the load data step are applied to the calibrated mode and severity crash estimates from the model to create a weighted aggregate score for each mode. For example, the pedestrian crash risk cost score is calculated by multiplying the calibrated model output for fatal pedestrian crashes on each window by the cost rate for fatal crashes, repeating this for each additional severity level’s modeled output and cost rate, and summing the values into a single pedestrian score. These cost rates calculated for each segment represent the annual expected societal cost per mile due to crashes from the model, given the trends do not change. For further investigation beyond what is offered within the tool’s interface, these costs can be converted into a cost per window segment per year, by multiplying the cost rate per mile by the segment length. At this point, the cost per segment per year can be summed across corridors."), 

p("The outputs are calibrated so that the total crash cost in the study area for a given mode is the same for observed and model estimated crashes. While the overall crash costs for a given mode will be consistent with that of observed crashes, individual severity cost totals breakdown will not be the same. In general, the calibrated model outputs tend to overestimate fatal crashes and underestimate lower severity crashes, with the total sum of calibrated costs across the network approximately equal to the observed cost of crashes on the network over five years. The model fit chart in the Dashboard tab shows the distribution of observed and estimated crashes by mode and severity after calibration."),

h4("3.5.5 How should I interpret models that utilize only FARS data?", id="faq-32"),
p("FARS data only has fatal crash information. Since there is no accurate way to estimate non-fatal crashes using FARS data, the model outputs for non-fatal crashes will be generated as though there are no observed crashes for those severity levels. In other words, the model will only use the Pilot Tool Model, functional class rates, and fatal crashes as priors in estimating outputs. The outputs will be calculated based on only the tract and functional class of the roads for non-fatal crashes. It is likely that the model estimated crashes for non-fatal severity will be lower when using FARS data than when using locally available crash data that includes all crash severities."),

h4("3.5.6 How does the model account for a range of crash observations?", id="faq-33"),
p("The model assigns a base level of risk to segments based on tract level Pilot Tool Model data and the functional class of the segments even when there are no observed crashes on the segment. When there are very few observed crashes on the network, the number of segments without any observed crashes are higher. Since the model estimates crash risk on these segments, the total number of crashes estimated will be higher in proportion to the number of observed crashes."),
p("For example, in a study area with 50 segments let us assume that the model estimate crashes are 1.1 on those with one observed crash and 0.2 on those without any observed crashes. Take two cases:"),

p(img(src="https://user-images.githubusercontent.com/15150997/125214823-08644100-e26e-11eb-9180-a34c34013f5e.PNG", alt="pbservation example table", width = '50%')),

p("The model appears to overestimate crashes by a much higher value (190%) in case 2 than in case 1 (40%). This is mainly due to the aggregating effect of crash risk associated with segments with no crash history. Please note that the values used are for illustration only, the actual crash risk that the model assigns are not uniform or deterministic in nature and vary based on the Pilot Tool Model inputs, functional class, and the variability associated with sampling process used by the Bayesian model. The final outputs of the tool are also calibrated to match the observed and modeled crash costs for the study area which might cause further distortion which will likely be much more pronounced for cases with low observed crashes."),
p("Model results should be used with caution for areas with low observed crashes."),

h4("3.5.7 How well does the model work in rural areas?", id="faq-34"),
p("Rural areas tend to have low number of observed crashes which would typically make the model estimated crashes vary heavily from those of observed. Like the case with low number of crashes mentioned in FAQ 3.5.6, the model results should be used with caution in rural areas with low observed crashes."),

h4("3.5.8 How should I interpret model fit charts within the dashboard?", id="faq-35"),
p("The model fit chart in the Dashboard tab shows the distribution of observed and estimated crashes by mode and severity after calibration. It is likely that the total model estimated crashes are different from the observed crashes due to the different ways the tool tallies observed crashes and model crashes, as the calibrated model outputs tend to overestimate fatal crashes and underestimate lower severity crashes, with the total sum of calibrated costs across the network approximately equal to the observed cost of crashes on the network over five years. "),
p("Observed crash totals represent only those crashes that were not flagged and excluded from analysis. Crashes might be flagged and excluded for many reasons like missing id, invalid location, invalid crash year, mode, or severity, etc."),
p("Model estimated crashes on the other hand use priors from the Sliding Windows analysis. The model also assigns a crash estimate on streets without any observed crashes. These model outputs are then calibrated so that the total sum of calibrated costs across the network are approximately equal to the observed cost of crashes on the network over five years. "),
p("Depending on the location of the observed crashes (mid-block and intersection) and the total number of observed crashes, the total tally of model estimated crashes will be higher or lower than the observed crashes. In general, the charts provide insights on how the aggregate crash totals (for observed vs model estimate) compare when tallied across all segments."),

h4("3.5.9 Why does my model indicate crash risk even in areas where there were no historic crashes?", id="faq-36"),
p("When there are no observed crashes of a given type, the model still assigns the crash risk based on the street’s functional classification and the census tracts that it is part of. Some segments which are part of a tract with high Pilot Tool Model values may see a higher crash risk. In some cases, when a street segment is near multiple tracts (like along the boundary of the tract), it will pick up the sum of Pilot Tool Model values for those tracts which might also lead to a higher crash risk. The absence of crashes does not imply an absence of risk — demonstrating the risk that exists on roads that have not had crashes in the past is central to a systemic safety approach."),

h4("3.5.10 How do I use the model outputs?", id="faq-37"),
p("The two main types of outputs from the model are the crash risk rates and estimated number of crashes. Crash risk rates are the estimated annual number of crashes per mile along the segments. Estimated crashes are calculated by multiplying the rate by the mileage of the segment."),

p("Within the tool's analysis page, model outputs can be visualized. However, this page only visualizes segments with an estimated annual average cost per mile of $120,563 or greater (the value of C - possible injury crash). For rural areas or areas with low numbers of historical crashes, this means results will not be mapped. If this is the case, the results can be downloaded and visualized using GIS software."),
p("When customizing visualizations in GIS software using model outputs, it is recommended to use the crash risk rate for symbolizing the segments to avoid the effect of segment length. For example, if there are two segments with the same crash risk rate along a corridor but one is twice as long as the other. Symbolizing these in a map with estimated crashes instead of crash rates will show the longer segment as much more dangerous than the shorter one. This creates odd discontinuities in the map due to varying lengths of the segments."),
p("For aggregated total crashes along a corridor or a specific neighborhood, it is recommended to use the absolute estimated values rather than the rates. However, keep in mind that the total model estimated crashes can be different from the actual number of historic crashes in the study area due to aggregation of crash risk across all segments and output calibration process.")

)
)
  
  
}
    
#' faq Server Functions
#'
#' @noRd 
mod_faq_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_faq_ui("faq_ui_1")
    
## To be copied in the server
# mod_faq_server("faq_ui_1")
