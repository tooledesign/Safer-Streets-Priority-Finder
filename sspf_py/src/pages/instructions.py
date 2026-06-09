from dash import html
import dash_bootstrap_components as dbc

def layout():
    return html.Div([
        html.H2("Instructions", className="mb-3"),
        html.Ol([
            # 1 Sign in
            html.Li([
                html.Strong("Sign in and set up your study."),
                html.P([
                    "Use the ",
                    html.A("sign in", href="/login", style={"color": "#0267FD"}),
                    " page to sign in to an existing account or create a new account. ",
                    "After signing in, you will be directed to the ",
                    html.A("studies", href="/studies", style={"color": "#0267FD"}),
                    " page to select from existing studies in your account or create a new study.",
                ], className="mb-2"),
            ]),

            # 2 Load Data
            html.Li([
                html.Strong("Load Data"),
                html.P("To use the Safer Streets Priority Finder, you'll either need to prepare and upload your own local data \
                    (recommended for best results) or use the default datasets available in the tool. \
                    Once you upload your data or select the default datasets, \
                    you can review your data in the 'Review Input Data' section which will note any issues with your data and display it on a map. \
                ",
                    className="mb-2"),
                html.H6("Default Data:"),
                html.Ul([
                    html.Li([
                        "Study Boundary: ", 
                        html.A("US Census County Boundaries", href="https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html", target="_blank", style={"color": "#0267FD"}),
                    ], className="ms-6"),
                    html.P("Using your own study area versus the default does not have a strong bearing on the analysis results. \
                        Using your own study area may be helpful if you would like to look at an area smaller than a county. \
                        Note that study areas larger than one county cannot be processed at this time. \
                        If you are interested in a larger study area, you may create additional scenarios in the tool to run the analysis for each county separately.", 
                    className="ms-6"),
                    html.Li([
                        "Roads: ", 
                        html.A("Open Street Map (OSM)", href="https://www.openstreetmap.org/", target="_blank", style={"color": "#0267FD"}),
                    ], className="ms-6"),
                    html.P("A downside to using the default OSM data is that it will not have your local roadway network IDs, \
                        so it may be more difficult to integrate outputs with your local roadway network dataset. \
                        When you upload your own roadway network data, you will be able to join the analysis results back to your road network later on. \
                        You should also consider whether you are fine with the OSM data’s allocation of functional classes on your road network. \
                        WARNING: We make no guarantees about the reliability of OSM data.",
                    className="ms-6"),
                    html.Li([
                        "Crashes: ", 
                        html.A("NHTSA's Fatality Analysis Reporting System (FARS) data", href="https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars", target="_blank", style={"color": "#0267FD"}),
                    ], className="ms-6"),
                    html.P("Using FARS data may produce unexpected or unhelpful results. \
                        If you have access to crash data containing more than just fatal crashes, we recommend you use it. \
                        Because the default FARS data only includes fatalities, it is not ideal for the purposes of the analyses within the SSPF. \
                        If you have access to any kind of geocoded crash data, we strongly recommend using this data, \
                        even if it requires some time on the front end to clean this data up to meet the data requirements listed below.",
                    className="ms-6"),
                ]),
                html.H6("Custom Data: "),
                html.P("For users uploading their own data, the data will need to be prepared to match the specific format requirements outlined below."),
                html.Ul([
                    html.Li("Shapefile uploads must be provided as a single .zip containing .shp, .shx, .dbf, and .prj files, and cannot exceed 20 MB."),
                    html.Li("Roads data is limited to a maximum total roadway length of 45,000 miles within the study area after clipping."),
                    html.Li("Crash data is limited to 250,000 crash records. Crash datasets over 50,000 rows may reduce in-app map performance. Analysis will still run, and you can still download outputs for use in your own GIS tools."),
                    html.Li("If you use FARS data or upload crash data that contains only fatal crashes, non-fatal model results and non-fatal crash costs are not calculated."),
                    html.Li("The tool is designed to use at least 5 years of crash data. If fewer years are available, analysis can still run, but results may be less reliable."),
                ], className="mb-2"),
                html.Ul([
                    dbc.Accordion([
                        dbc.AccordionItem(
                            title="Study Boundary Data Preparation",
                            children=html.Ul([
                                html.P("The data must be in a polygon-type shapefile format. \
                                    While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, and .prj. \
                                    The uploaded .zip file cannot exceed 20 MB."),
                            ])
                        )
                    ], start_collapsed=True, className="custom-accordion",),   

                    html.P(),
                    dbc.Accordion([
                        dbc.AccordionItem(
                            title="Road Data Preparation",
                            children=html.Ul([
                                html.P("The data should be in line-type shapefile format. \
                                    While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, .prj. \
                                    The uploaded .zip file cannot exceed 20 MB."),
                                html.P("As you follow the tool's upload steps, you'll need to indicate which columns in your data store the unique id, functional class and road name attributes. \
                                    In addition, you'll need to assign the functional class values in your data to the functional class categories listed. \
                                    This step is necessary due to the variation in functional class naming conventions used by jurisdictions."),
                                html.P("The shapefile must have the following attributes (attribute names can be different from those given below):"),
                                html.Li("Unique ID - a unique integer value", className="ms-4"),
                                html.Li("Road Name - a text value", className="ms-4"),
                                html.Li("Functional Classification - must be assigned to 'Expressway' 'Major Arterial', 'Minor Arterial', 'Major Collector', 'Minor Collector', 'Local Road'. \
                                        Any limited-access highways should be considered Expressways. \
                                        If you'd like to omit Expressways (or any other class of roads), you should assign them to the 'Omit from Analysis' category. \
                                        If your data does not have a clear matching with the standardized functional classes, you should assign them to the closest matching category. \
                                        If necessary, review the analysis results from an initial assignment and adjust the functional class assignments before re-running the analysis to achieve results that best reflect the actual conditions.", className="ms-4"),
                                html.Li("Total roadway length in the study area must not exceed 45,000 miles. \
                                    This limit was chosen based on the OSM roadways mileage for the largest county (Los Angeles County) we tested the tool for .", className="ms-4"),
                                html.P("Properly formatted road data: "),
                                html.Img(src="/assets/proper_road_data.png", style={"width": "100%", "maxWidth": "600px"}),
                                html.P("Note that dual carriageways -- divided roads that are represented by two lines in the data -- may have lower than expected results when modeled. \
                                    This is because crashes are split between the two lines representing the street -- versus an undivided road where the street is represented by a single line. \
                                    Users should be wary of interpreting results for dual centerlines."),
                                html.P("The tool will guide you through the process of addressing any NULL functional class values, which will need to be omitted. \
                                    Alternatively, if you have local knowledge that most or all of the null or unknown functional class values are usually related to a specific type of street \
                                    (e.g., undesignated residential streets), you may assign them to the functional class that best fits local conditions.")
                            ])
                        )
                    ], start_collapsed=True, className="custom-accordion",),   

                    html.P(),
                    dbc.Accordion([
                        dbc.AccordionItem(
                            title="Crash Data Preparation",
                            children=html.Ul([
                                html.P("The data must be in a point-type shapefile format. \
                                    While uploading shapefiles, the following four files associated with the shapefile must be uploaded in one .zip file: .shp, .shx, .dbf, and .prj. \
                                    The uploaded .zip file cannot exceed 20 MB."),
                                html.P("The tool will guide you through the process of addressing any NULL values in your crash data, which will need to be omitted in most cases (except as outlined above for nulls in crash severity values."),
                                html.P("The shapefile must have each of the following attributes and characteristics (attribute names can be different from those given below): "), 
                                html.Li("Report ID - A unique ID for the crash to ensure one location record per crash. \
                                    If your dataset has multiple records per crash, you may be working with a 'person', 'unit', or 'vehicle' table. \
                                    Please restructure your data or ask your crash data provider for help getting a dataset with one row per crash.", className="ms-4"),
                                html.Li("Crash year - four digit integer format. The tool is designed to use five years of crash data. \
                                    If fewer years are available, analysis can still run with available years, but results may be less reliable.", className="ms-4"),
                                html.Li("Severity - must be mapped to the KABCO scale as follows: 'Fatality (K)', 'Incapacitating Injury (A)', 'Non-Incapacitating Injury (B)', 'Possible Injury (C)', 'Property Damage Only (O)'. \
                                    You may also choose to assign them to 'Omit From Analysis' which will exclude these crashes from the analysis. \
                                    Severity should represent the most severely injured person or worst outcome associated with the crash.", className="ms-4"),
                                html.Li("If your data has crashes with null or unknown severity values: for pedestrian and bicycle crashes, if nulls are a small percentage of total crashes, consider assigning these to Non-Incapacitating Injury (B). \
                                    For motor vehicle crashes missing severities (especially if a large percentage are missing), consider assigning these to Property Damage Only (PDO). \
                                    Higher severity crashes (especially fatal crashes) are less likely to have missing or incomplete data. \
                                    At the same time, missing or incomplete data are still pretty common, and the crashes themselves are statistically rare, \
                                    so making an assumption about severity lets us keep missing or incomplete records in the dataset.", className="ms-4"),
                                html.Li("Crash mode - the mode in your data must be assigned to bicycle, pedestrian, or motor vehicle mode. \
                                    Identification of mode must occur in one column. E.g., if you have separate columns to indicate bicyclist involved or pedestrian involved, \
                                    you will need to restructure your data to use a single mode column before using it with the tool. \
                                    For crashes involving multiple modes, it is most common to assign the entire crash either the mode of the most severely injured person, \
                                    or the mode of the most vulnerable road user (typically in this order: pedestrian, bicycle, motor vehicle). \
                                    You may also choose to assign them to 'Omit From Analysis' which will exclude these crashes from the analysis.", className="ms-4"),
                                html.Li("Crash data is limited to 250,000 crash records. Crash datasets with more than 50,000 records may lead to slower map performance in the app. \
                                    Analysis can still run, and outputs can still be downloaded for use in other GIS tools.", className="ms-4"),
                                html.Li("If crash data contains only fatal crashes (for example, FARS), non-fatal model results and non-fatal crash costs are not calculated.", className="ms-4"),
                                html.P("Properly formatted crash data: "),
                                html.Img(src="/assets/proper_crash_data.png", style={"width": "100%", "maxWidth": "600px"}),
                                html.P("Improperly formatted crash data: "),
                                html.Img(src="/assets/improper_crash_data.png", style={"width": "100%", "maxWidth": "600px"}),
                            ])
                        )
                    ], start_collapsed=True, className="custom-accordion",),
                    html.P(),
                ]),
                html.H6("Data Review:"),
                html.P("After you have uploaded custom data or selected default data, click 'Review Input Data' to check that your data is being properly visualized.\
                    This section will also perform some basic data checks on your data. \
                    If there are any issues with your data, you will get a warning message with guidance on how to fix it.", className="mb-2"),
            ]),

            # 3. Analysis Settings
            html.Li([
                html.Strong("Analysis Settings"),
                html.P("Users are given the option to customize certain parameters for the analyses.",className="mb-2"),
                html.Ul([
                    html.Li(html.Strong("Severity Weights: "), className="ms-6"),
                    html.P("The default severity weights are based on the KABCO scale. \
                        The weights are as follows: Fatality (K) = 3, Incapacitating Injury (A) = 3, Non-Incapacitating Injury (B) = 1, Possible Injury (C) = 0, Property Damage Only (O) = 0. \
                        These weights are used to calculate the crash density estimates along street corridors by weighting crashes according to their severity. \
                        Users have the option to adjust these weights based on local conditions or policy priorities. \
                        They must maintain ordinality (K >= A >= B >= C >= O).", className="ms-6"),
                    html.Li(html.Strong("Window Lengths: "), className="ms-6"),
                    html.P("These lengths control how long the sliding and short windows (step length in sliding windows analysis) are in miles. \
                        Sliding window length must be in increments of 0.05 miles between 0.25 and 2 miles. \
                        Sliding Window to Short Window Length Ratio must be an integer between 1 and 10. \
                        The ratio determines the short window length, which is used as the unit of analysis.", className="ms-6"),
                    html.Li(html.Strong("Crash Costs: "), className="ms-6"),
                    html.P([
                        "These costs represent the estimated societal cost (in USD) associated with crashes of different severities. \
                        The default costs are based on ",
                        html.A(
                            "FHWA's Crash Costs for Highway Safety Analysis (2024)",
                            href="https://highways.dot.gov/sites/fhwa.dot.gov/files/2025-10/CrashCostFactSheet_508_OCT2025.pdf",
                            target="_blank",
                            rel="noopener noreferrer",
                            style={"color": "#0267FD"},
                        ),
                        ". ", 
                        "They must maintain ordinality (K >= A >= B >= C >= O).",
                    ], className="ms-6"),
                    html.Li(html.Strong("Crash Join Distance: "), className="ms-6"),
                    html.P("Crash join distance controls how close crashes must be to a roadway segment to be considered associated with that segment (in feet). \
                        Distance must be between 25 and 250 feet.",className="ms-6"),  
                    html.Li(html.Strong("Number of Streets in Report: "), className="ms-6"),
                    html.P("Number of streets in report controls how many of the highest scoring streets are included in the highest crash corridor map and table. \
                        Value must be an integer between 5 and 20 streets. \
                        This setting does not affect the analysis itself, it is only for the report and dashboard visualization.", className="ms-6"),
                ]),
            ], className="instruction-list"),

            # 4. Run Analysis
            html.Li([
                html.Strong("Run Analysis"),
                html.P("The tool has two types of analysis: Sliding Windows Analysis and the Safer Streets Model. \
                    The sliding windows analysis is a more traditional approach to identifying high crash corridors based on crash history, \
                    while the Safer Streets Model provides modeled risk estimates that can be especially helpful for identifying high-risk corridors that may not have a long crash history but still have risk factors present. ",
                ),
                html.Ul([
                    html.Li(html.Strong("Sliding Windows Analysis: "), className="ms-6"),
                    html.P("Click the 'Run Sliding Windows Analysis' to run the sliding windows analysis. Do not close the browser or refresh while the analysis is running. \
                        Once the sliding windows analysis is complete, click 'Visualize Sliding Windows Analysis Results' to view it on the interactive map. ",
                        className="ms-6"
                    ),  
                    html.Li(html.Strong("Safer Streets Model: "), className="ms-6"),
                    html.P("Click 'Run Safer Streets Model' to run the Bayesian crash-pattern model. Do not close the browser or refresh while the analysis is running. \
                        Click 'Visualize Safer Streets Model Analysis Results' to view the results on the interactive map.",
                    className="ms-6"
                    ),
                ]),
            ],className="instruction-list"),

            # 5. Dashboard
            html.Li([
                html.Strong("Dashboard"),
                html.P("Check the dashboard for a summary of your analysis results. The dashboard includes an interactive map of the highest crash corridors and multiple graphs and tables highlighting different features of your data. "
                    "Compile and download a pdf report of your results or a package including all data associated with your study in a GIS compatible format.")
            ], className="instruction-list"),

            # 6. Map Visualization
            html.Li([
                html.Strong("Map Visualization"),
                html.P("Visualize your study area data and analysis results in the interactive map viewer. \
                    You may select any datasets or analysis results from you study to visualize using the dropdowns.")
            ])
        ], className="instruction-list"),
    ], className="p-2",)
