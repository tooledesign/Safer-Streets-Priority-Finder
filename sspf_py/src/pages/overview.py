from dash import html
import dash_bootstrap_components as dbc


def layout():
    return dbc.Container(
        [
            html.H2("Overview", className="mb-3"),
            html.P(
                "The Safer Streets Priority Finder (SSPF) is a free, interactive, open-source tool available in the United States "
                "that is intended to help transportation practitioners identify a roadway network that is similar to a High Injury "
                "Network for bicyclists, pedestrians, and motor vehicles. The Safer Streets Model goes further than a typical High Injury "
                "Network by not only taking into consideration areas where a disproportionate share of fatal and serious injury "
                "crashes have already occurred, but also areas that have other kinds of factors present that may indicate potential "
                "risk of crashes."
            ),
            html.Div(
                [
                    html.Img(
                        src="/assets/Safer Streets Model_v4.png",
                        alt="Flowchart illustrating the Safer Streets Modeling process. "
                            "Crash and roadway data combine to produce observed crash totals, "
                            "and roadway data, the block group fatality risk model, functional class fatal crash rates, "
                            "and severity ratios combine to produce an initial crash risk estimate. "
                            "Lastly, observed crash totals and the initial crash risk estimate are combined and "
                            "adjusted using an empirical Bayes method to estimate crash risk on a road segment. "
                            "A legend to the right of the flowchart shows a red box representing user input or "
                            "tool defaults (including crash data and roadway data), a light gray box representing precalculated inputs "
                            "(including block group fatality risk model, functional class fatal crash rates, and severity ratios), "
                            "a dark gray box representing intermediate computed data (including observed crash totals and the initial crash risk estimate), "
                            "and a green box representing the final output (empirical bayes adjustment and segment crash risk).",
                        className="img-fluid rounded",
                        style={"maxWidth": "640px", "margin": "0", "display": "block"},
                    ),
                    html.Img(
                        src="/assets/Sliding Window_v4.png",
                        alt="Flowchart illustrating the Sliding Window Analysis process. "
                            "Crash data and roadway data are combined by spatially joining crashes to roads using sliding windows. "
                            "The process produces crash density scores by mode. The diagram on the right shows a simple stick-figure "
                            "aerial map that demonstrates the sliding windows. Crashes are marked as red dots along the roadway, "
                            "with overlapping sliding-window segments moving in 1/10-mile steps across a 1/2-mile window. "
                            "A legend to the right of the flowchart shows a red box representing user input or tool defaults "
                            "(including crash data and roadway data), a dark gray box representing intermediate computed data "
                            "(including crashes spatially joined to roads), and a green box representing the final output "
                            "(including crash density scores by mode).",
                        className="img-fluid rounded",
                        style={"maxWidth": "640px", "margin": "0", "display": "block"},
                    ),
                    html.Img(
                        src="/assets/Network Output_v4.png",
                        alt="Diagram showing the Network Output of the safer streets model and sliding window analysis. "
                            "The diagram includes a simplified street grid graphic with map pins. "
                            "Connected to the map pins are red callout boxes describing the various network outputs, "
                            "which include crash density based on historic crash data, estimating risk on road segments "
                            "using sliding windows, estimating risk in areas without crash history, expressing risk as a "
                            "cost linked to planning, and being easy to use with readily available data.",
                        className="img-fluid rounded",
                        style={"maxWidth": "640px", "margin": "0", "display": "block"},
                    ),
                ],
                style={"textAlign": "center", "display": "flex", "flexDirection": "column", "alignItems": "center", "gap": "0px"},
            ),
            html.P(
                "The current version (v2.0) of the SSPF is an update to the original SSPF, which was developed in 2021."
                "The current version includes updates to the underlying data and modeling framework, as well as a new interactive interface. "
                "The SSPF is intended to be used by transportation practitioners to help identify areas of potential risk across the roadway network and prioritize safety investments. "
                "The tool is not intended to replace local knowledge or other sources of information, but rather to complement them with data-driven insights."
            ),
            # What's new in SSPF v2.0
            html.H4("What's new in SSPF v2.0?"),
            html.P(
                [
                    "The first version of SSPF is an expansion of another prior effort—the Pedestrian Fatality Risk Pilot beta tool, "
                    f"developed by the USDOT (i.e., Pilot Tool - ",
                    html.A(
                        "Mansfield, et al. (2018).",
                        href="https://www.transportation.gov/sites/dot.gov/files/docs/mission/office-policy/transportation-policy/328686/effects-roadway-and-built-environment-characteristics-pedestrian-fatality-risk-mansfield-et-al.pdf",
                        target="_blank",
                        rel="noopener noreferrer",
                        style={"color": "#0267FD"},
                    ),
                    " The Pilot Tool is based on a tract-level statistical model of pedestrian fatalities for the entire United States.",
                ],
            ),
            html.P(
                [
                    "SSPF 2.0 has the following updates:",
                    html.Ul(
                        [
                            html.Li(
                                "Previous tract level model was updated to newly developed block group level models. \
                                    These new fatal crash models include bicyclist and motor vehicle modes in addition to the pedestrian mode."
                            ),
                            html.Li(
                                [
                                    "Users now have the ability to customize the following analysis parameters:",
                                    html.Ul(
                                        [
                                            html.Li(
                                                "Sliding window size"
                                            ),
                                            html.Li(
                                                "Short Window Length / Step Length"
                                            ),
                                            html.Li(
                                                "Crash severity weights for sliding windows analysis"
                                            ),
                                            html.Li(
                                                "Crash cost values for Safer Streets Model outputs"
                                            ),
                                            html.Li(
                                                "Crash join distance for assigning crashes to road segments"
                                            ),
                                            html.Li(
                                                "Number of streets to include in the output reports"
                                            ),
                                        ]
                                    ),
                                ]
                            ),
                            html.Li(
                                "Additional input data checks and validations."
                            ),
                            html.Li(
                                "Updated model framework that aligns with the latest research and data availability."
                            ),
                            html.Li(
                                "Performance improvements for the model which can run the analysis in minutes instead of hours needed in version 1.0."
                            ),
                        ]
                    ),
                ]
            ),
            html.H4("Block Group Level Fatal Crash Models"),
            html.P(
                "The previous tract-level model was developed for pedestrians only and applied to the bicyclist analysis post-hoc. \
                SSPF 2.0 is built on a new suite of models that cover all modes at finer geographic scale (Census Block Group instead of Tract). \
                Separate models have been developed for fatal crashes involving pedestrians, bicyclists, and (only) motorists. \
                Models are tailored across the urban-rural gradient, using the National Walkability Index as a proxy for level of urbanization. \
                These new models allow SSPF 2.0 to be applied for all modes, including motorist-only crashes."
            ),
            html.H4("Modeling Framework"),
            html.P(
                """
                The Safer Streets Model uses a three-stage crash risk estimation pipeline. 
                First, block group-level fatal crash estimates are disaggregated to individual roadway segments 
                based on each segment's functional class and effective mileage within the block group, 
                using national fatal crash rates as a baseline. 
                Second, the fatal estimates are expanded to all injury severity levels (K, A, B, C, and O on the KABCO scale) 
                by applying a expected severity ratio based on the geographic region of the study area. 
                Finally, the modeled estimates are refined through an Empirical Bayes update that blends 
                the predicted crash counts with observed local crash history, producing adjusted 
                segment-level estimates that leverage both the national model and local data. 
                Local observed crashes are distributed across nearby roadway segments to account for 
                inaccuracies in reported crash locations
                """,
                className="mb-4",
            ),
            html.H4("Model Validation and Calibration"),
            html.P(
                """
                The Safer Streets Model was calibrated and validated in three study areas representing urban, 
                suburban, and rural contexts: New Orleans, LA; Albany, OR; and Knox County, OH. For each location, 
                two consecutive five-year periods of crash data were used: one to refine model output through 
                Empirical Bayes updating and the other to validate model performance. Performance was evaluated 
                using Normalized Discounted Cumulative Gain (NDCG) and Percent Crash Capture, and compared with an 
                observation-based approach that ranks segments solely by historical crash counts. Calibration was 
                conducted through a grid search to find the optimal Empirical Bayes weighting.
                """,
                className="mb-4",
            ),
            dbc.Row(
                [
                    # DATA SOURCES
                    dbc.Col(
                        dbc.Card(
                            [
                                dbc.CardHeader(
                                    html.H5("Data Sources / Inputs", className="mb-0")
                                ),
                                dbc.CardBody(
                                    [
                                        html.P(
                                            [
                                                html.Strong("Study Area Boundary: "),
                                                "User input, or tool built-in Census County boundary.",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.P(
                                            [
                                                html.Strong("Crash Data: "),
                                                "User input, or tool built-in data from the Fatality Analysis Reporting System (FARS). "
                                                "FARS includes only fatal crashes, which may limit outputs—especially crash density results. "
                                                "Local crash data are strongly recommended when available.",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.P(
                                            [
                                                html.Strong("Road Network Data: "),
                                                "User input, or tool built-in data from OpenStreetMap (OSM).",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.P(
                                            [
                                                html.Strong(
                                                    "Average Fatal Crash Rates: "
                                                ),
                                                "Fatal crash rates are based on 2018-2022 FARS data "
                                                "and FHWA Highway Statistics mileage by functional class for each state.",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.P(
                                            [
                                                html.Strong(
                                                    "Crash Severity Ratios: "
                                                ),
                                                "State- and mode-specific severity ratios are computed from publicly available crash data for CA, FL, GA, MA, MN, NY, OH, and PA. "
                                                "For states without available data, ratios are gap-filled using Census region averages, with national averages applied to territories.",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.P(
                                            [
                                                html.Strong(
                                                    "Block Group Fatality Risk Model: "
                                                ),
                                                "Contains estimated fatal crashes for each block group in the United States, by mode (pedestrian, bicycle, motor vehicle).",
                                            ],
                                            className="mb-0",
                                        ),
                                    ]
                                ),
                            ],
                            className="h-100 shadow-sm",
                        ),
                    ),
                    # OUTPUTS
                    dbc.Col(
                        dbc.Card(
                            [
                                dbc.CardHeader(html.H5("Outputs", className="mb-0")),
                                dbc.CardBody(
                                    [
                                        html.H6("Sliding Windows Analysis Output"),
                                        html.P(
                                            [
                                                "This analysis creates crash density estimates by mode along street corridors throughout the study area, \
                                                weighted by crash severity. This output can be used independently as a traditional High Injury Network based on crash history. "
                                            ],
                                            className="mb-3",
                                        ),
                                        html.H6("Safer Streets Model Output"),
                                        html.P(
                                            "Safer Streets Model outputs provide modeled risk estimates across the roadway network. \
                                            The analysis combines block group level fatal crash estimates with roadway network data to estimate risk. \
                                            This is then adjusted using observed crash data using an Empirical Bayes approach to come up with more accurate estimates. \
                                            The output of this is an estimated crash cost for each mode along the network. ",
                                            className="mb-0",
                                        ),
                                    ]
                                ),
                            ],
                            className="h-100 shadow-sm",
                        ),
                    ),
                ],
                className="g-4 mb-4",
            ),
            dbc.Row(
                [
                    # CRASH COSTS
                    dbc.Col(
                        dbc.Card(
                            [
                                dbc.CardHeader(
                                    html.H5("Crash Costs", className="mb-0")
                                ),
                                dbc.CardBody(
                                    [
                                        html.P(
                                            [
                                                "Standard crash cost rates are built into the tool to reflect the full societal costs of crashes. ",
                                                "This tool uses crash costs provided by ",
                                                html.A(
                                                    "FHWA's Crash Costs for Highway Safety Analysis (2024)",
                                                    href="https://highways.dot.gov/sites/fhwa.dot.gov/files/2025-10/CrashCostFactSheet_508_OCT2025.pdf",
                                                    target="_blank",
                                                    rel="noopener noreferrer",
                                                    style={"color": "#0267FD"},
                                                ),
                                                ". Default costs are national-level and not localized to individual states.",
                                            ],
                                            className="mb-3",
                                        ),
                                        html.Div(
                                            html.Table(
                                                [
                                                    html.Thead(
                                                        html.Tr(
                                                            [
                                                                html.Th("KABCO Severity Category"),
                                                                html.Th("Economic Crash Unit Costs, 2024 Dollars"),
                                                                html.Th("QALY Crash Unit Costs, 2024 Dollars"),
                                                                html.Th("Comprehensive Unit Costs, 2024 Dollars"),
                                                            ]
                                                        )
                                                    ),
                                                    html.Tbody(
                                                        [
                                                            html.Tr(
                                                                [
                                                                    html.Td("K"),
                                                                    html.Td("$2,238,500"),
                                                                    html.Td("$13,749,500"),
                                                                    html.Td("$15,988,000"),
                                                                ]
                                                            ),
                                                            html.Tr(
                                                                [
                                                                    html.Td("A"),
                                                                    html.Td("$272,700"),
                                                                    html.Td("$1,432,400"),
                                                                    html.Td("$1,705,100"),
                                                                ]
                                                            ),
                                                            html.Tr(
                                                                [
                                                                    html.Td("B"),
                                                                    html.Td("$80,800"),
                                                                    html.Td("$303,200"),
                                                                    html.Td("$384,000"),
                                                                ]
                                                            ),
                                                            html.Tr(
                                                                [
                                                                    html.Td("C"),
                                                                    html.Td("$53,000"),
                                                                    html.Td("$151,600"),
                                                                    html.Td("$204,600"),
                                                                ]
                                                            ),
                                                            html.Tr(
                                                                [
                                                                    html.Td("O"),
                                                                    html.Td("$18,100"),
                                                                    html.Td("$0"),
                                                                    html.Td("$18,100"),
                                                                ]
                                                            ),
                                                        ]
                                                    ),
                                                ],
                                                className="table table-striped table-sm mb-0",
                                            ),
                                            className="table-responsive mb-3",
                                        ),
                                        html.P(
                                            (
                                                "Users may optionally customize crash cost values using local data to align with their project costs over a five-year horizon."
                                            ),
                                            className="mb-0",
                                        ),
                                    ]
                                ),
                            ],
                            className="h-100 shadow-sm",
                        ),
                        md=8,
                    ),
                    # TOOL INFRASTRUCTURE
                    dbc.Col(
                        dbc.Card(
                            [
                                dbc.CardHeader(
                                    html.H5("Tool Infrastructure", className="mb-0")
                                ),
                                dbc.CardBody(
                                    [
                                        html.H6("Python"),
                                        html.P(
                                            "The tool is primarily built using Python for the web interface, interacting with the database, and data processing.",
                                            className="mb-3",
                                        ),
                                        html.H6("PostgreSQL"),
                                        html.P(
                                            "All tool data are stored in a PostgreSQL database hosted on AWS RDS. Some of the data processing is also done using PostGIS functions within the database.",
                                            className="mb-0",
                                        ),
                                        html.H6("Ubuntu OS"),
                                        html.P(
                                            "The tool is tested and built on a Ubuntu 24.04 operating system. The tool uses linux shell scripts for certain processes.",
                                            className="mb-3",
                                        ),
                                        html.H6("Amazon Web Services (AWS)"),
                                        html.P(
                                            "The tool is hosted on an EC2 instance on a Ubuntu 24.04 server.",
                                            className="mb-3",
                                        ),
                                        
                                    ]
                                ),
                            ],
                            className="h-100 shadow-sm",
                        ),
                        md=4,
                    ),
                ],
                className="g-4",
            ),
        ],
        fluid=True,
        className="py-3",
    )
