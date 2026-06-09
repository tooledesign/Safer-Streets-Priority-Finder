from dash import html
import dash_bootstrap_components as dbc
from ..global_vars import GIT_REPO_URL

def _case_study_card(title, context, description):
    """Helper function to create a case study card with standardized structure.
    
    Generates file paths automatically from title and context using the format:
    - {title}_{context}_inputs.zip
    - {title}_{context}_output_gis.zip
    - {title}_{context}_report.pdf
    """
    # Normalize title and context for filenames
    title_normalized = title.replace(" ", "_").lower()
    context_normalized = context.replace(" ", "_").lower()
    
    # Generate file paths
    input_link = f"/assets/case_studies/{title_normalized}_{context_normalized}_inputs.zip"
    output_gis_link = f"/assets/case_studies/{title_normalized}_{context_normalized}_output_gis.zip"
    output_pdf_link = f"/assets/case_studies/{title_normalized}_{context_normalized}_report.pdf"
    
    return dbc.Card(
        [
            dbc.CardHeader(html.H5(f"{title} ({context})", className="mb-0")),
            dbc.CardBody(
                [
                    html.P(description, className="mb-3"),
                    html.Div(
                        [
                            html.Strong("Case Study Data:"),
                            html.Ul(
                                [
                                    html.Li(
                                        html.A(
                                            "Input Data (GIS ZIP Archive)",
                                            href=input_link,
                                            download=input_link.split("/")[-1],
                                            target="_self",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        )
                                    ),
                                    html.Li(
                                        html.A(
                                            "Output Data (GIS ZIP Archive)",
                                            href=output_gis_link,
                                            download=output_gis_link.split("/")[-1],
                                            target="_self",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        )
                                    ),
                                    html.Li(
                                        html.A(
                                            "Output Report (PDF)",
                                            href=output_pdf_link,
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        )
                                    ),
                                ]
                            ),
                        ]
                    ),
                ]
            ),
        ],
        className="mb-4 shadow-sm",
    )


def layout():
    return dbc.Container(
        [
            html.H2("Resources and Case Studies", className="mb-3"),
            html.P(
                "Explore case studies demonstrating the Safer Streets Priority Finder in different geographic contexts, "
                "and access documentation and reports supporting the SSPF v2.0 modeling framework.",
                className="mb-4",
            ),
            # Case Studies Section
            html.H3("Case Studies", className="mt-5 mb-3"),
            html.P(
                "The following case studies demonstrate how SSPF was applied across urban, suburban, and rural contexts "
                "to identify high injury and high risk roadway segments and support safety investment decisions.",
                className="mb-3",
            ),
            _case_study_card(
                title="New Orleans",
                context="Urban",
                description="The New Orleans case study applies SSPF to the City of New Orleans, Louisiana. "
                "The analysis identifies high injury and high risk corridors for pedestrians, bicyclists, and motor vehicles "
                "using local crash data and the SSPF modeling framework. Results were used to help prioritize safety investments "
                "across the city's roadway network. "
                "The input GIS archive contains 10-years of crash data divided into two separate 5-year periods. "
                "The two separate crash datasets are used in the testing and validation of the model. "
                "There is also a boundary shapefile for the study area included in the input GIS archive. "
                "There is not roads shapefile included as the studies used the built-in OSM road data for the analysis. "
                "The output GIS layers and PDF report are based on the most recent 5-year period crash data and default analysis parameters.",
            ),
            _case_study_card(
                title="Albany",
                context="Suburban",
                description="The Albany, Oregon case study demonstrates SSPF application in a suburban context. "
                "The analysis surfaces high-risk segments using a combination of local and regional crash data, "
                "identifying opportunities for targeted safety improvements in this mid-sized community."
                "The input GIS archive contains 10-years of crash data divided into two separate 5-year periods. "
                "The two separate crash datasets are used in the testing and validation of the model. "
                "There is also a boundary shapefile for the study area included in the input GIS archive. "
                "There is not roads shapefile included as the studies used the built-in OSM road data for the analysis. "
                "The output GIS layers and PDF report are based on the most recent 5-year period crash data and default analysis parameters.",
            ),
            _case_study_card(
                title="Knox County",
                context="Rural",
                description="The Knox County, Ohio case study explores SSPF application in a rural context. "
                "The analysis leverages FARS data and the block group fatality risk model to estimate crash risk "
                "across diverse rural road types, supporting data-driven safety planning decisions."
                "The input GIS archive contains 10-years of crash data divided into two separate 5-year periods. "
                "The two separate crash datasets are used in the testing and validation of the model. "
                "There is also a boundary shapefile for the study area included in the input GIS archive. "
                "There is not roads shapefile included as the studies used the built-in OSM road data for the analysis. "
                "The output GIS layers and PDF report are based on the most recent 5-year period crash data and default analysis parameters.",
            ),
            # Static Tool Data Section
            html.H3("Static Tool Data", className="mt-5 mb-3"),
            html.P(
                "Download static datasets and reference sources used by the SSPF tool.",
                className="mb-3",
            ),
            dbc.Card(
                [
                    dbc.CardHeader(html.H5("Static Inputs and Reference Sources", className="mb-0")),
                    dbc.CardBody(
                        [
                            html.Ul(
                                [
                                    html.Li(
                                        html.A(
                                            "Block Group Fatal Crash Estimates (GPKG)",
                                            href="https://sspf-static-files.s3.us-east-1.amazonaws.com/block_group_fatals_model_results.zip",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            "FARS Crashes (GPKG)",
                                            href="https://sspf-static-files.s3.us-east-1.amazonaws.com/fars_crashes.zip",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        [
                                            "OSM Centerlines uploaded via ",
                                            html.A(
                                                "this script",
                                                href=f"{GIT_REPO_URL}/00_prep/06_load_osm_roads.sh",
                                                target="_blank",
                                                rel="noopener noreferrer",
                                                style={"color": "#0267FD"},
                                            )
                                        ],
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            "Fatal Crash Rates by Mode and State (CSV)",
                                            href=f"/assets/default_fatal_rates_by_state.csv",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            "Severity Ratio by Mode and State (CSV)",
                                            href=f"/assets/state_severity_ratios.csv",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            [
                                                "US Counties and State Boundaries uploaded via ",
                                                html.A(
                                                    "this script",
                                                    href=f"{GIT_REPO_URL}/00_prep/08_load_counties_and_state.sh",
                                                    target="_blank",
                                                    rel="noopener noreferrer",
                                                    style={"color": "#0267FD"},
                                                )
                                            ],
                                        ),
                                        className="mb-2",
                                    ),
                                ]
                            ),
                        ]
                    ),
                ],
                className="mb-4 shadow-sm",
            ),
            # Resources Section
            html.H3("Project Documentation and Reports", className="mt-5 mb-3"),
            html.P(
                "Access key reports and technical documentation supporting the Safer Streets Priority Finder v2.0.",
                className="mb-3",
            ),
            dbc.Card(
                [
                    dbc.CardHeader(html.H5("Reports and Methodology", className="mb-0")),
                    dbc.CardBody(
                        [
                            html.Ul(
                                [
                                    html.Li(
                                        html.Span(
                                            "SSPF v2.0 Report [PLACEHOLDER - link will be updated shortly]",
                                            style={"color": "#6c757d"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.Span(
                                            "Block Group Fatality Model Report [PLACEHOLDER - link will be updated shortly]",
                                            style={"color": "#6c757d"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            "Safer Streets Model Process Description",
                                            href="/assets/documents/Bayesian Model Updates Memo - v2.pdf",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        html.A(
                                            "Validation and Calibration Memo",
                                            href="/assets/documents/Task_4_Model_Validation_Methodology_and_Results.pdf",
                                            target="_blank",
                                            rel="noopener noreferrer",
                                            style={"color": "#0267FD"},
                                        ),
                                        className="mb-2",
                                    ),
                                    html.Li(
                                        [
                                            html.Span("SSPF Version 1.0 Resources:"),
                                            html.Ul(
                                                [
                                                    html.Li(
                                                        html.A(
                                                            "SSPF Version 1.0 - Report",
                                                            href="/assets/documents/SSPF v1.0 final_report.pdf",
                                                            target="_blank",
                                                            rel="noopener noreferrer",
                                                            style={"color": "#0267FD"},
                                                        ),
                                                        className="mb-2",
                                                    ),
                                                    html.Li(
                                                        html.A(
                                                            "SSPF Version 1.0 - Research Article",
                                                            href="https://doi.org/10.1177/03611981231189734",
                                                            target="_blank",
                                                            rel="noopener noreferrer",
                                                            style={"color": "#0267FD"},
                                                        ),
                                                        className="mb-2",
                                                    ),
                                                ]
                                            ),
                                        ],
                                        className="mb-2",
                                    ),
                                ]
                            ),
                        ]
                    ),
                ],
                className="mb-4 shadow-sm",
            ),
        ],
        fluid=True,
        className="py-3",
    )
