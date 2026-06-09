from dash import html, dcc
import dash_leaflet as dl
import dash_bootstrap_components as dbc
from ..global_vars import *
from .. import utils

def layout():
    # Navbar to toggle between analyses
    navbar = dbc.Card(
        [
            dbc.CardBody(
                [
                    dbc.Nav(
                        [
                            dbc.NavItem(dbc.NavLink("Sliding Windows Analysis", id="nav-sliding-windows", active=True, className="px-3 border border-secondary")),
                            # html.Div(style={"width": "1rem"}),
                            dbc.NavItem(dbc.NavLink("Safer Streets Model", id="nav-safer-streets-model", active=False, className="px-3 border border-secondary")),
                        ],
                        pills=True,
                        className="mb-3",
                    ),
                    html.P("Click on the navigation buttons above to switch between the sliding windows and safer streets model analysis pages."),
                    dcc.Store(id="active-analysis-tab", data="sliding-windows"),
                    dcc.Store(id="page-load-trigger", data=0),
                ]
            )
        ],
        className="mb-3"
    )
    
    # Sliding Windows Analysis Section
    sliding_windows_section = _analysis_section_div(analysis_type="sliding-windows")
    
    # Safer Streets Model Section
    safer_streets_section = _analysis_section_div(analysis_type="safer-streets-model")

    # dummy model map section just to have the right ids to avoid callback errors
    dummy_model_map_section = html.Div(
        id="dummy-model-map-container",
        children=utils.analysis.placeholder_analysis_results_map(analysis_type="safer-streets-model"),
        style={"display": "none"}
    )

    return html.Div([navbar, sliding_windows_section, safer_streets_section, dummy_model_map_section], className="p-2")

def _analysis_section_div(analysis_type):
    "Generate analysis section area layouts"
    # analysis must be either "sliding-windows" or "safer-streets-model"
    if analysis_type not in ["sliding-windows", "safer-streets-model"]:
        raise ValueError("analysis_type must be either 'sliding-windows' or 'safer-streets-model'")

    analysis_type_readable = analysis_type.replace("-", " ").title()
    # unordered list items for card body
    card_body_list_items = {
        "sliding-windows": [
            "Analyze crash patterns using sliding windows methodology.",
            "This analysis divides your study area into overlapping windows and calculates crash statistics for each window.",
            "Clicking the run analysis button below will overwrite existing results. \
            If you have previously run this analysis, you can load existing results on the map using the visualization button.",
            "Depending on the size of your study area and the amount of data, this analysis may take a few minutes to complete. \
            For a large study area we tested with ~44,000 miles of roadway and ~200,000 crash records, the analysis took approximately 2 minutes. \
            Please do not navigate away from this page while the analysis is running."
        ],
        "safer-streets-model": [
            "Run the safer streets model to identify high-risk locations.",
            "If you crash data has Fatal crashes only (e.g using FARS data), the model will not estimate crash risk for non-fatal crashes. If you would like to estimate risk for non-fatal crashes, you can use the crash severity ratios for your region to come up with a simple estimate.",
            "Depending on the size of your study area and the amount of data, this analysis may take a few minutes to complete. \
             Please do not navigate away from this page while the analysis is running.",
            "Clicking the run analysis button below will overwrite existing results. \
             If you have previously run this analysis, you can load existing results on the map using the visualization button.",
        ]
    }

    # map card body text
    map_card_body_text = {
        "sliding-windows": "This map depicts severity-weighted pedestrian/bicycle/motor vehicles crash scores using sliding windows analysis. \
                           Crash scores are calculates using the severity weights provided along with crash severity and frequency. \
                           Higher scores indicates more severe crash history and/or frequency for a given mode of travel and these segments may form the basis of High Injury Network (HIN) identification. \
                           Only segments with a non-zero crash score are shown. \
                           The map is not populated upon page load, please use the visualization button to load results. \
                           Additional information will pop-up if you click on a roadway segment score in the map. \
                           If the color contrast of the map layers is not sufficient with your data, \
                           try switching to a different basemap using the layers pane in the top right corner of the map.",
        "safer-streets-model": f"This map depicts your model results for pedestrian/bicycle/motor vehicle crashes in terms of crash cost. \
                                The analysis uses short windows (length derived from analysis settings) as the unit of analysis. Some segments may be shorter than this length, especially at the end of the street corridors. \
                                Only segments with an estimated cost of greater than the cost of a PDO crash are shown. \
                                If a mode does not have any segments with crash costs greater than the cost of a PDO crash, that mode will not be shown on the map. \
                                The map is not populated upon page load, please use the visualization button to load results. \
                                Additional information will pop-up if you click on a roadway segment score in the map. \
                                If the color contrast of the map layers is not sufficient with your data, \
                                try switching to a different basemap using the layers pane in the top right corner of the map."
    }

    return html.Div(
        id=f"{analysis_type}-section",
        children=[
            dbc.Card(
                [
                    dbc.CardHeader([html.H5(analysis_type_readable + " Analysis")]),
                    dbc.CardBody(
                        [
                            html.Ul([
                                html.Li(item) for item in card_body_list_items[analysis_type]
                            ]),
                            dbc.Button(f"Run {analysis_type_readable} Analysis", id=f"run-{analysis_type}-btn", color="secondary", className="me-2"),
                            html.Br(), html.Br(),
                            html.Div(id=f"{analysis_type}-results-message", children=[]),
                        ]
                    )
                ],
            ),
            dbc.Card(
                [
                    dbc.CardBody(
                        [
                            html.P(map_card_body_text[analysis_type]),
                            dbc.Button(f"Visualize {analysis_type_readable} Analysis Results",
                                id=f"map-{analysis_type}-btn", color="secondary", className="me-2",
                                disabled=True
                            ),
                            html.Div(id=f"{analysis_type}-map-results-message", className="mt-2"),
                            html.Hr(),
                            html.Div(
                                id=f"{analysis_type}-map-container",
                                children=(
                                    utils.analysis.placeholder_analysis_results_map(analysis_type=analysis_type) if analysis_type == "sliding-windows" 
                                    else 
                                    [] # empty children because map  is not rendering properly on a hidden div
                                ),
                                style={"display": "block"}, # hidden until button clicked
                            ),
                            dcc.Store(id=f"{analysis_type}-review-store", data=None),
                            dcc.Store(id=f"{analysis_type}-legend-data", data=None),
                            dcc.Store(id=f"{analysis_type}-geojson-store", data=None),
                            dcc.Store(id=f"{analysis_type}-mode-style-config", data=None),
                            utils.preset_modals.busy_modal(text=f"Getting {analysis_type.replace('-', ' ')} results...", is_open=False, id_=f"get-{analysis_type}-results-busy"),
                        ]
                    ),
                    utils.preset_modals.busy_modal(text=f"Running {analysis_type.replace('-', ' ')} analysis...", is_open=False, id_=f"run-{analysis_type}-busy"),
                ],
                className="mt-3"
            )
        ],
        style={"display": "block"} if analysis_type == "sliding-windows" else {"display": "none"}
    )
