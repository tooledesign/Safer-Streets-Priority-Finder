from dash import html, dcc
import dash_bootstrap_components as dbc
import dash_leaflet as dl
from ..global_vars import *
from .. import utils

def layout():
    return html.Div(
        [
            html.H2("Map Visualization"),
            html.Ul(
                        [
                            html.Li("Visualize your study area data and analysis results in the map below."),
                            html.Li("If the default basemap contrast is not sufficient for your data, try switching to the basemap using the layers pane in the top left corner of the map."),
                            html.Li("Select the datasets and analysis results to visualize using the dropdowns. The map will update automatically upon selection."),
                            html.Li("The dropdowns are populated with the datasets and results available in your study."),
                            html.Li("Note that the fatality block group layer might have some block groups clipped at the study area boundary.\
                                    For those block groups, the predicted fatalities represent the portion within the study area calculated using area apportionment."),
                            html.Li("Sliding windows results only show segments with non-zero crash score."),
                            html.Li("Safer streets model results only show segments with estimated crash cost greater than the cost of a PDO crash."),
                            html.Li("Safer streets model results uses short windows (length derived from analysis settings) as the unit of analysis. \
                                    Some segments may be shorter than this length, especially at the end of the street corridors."),
                        ]
                    ),
            html.Hr(),
            # add dropdown buttons for selecting crash, roads, fatality model, study area
            html.Div([
                html.H5("Select Inputs:"),
                dbc.Row([
                    dbc.Col([
                        dbc.Label("Choose a crash dataset:"),
                        dcc.Dropdown(
                            id="map-vis-crash-dataset-dropdown",
                            options=[{"label": "No Crashes", "value": "No Crashes"}],  # populated via callback
                            clearable=False,
                            value="No Crashes",
                            style={"zIndex": 2000}
                        ),
                ]),
                    dbc.Col([
                        dbc.Label("Choose a roads dataset:"),
                        dcc.Dropdown(
                            id="map-vis-roads-dataset-dropdown",
                                options=[{"label": "No Roads", "value": "No Roads"}],  # populated via callback
                                clearable=False,
                                value="No Roads",
                                style={"zIndex": 2000}
                        ),
                    ]),
                    dbc.Col([
                        dbc.Label("Choose a fatality block group dataset:"),
                        dcc.Dropdown(
                            id="map-vis-fatality-model-dropdown",
                            options=[
                                {"label": "No Fatality Model", "value": "No Fatality Model"},
                                *[{"label": f"{ml} Fatality Model", "value": f"{m}"} for m, ml in MODE_LABELS.items()]
                            ],
                            value="No Fatality Model",
                            clearable=False,
                            style={"zIndex": 2000}
                        ),
                    ]),
                ]),
            ], className="mb-3", style={"position": "relative", "zIndex": 2000}),
            html.Hr(),
            html.Div([
                utils.map_vis_func.visualization_leaflet_map(),
                dbc.Alert(
                    id="map-vis-large-dataset-notice",
                    color="warning",
                    is_open=False,
                    dismissable=True,
                    style={
                        "position": "absolute",
                        "top": "10px",
                        "left": "50px",
                        "right": "50px",
                        "zIndex": 1000,
                    }
                ),
            ], style={"position": "relative"}),
            dbc.Alert(id="map-vis-crash-truncation-notice", color="info", is_open=False, className="mt-2"),


            # busy modal upon page load
            utils.preset_modals.busy_modal(text="Loading map data...", is_open=True, id_="map-vis-page-load-busy"),
            # busy modal for client-side layer redraws on large crash datasets
            utils.preset_modals.busy_modal(text="Rendering map...", is_open=False, id_="map-vis-update-busy"),

            # error div for when map update fails
            html.Div(id="map-vis-page-error", className="text-danger mt-2"),

            # chain stores: data stores hold layer payloads, signal stores
            # are lightweight booleans that trigger the next callback
            # (avoids sending large data in POST body as Input)
            dcc.Store(id="map-vis-init-store", data=None),
            dcc.Store(id="map-vis-crash-data", data=None),
            dcc.Store(id="map-vis-crash-done", data=None),
            dcc.Store(id="map-vis-swa-data", data=None),
            dcc.Store(id="map-vis-swa-done", data=None),
            dcc.Store(id="map-vis-ssm-data", data=None),
            dcc.Store(id="map-vis-ssm-done", data=None),
            dcc.Store(id="map-vis-fatality-data", data=None),

            # merged stores consumed by clientside dropdown-switch callback
            dcc.Store(id="map-vis-available-layers-store", data=None),
            dcc.Store(id="map-vis-rendered-legends-store", data=None),
            dcc.Store(id="map-vis-crash-meta-store", data=None),

        ],
        className="p-2"
    )
