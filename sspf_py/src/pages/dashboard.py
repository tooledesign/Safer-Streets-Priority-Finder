from dash import html, dcc
import dash_bootstrap_components as dbc
import dash_leaflet as dl
from ..global_vars import *
from .. import utils

def layout():
    return html.Div(
        children=[
            utils.preset_modals.busy_modal(text="Reading dashboard data...", is_open=True, id_="dashboard-busy"),
            dbc.Card(
                [
                    dbc.CardHeader([html.H4("Exploratory Data Dashboard")]),
                    dbc.CardBody(
                        [
                            html.Ul([
                                html.Li("View summaries and visualizations of your crash and roadway data."),
                                html.Li("Crashes and roadways are clipped to your study area.\
                                   Crashes are also limited to the latest five years of data available in your dataset."),
                                html.Li("Compile or download a PDF summary report of your study area and analysis results. \
                                        If you have a previously compiled report, you can download it directly by clicking the 'Download PDF Report' button.\
                                        If you have not yet compiled a report, or re-run your analysis, you can compile a new report by clicking the 'Compile PDF Report' button."),
                                html.Li("Compile or download the data (input and output GIS layers, tables, etc) associated with your study.\
                                        The GIS layers will be compiled as a GeoPackage file which can be opened in any GIS software.\
                                        If you have a previously compiled data package, you can download it directly by clicking the 'Download Study Data' button.\
                                        If you have not yet compiled the data package, or re-run your analysis, you can compile a new data package by clicking the 'Compile Study Data' button."),
                                html.Li("The application periodically purges previously compiled reports and data packages to save storage space. "
                                        "If you attempt to download a report or data package that has been purged, you will receive a message to compile a new one."),
                            ]),
                            html.Hr(),
                            dbc.Row(
                                [
                                    dbc.Col(
                                        dbc.Card(
                                            [
                                                dbc.CardHeader(html.H6("PDF Report", className="mb-0")),
                                                dbc.CardBody(
                                                    [
                                                        html.P("Compile a fresh PDF report, or download the most recently compiled report.\
                                                               If you have not compiled a report yet, please do so before attempting to download.", className="mb-3"),
                                                        dbc.ButtonGroup(
                                                            [
                                                                dbc.Button("Compile PDF Report", id="dashboard-compile-report-btn", color="primary"),
                                                                dbc.Button("Download PDF Report", id="dashboard-download-report-btn", color="outline-primary"),
                                                            ]
                                                        ),
                                                        utils.preset_modals.busy_modal(text="Compiling PDF report...", is_open=False, id_="dashboard-compile-report-busy"),
                                                        dcc.Store(id="dashboard-pdf-report-url-store"),
                                                        html.Div(id="dashboard-pdf-dl-dummy", style={"display": "none"}),
                                                        html.Div(id="dashboard-pdf-report-output-div", className="mt-2"),
                                                    ]
                                                ),
                                            ],
                                            className="h-100"
                                        ),
                                        md=6,
                                        className="mb-3",
                                    ),
                                    dbc.Col(
                                        dbc.Card(
                                            [
                                                dbc.CardHeader(html.H6("Study Data Package", className="mb-0")),
                                                dbc.CardBody(
                                                    [
                                                        html.P("Compile a full study data export package, or download the most recently compiled package.\
                                                               If you have not compiled a package yet, please do so before attempting to download.", className="mb-3"),
                                                        dbc.ButtonGroup(
                                                            [
                                                                dbc.Button("Compile Study Data", id="dashboard-compile-study-data-btn", color="primary"),
                                                                dbc.Button("Download Study Data", id="dashboard-download-study-data-btn", color="outline-primary"),
                                                            ]
                                                        ),
                                                        utils.preset_modals.busy_modal(text="Compiling Study Data...", is_open=False, id_="dashboard-compile-study-data-busy"),
                                                        dcc.Store(id="dashboard-study-data-url-store"),
                                                        html.Div(id="dashboard-study-data-dl-dummy", style={"display": "none"}),
                                                        html.Div(id="dashboard-study-data-output-div", className="mt-2"),
                                                    ]
                                                ),
                                            ],
                                            className="h-100"
                                        ),
                                        md=6,
                                        className="mb-3",
                                    ),
                                ],
                                className="g-3",
                            ),
                        ]
                    )
                ],
            ),
            html.Hr(),
            html.Div(id="dashboard-content",
                children=[
                    # crashes by mode
                    _dashboard_plot_card(id_prefix="dashboard-crashes-by-mode", 
                                         title="Crashes by Mode", 
                                         filter_title="Select Severity:"),
                    html.Br(),
                    # crashes by severity
                    _dashboard_plot_card(id_prefix="dashboard-crashes-by-severity", 
                                         title="Crashes by Severity", 
                                         filter_title="Select Mode:"),
                    html.Br(),
                    # crashes by mode and severity (stacked bar for percentage)
                    _dashboard_plot_card(id_prefix="dashboard-crashes-by-mode-severity", 
                                         title="Crashes by Mode and Severity", 
                                         exclude_filter=True),
                    html.Br(),
                    # crashes by fclass
                    _dashboard_plot_card(id_prefix="dashboard-crashes-by-fclass", 
                                         title="Crashes by Functional Class", 
                                         filter_title="Select Mode:",
                                         opener_text=html.P("Note: Crash counts are summarized by the functional class of the segment closest to the crash. \
                                            This method avoids double counting crashes, but it also means that results are subject to the crash location. \
                                            Crashes in reality may actually be associated with intersections and related to all four legs, not just the closest segment.", 
                                            style={"fontSize": "1rem", "fontStyle": "italic"})),
                    html.Br(),
                    # crashes by year
                    _dashboard_plot_card(id_prefix="dashboard-crashes-by-year",
                                         title="Crashes by Year",
                                         filter_title="Select Mode:",
                                         exclude_slider=True),
                    dcc.Store(id="dashboard-crashes-summary-df-store"),
                    html.Br(),
                    # observed vs modeled crashes
                    _dashboard_plot_card(id_prefix="dashboard-obs-vs-est",
                                         title="Observed vs. Modeled Crashes",
                                         filter_title="Select Mode:",
                                         exclude_slider=True),
                    html.Br(),
                    
                    # Top crash density corridors and map
                    dbc.Card([
                        dbc.CardHeader([
                            dbc.Row(
                                [
                                    dbc.Col(html.H5("Highest Crash Density Corridors (Sliding Windows Scores)", className="mb-0"), width=True),
                                    dbc.Col(
                                        dbc.Button(
                                            "-",
                                            id=f"top-corridors-toggle",
                                            size="lg",
                                            className="p-0 border-0",
                                            style={
                                                "background": "transparent", 
                                                "color": "#6c757d", 
                                                # "fontSize": "2rem"
                                            },
                                        ),
                                        width="auto",
                                    ),
                                ],
                                align="center",
                            )
                        ]),
                        dbc.Collapse(
                            dbc.CardBody([
                                # placeholder for warning user about missing sliding windows scores
                                html.Div(id="missing-sliding-windows-scores-modal-div"), 
                                dbc.Row([
                                    dbc.Col([
                                        dbc.Row(
                                            dcc.Dropdown(
                                                id=f"top-corridors-mode-filter",
                                                options=[{"label": ml, "value": m} for m, ml in MODE_LABELS.items()],
                                                clearable=False
                                            ),
                                        ),
                                        html.Hr(),
                                        dbc.Row(
                                            html.Div(id="top-corridors-summary-table-div")
                                        )
                                    ]),
                                    dbc.Col(_top_corridors_map_card())
                                ])
                            ]),
                            id=f"top-corridors-collapse", is_open=True
                        )
                    ]),
                    dcc.Store(id="top-corridors-fc-store"),
                ]
            ),
        ],
        className="m-2"
    )

def _dashboard_plot_card(id_prefix, title, filter_title=None, 
                         exclude_filter=False, exclude_slider=False,
                         opener_text=None):
    """
    Create a dashboard plot card with placeholder divs for filters and summary table.
    
    :param id_prefix: prefix for the component IDs (used in callbacks)
    :param title: Card title
    :param dropdown_filter_title: Title for the dropdown filter
    :param slider_title: Title for the year range slider
    :param exclude_filter: Whether to exclude the dropdown filter
    :param exclude_slider: Whether to exclude the year range slider
    :return: A dbc.Card containing the div for dashboard plot, filters, and summary table
    """
    return dbc.Card([
        dbc.CardHeader(
            dbc.Row(
                [
                    dbc.Col(html.H5(title, className="mb-0"), width=True),
                    dbc.Col(
                        dbc.Button(
                            "-",
                            id=f"{id_prefix}-toggle",
                            size="lg",
                            className="p-0 border-0",
                            style={
                                "background": "transparent", 
                                "color": "#6c757d", 
                                # "fontSize": "2rem"
                            },
                        ),
                        width="auto",
                    ),
                ],
                align="center",
            )
        ),
        dbc.Collapse(
            dbc.CardBody([opener_text]+[
                dbc.Row([
                    dbc.Col([
                        dbc.Row([
                            html.B(filter_title),
                            dcc.Dropdown(
                                id=f"{id_prefix}-filter",
                                options=[{"label": f"All Crashes", "value": f"All Crashes"}],
                                value="All Crashes",
                                clearable=False
                            ),
                            html.Br(),
                            html.Br(),
                        ], style={"display": "none"} if exclude_filter else {}),
                        dbc.Row([
                            html.B("Select Year Range:"),
                            html.Br(),html.Br(),
                            html.Div(
                                children=dcc.RangeSlider(
                                    id=f"{id_prefix}-slider",
                                    min=2020,
                                    max=2024,
                                    step=1,
                                    value=[2020, 2024],
                                    marks={year: str(year) for year in range(2020, 2025)},
                                    tooltip={"placement": "top", "always_visible": True},
                                ),
                                id=f"{id_prefix}-slider-div"
                            ),
                        ], style={"display": "none"} if exclude_slider else {}),
                    ], width=3),
                    dbc.Col(html.Div(id=f"{id_prefix}-figure"), width=9)
                ]),
                dbc.Row(html.Div(id=f"{id_prefix}-table")),
            ]), 
            id=f"{id_prefix}-collapse", 
            is_open=True
        )
        ])

def _top_corridors_map_card():
    # setup base map tiles
    map_children = [
        dl.Pane(name="pop_pane", style={"zIndex": 1000}),
        dl.LayersControl(
            [
                dl.BaseLayer(
                    dl.TileLayer(
                        url=bm.tile_url,
                        attribution=bm.attribution,
                    ),
                    name=bm.name,
                    checked=bm.default,
                )
                for bm in TOP_CORRIDOR_CONFIG.basemaps
            ]
        )
    ]

    map_children.append(dl.GeoJSON(id="top-corridors-layer"))

    map_card = dbc.Card(
        [
            dbc.CardHeader("Top Crash Density Corridors Map"),
            dbc.CardBody(
                dl.Map(
                    id="top-corridors-map",
                    center=(39.5, -98.35),  # CONUS
                    zoom=4,
                    children=map_children,
                    style={"height": "60vh", "width": "100%", "borderRadius": "12px"},
                )
            ),
        ],
        className="mb-3 shadow-sm",
    )
    return map_card