from dash import html, dcc
import dash
import os
import utm
import dash_bootstrap_components as dbc
from .. import utils
from ..global_vars import *

def _note(text):
    return dbc.Alert(text, color="secondary", className="mt-2")

_overwrite_warning = dbc.Alert(
        [
            html.Strong("Warning: "),
            "Uploading new data will overwrite any existing data. Each of the study area, roads, and crashes sections have their own data store, so you can update one without affecting the others. Use the buttons above to navigate to the Review page to view your existing data.",
        ],
        color="warning",
        id="overwrite-warning",
        is_open=True,
        className="mt-3"
    )

def study_area_panel():
    # --- Card 1: Intro
    intro = dbc.Card(
        [
            dbc.CardHeader(html.H3("Study Area Selection")),
            dbc.CardBody([
                html.Ul([
                    html.Li("If you have previously uploaded a study area for this study that you wish to continue using, you can skip this step and use the existing study boundary."),
                    html.Li("Use the buttons above to navigate to update your roads, crashes, or to review your data"),
                    html.Li("If you want to upload a new study area, you can either select a US Census county/parish boundary or upload your own study area as a shapefile."),
                    html.Li("If your study area polygon has multiple parts, they will be dissolved into a single geometry."),
                    html.Li("This study area will be used to clip roads and crashes data for your analysis."),
                    html.Li("If the study area boundary crosses the road network, consider buffering it slighly to avoid random segmentation of road segments. \
                            Buffering it out too much might skew the analysis results."),
                    html.Li([
                                "See the ",
                                html.A("instructions page", href="/instructions", style={"color": "#0267FD"}),
                                " for more details.",
                            ]),
                ]),
            ])
        ],
        className="mb-3 shadow-sm"
    )

    # --- Card 2: Source choice (Preset County vs Local Upload)
    choose = dbc.Card(
        [
            dbc.CardHeader(html.Strong("Select a US Census county boundary or upload your own study area")),
            dbc.CardBody(
                [
                    html.Div("Make a choice :", className="fw-semibold mb-2"),
                    dbc.RadioItems(
                        id="study-source",
                        options=[
                            {"label": " Nationally Available County Boundary", "value": "preset"},
                            {"label": " Local Study Area Boundary", "value": "local"},
                        ],
                        value="preset",
                        inline=True,
                        inputClassName="form-check-input me-2",
                        className="mb-2"
                    ),

                    # PRESET controls
                    html.Div(
                        [
                            dbc.Row(
                                [
                                    dbc.Col(
                                        dcc.Dropdown(
                                            id="preset-state",
                                            placeholder="Select a state…",
                                            options=[{"label": s, "value": s} for s in utils.load.get_state_list()],
                                        ), md=4
                                    ),
                                    dbc.Col(
                                        dcc.Dropdown(
                                            id="preset-county",
                                            placeholder="Select a county…",
                                            options=[],   # <-- will be populated by a callback
                                            value=None,
                                            clearable=True,
                                        ), md=8
                                    ),
                                ], className="g-2"
                            ),
                        ],
                        id="preset-controls"
                    ),

                    utils.preset_modals.busy_modal(text="Getting county list...", is_open=False, id_="study-state-load-busy"),
                    utils.preset_modals.busy_modal(text="Getting county geometry...", is_open=False, id_="study-county-load-busy"),
                    utils.preset_modals.busy_modal(text="Reading your upload...", is_open=False, id_="study-shp-load-busy"),

                    # LOCAL upload controls
                    html.Div(
                        [
                            _note("Upload a single ZIP containing .shp, .shx, .dbf, .prj files. The maximum file size allowed is 20 Mbs."),
                            dcc.Upload(
                                id="upload-study",
                                children=html.Div(["Drag and drop or ", html.A("select a file")]),
                                multiple=False,
                                style={
                                    "width":"100%", "height":"140px", "lineHeight":"140px",
                                    "borderWidth":"1px", "borderStyle":"dashed",
                                    "borderRadius":"12px", "textAlign":"center", "background":"#fff"
                                }
                            ),
                        ],
                        id="local-controls",
                        style={"display": "none"}  # hidden until 'local' chosen
                    ),
                    html.Div(id="load-study-status", className="mt-2 text-muted"),
                ]
            )
        ],
        className="mb-3 shadow-sm"
    )

    # Confirm the study area once loaded
    confirm = dbc.Card(
        [
            dbc.CardBody(
                [
                    dbc.Button(
                        "Confirm Study Area",
                        id="confirm-study",
                        color="success",
                        className="mt-2",
                        disabled=True,
                    ),
                    # message area confirming the study area has been uploaded
                    html.Div(id="confirm-study-msg", className="mt-2 text-muted"),
                    html.P("Once you upload or select a study area from the county dropdown, that study area will be shown on the map pane below. \
                           Please confirm that the study area looks correct before clicking 'Confirm Study Area' button to proceed. \
                           If you need to change your study area after confirming, simply upload a new one or select a different county and confirm again. \
                           If you have previously uploaded a study area, you can view that in the 'Review Input Data' section along with your other input data.", className="mt-3"),
                    # store the dissolved geometry + metadata from preset/upload
                    dcc.Store(id="study-area-store"),
                ]
            ),
            utils.preset_modals.busy_modal(text="Uploading your study area to database...", is_open=False, id_="study-confirm-busy")
        ],
        className="mb-3 shadow-sm"
    )

    # --- Map panel
    leaflet = utils.gen.leaflet_map_card(
        layer_params=[
            {
                "LayerGroup": {"id": "study-layer"}, 
                "Overlay": {"name": "Study Area", "checked": True}, 
                "Pane": {"name": "study_pane", "style": {"zIndex": 300}}
            }
        ],
        header="Study Area Map",
        id_="study-map"
    )

    return html.Div([intro, choose, confirm, _overwrite_warning, leaflet], className="p-2")

def roads_panel():
    return html.Div(
        [
            dbc.Card(
                [
                    dbc.CardHeader(html.H3("Load Roads (Street Centerlines)"),),
                    dbc.CardBody([
                        html.Ul([
                            html.Li("If you have previously uploaded roads data for this study that you wish to continue using, you can skip this step and use the existing roads data."),
                            html.Li("Use the buttons above to navigate to update your study boundary, crashes, or to review your data"),
                            html.Li("You can either choose from Open Street Map (OSM) or upload your own roads data as a shapefile ZIP."),
                            html.Li("Your roads data will be clipped to your study area for analysis."),
                            html.Li([
                                "See the ",
                                html.A("instructions page", href="/instructions", style={"color": "#0267FD"}),
                                " for more details.",
                            ]),
                        ]),
                    ])
                ],
                className="mb-3 shadow-sm"
            ),

            dbc.RadioItems(
                id="roads-source",
                options=[
                    {"label": " Use Open Street Map (OSM) data", "value": "builtin"},
                    {"label": " Upload my own roads (shapefile ZIP)", "value": "upload"},
                ],
                value="builtin",
                inline=True,
                className="mb-2"
            ),

            # Upload block (shown only when 'upload' is selected)
            html.Div(id="upload-roads-status", className="mt-2 text-muted"),
            utils.preset_modals.busy_modal(text="Reading your upload...", is_open=False, id_="roads-shp-load-busy"),
            html.Div(
                [
                    dcc.Upload(
                        id="upload-roads",
                        children=html.Div(["Drag and drop or ", html.A("select a file")]),
                        multiple=False,
                        style={
                            "width": "100%", "height": "120px", "lineHeight": "120px",
                            "borderWidth": "1px", "borderStyle": "dashed",
                            "borderRadius": "12px", "textAlign": "center", "background": "#fff"
                        }
                    ),
                ],
                id="roads-upload-wrap",
                style={"display": "none"}  # default hidden; toggled by radio
            ),

            # identify id column, name column, functional class column to select as a drop down
            html.Div(
                children=[
                    html.Div(
                        children=[
                            utils.load.roads_cols_card()
                        ],
                        id="roads-col-select-wrap",
                        style={"display": "none"},
                    ),
                    html.Div(id="fclass-assign-msg", className="mt-2 text-muted"),
                    html.Div(
                                children=[
                                    utils.load.attribute_map_card(attr_type="fclass")
                                ],
                                id="roads-fclass-mapping-wrap",
                                style={"display": "none", "position": "relative", "overflow": "visible", "zIndex": 1}
                            ),
                ],
                id="roads-options-wrap",
                style={"display": "none"},
            ),

            # button to map functional class values to standard set
            dbc.Button("View Functional Class Assignments", id="fclass-assign", color="success", className="mt-3", disabled=False),

            # confirm roads after selecting columns and fclass mapping
            html.Hr(className="mt-4"),
            dbc.Button("Confirm Roads", id="confirm-roads", color="success", className="mt-3", disabled=True),
            html.Div(id="confirm-roads-msg", className="mt-2 text-muted"),
            utils.preset_modals.busy_modal(text="Uploading roads data to database...", is_open=False, id_="roads-confirm-busy"),

            # store which roads source and (if upload) any temp table name
            dcc.Store(id="roads-store", data={"source": "builtin"}),

            # --- Warning about overwriting existing data
            _overwrite_warning
        ],
        className="p-2"
    )

def crashes_panel():
    return html.Div(
        [
            dbc.Card(
                [
                    dbc.CardHeader(html.H3("Load Crashes"),),
                    dbc.CardBody([
                        html.Ul([
                            html.Li("If you have previously uploaded crashes data for this study that you wish to continue using, you can skip this step and use the existing crashes data."),
                            html.Li("Use the buttons above to navigate to update your study boundary, roads, or to review your data"),
                            html.Li("You can either choose from FARS data or upload your own crashes data as a shapefile ZIP."),
                            html.Li("The model needs 5 years of crash data to run. If you have more than 5 years of data, only the most recent 5 years will be used."),
                            html.Li("Your crashes data will be clipped to your study area for analysis."),
                            html.Li([
                                "See the ",
                                html.A("instructions page", href="/instructions", style={"color": "#0267FD"}),
                                " for more details.",
                            ]),
                        ]),
                    ])
                ],
                className="mb-3 shadow-sm"
            ),
            dbc.RadioItems(
                id="crashes-source",
                options=[
                    {"label": " Use FARS crashes", "value": "builtin"},
                    {"label": " Upload my own crashes (shapefile ZIP)", "value": "upload"},
                ],
                value="builtin",
                inline=True,
                className="mb-2"
            ),

            # Upload block (shown only when 'upload' is selected)
            html.Div(
                [
                    html.P("Upload a single ZIP containing .shp, .shx, .dbf, .prj files. The maximum file size allowed is 20 Mbs."),
                    dcc.Upload(
                        id="upload-crashes",
                        children=html.Div(["Drag and drop or ", html.A("select a file")]),
                        multiple=False,
                        style={
                            "width": "100%", "height": "120px", "lineHeight": "120px",
                            "borderWidth": "1px", "borderStyle": "dashed",
                            "borderRadius": "12px", "textAlign": "center", "background": "#fff"
                        }
                    ),
                    html.Div(id="upload-crashes-status", className="mt-2 text-muted"),
                    utils.preset_modals.busy_modal(text="Reading your upload...", is_open=False, id_="crashes-shp-load-busy"),

                    dbc.Card(
                        [
                            dbc.CardHeader("Crash Column Selection"),
                            dbc.CardBody(
                                [
                                    utils.load.column_dropdown(id_="crash-id-col", label="ID Column", options=[], value=None),
                                    utils.load.column_dropdown(id_="crash-year-col", label="Year Column", options=[], value=None),
                                    utils.load.column_dropdown(id_="crash-mode-col", label="Crash Mode Column", options=[], value=None),
                                    utils.load.column_dropdown(id_="crash-severity-col", label="Injury Severity Column", options=[], value=None),
                                ],
                                id="crashes-col-select-wrap",
                                className="mt-3"
                            ),
                        ],
                        className="mt-2"
                    ),

                    # add two cards for mode and severity mapping in two columns
                    dbc.Row(
                        [
                            dbc.Col(dbc.Button("View Crash Mode Assignments", id="crash-mode-assign", color="success", className="mt-3", disabled=True)),
                            dbc.Col(dbc.Button("View Crash Severity Assignments", id="crash-severity-assign", color="success", className="mt-3", disabled=True)),
                            html.Div(id="crash-attr-assign-msg", className="mt-2 text-muted"),
                        ],
                        className="mt-2"
                    ),
                    dbc.Row(
                        [
                            dbc.Col(
                                html.Div(
                                    children=[
                                        utils.load.attribute_map_card(attr_type="crash_mode"),
                                    ],
                                    id="crash-mode-mapping-wrap",
                                    style={"display": "none", "position": "relative", "overflow": "visible", "zIndex": 1}
                                ),
                            ),
                            dbc.Col(
                                html.Div(
                                    children=[
                                        utils.load.attribute_map_card(attr_type="crash_severity"),
                                    ],
                                    id="crash-severity-mapping-wrap",
                                    style={"display": "none", "position": "relative", "overflow": "visible", "zIndex": 1}
                                ),
                            ),
                        ],
                        className="mt-2"
                    ),
                ],
                id="crashes-upload-wrap",
                style={"display": "none"}  # default hidden; toggled by radio
            ),
            html.Hr(className="mt-4"),
            dbc.Button("Confirm Crashes", id="confirm-crashes", color="success", className="mt-3", disabled=False),
            html.Div(id="confirm-crashes-msg", className="mt-2 text-muted"),
            utils.preset_modals.busy_modal(text="Uploading crashes data to database...", is_open=False, id_="crashes-confirm-busy"),

            # store which roads source and (if upload) any temp table name
            dcc.Store(id="crashes-store", data={"source": "builtin"}),

            # --- Warning about overwriting existing data
            _overwrite_warning
        ],
        className="p-2"
    )

def review_panel():
    return html.Div(
        [
            dbc.Card(
                [
                    dbc.CardHeader(html.Strong("Review Your Data")),
                    dbc.CardBody(
                        """
                        In this section, you'll have a chance to review your input data and view any issues with your input data. 
                        If no critical issues are found, you can click on the "Map Input Data" button to load data onto the map.
                        Once you review the data, you may proceed to adjusting analysis parameters on the Analysis page.
                        Note that this step does not save any data to your study; it is just for review.
                        """
                    )
                ],
                className="mb-3 shadow-sm"
            ),
            html.Hr(),
            dbc.Card(
                [
                    utils.preset_modals.busy_modal(text="Checking Input Data...", is_open=True, id_="data-issues-busy"),
                    dbc.CardHeader(html.Strong("Data Issues Tracker")),
                    html.Div(
                        id="data-issues-output", className="mt-2 text-muted"
                    )
                ],
                className="mb-3 shadow-sm"
            ),
            html.Hr(),
            dbc.Button("Map Input Data", id="map-input-data", color="primary", className="me-2", disabled=True),
            html.P(
                [
                    "Once you review your inputs, proceed to ",
                    html.A("Analysis Settings", href="/analysis_settings", style={"color": "#0267FD"}),
                    " to set your analysis parameters or ",
                    html.A("Run Analysis", href="/run_analysis", style={"color": "#0267FD"}),
                    " to perform sliding windows or safer streets model analyses. ",
                    "If the color contrast of the map layers is not sufficient with your data, try switching to a different basemap using the layers pane in the top right corner of the map."
                ],
                className="mt-3"
            ),
            html.Div(id="inputs-review-map-truncation-notice", className="mt-2"),
            html.Div(style={"height": "16px"}),
            utils.preset_modals.busy_modal(text="Updating Map...", is_open=False, id_="inputs-review-map-busy"),
            html.Div(
                children=[
                    utils.gen.leaflet_map_card(
                        id_="inputs-review-map", header="Inputs Review Map",
                        layer_params=[
                            {
                                "LayerGroup": {"id": "input-crashes-layer"}, 
                                "Overlay": {"name": "Crashes", "checked": True}, 
                                "Pane": {"name": "crashes_pane", "style": {"zIndex": 500}}
                            },
                            {
                                "LayerGroup": {"id": "input-roads-layer"}, 
                                "Overlay": {"name": "Roads", "checked": True}, 
                                "Pane": {"name": "roads_pane", "style": {"zIndex": 400}}
                            },
                            {
                                "LayerGroup": {"id": "input-study-layer"}, 
                                "Overlay": {"name": "Study Area", "checked": True}, 
                                "Pane": {"name": "study_pane", "style": {"zIndex": 300}}
                            },
                        ],
                        legend_params=[
                            {"title": "Functional Class", "items": [(fc, color) for fc, color in FCLASS_COLORS.items()]},
                            {"title": "Severity", "items": [(cm, color) for cm, color in SEVERITY_COLORS.items()]},
                        ],
                    ),
                ],
                style={"display": "block"}, id="inputs-review-map-wrap"), # hidden until button clicked
            dcc.Store(id="review-store"),
        ],
        className="p-2"
    )

def load_navbar():
    # Styled via .ld-step classes in assets/custom.css
    return html.Div(
        [
            html.A("Load Study Area",  href="#study",       id="ld-step-study",       className="ld-step"),
            html.A("Load Roads",       href="#roads",       id="ld-step-roads",       className="ld-step ld-step-muted"),
            html.A("Load Crashes",     href="#crashes",     id="ld-step-crashes",     className="ld-step ld-step-muted"),
            html.A("Review Input Data", href="#review",      id="ld-step-review",      className="ld-step ld-step-muted"),
        ],
        className="ld-steps-wrap mb-3"
    )

# Page layout
layout = html.Div(
    [
        load_navbar(),
        html.Div(id="load_data-content")
    ],
    className="p-2"
)