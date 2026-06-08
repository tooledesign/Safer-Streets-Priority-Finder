from dash import html, dcc
import dash_bootstrap_components as dbc

def layout():
    return dbc.Container(
        [
            html.H2("Create a new study or Select an existing one"),
            html.Hr(),
            dbc.Row(
                [
                    dbc.Col(
                        [
                            dbc.Label("Create a New Study"),
                            dbc.Input(
                                    id="study-name",
                                    type="text",
                                    maxLength=16,
                                    minLength=1,
                                    pattern="^[a-zA-Z0-9_]+$",
                                ),
                            dbc.Button("Create", id="study-create", color="secondary", className="mt-2"),
                        ], md=6
                    ),
                    dbc.Col(
                        [
                            dbc.Label("Your Studies"),
                            dcc.Dropdown(id="study-select", options=[], placeholder="Select a study…"),
                            dbc.Button("Use this study", id="study-use", color="primary", className="mt-2"),
                            dcc.Location(id="studies-redirect", refresh=True)
                        ], md=6
                    ),
                ], className="g-3"
            ),
            dbc.Alert(id="study-msg", color="info", is_open=False),
            dcc.Store(id="active-study"),   # keep client-side too (optional)
            dbc.Modal(
                [
                    # dbc.ModalHeader(dbc.ModalTitle("Study selected")),
                    dbc.ModalHeader(dbc.ModalTitle(
                        html.P(["Selected study: ", html.Strong(id="study-next-step-study-name"),])
                    )),
                    dbc.ModalBody(
                        [
                            html.P("Where would you like to go next?"),
                            html.Ul(
                                [
                                    html.Li([
                                        "If you want to load new data for this study, click ",
                                        html.A("Load Data", href="/load_data", style={"color": "#0267FD"}),
                                    ]),
                                    html.Li([
                                        "If you want to adjust analysis settings, click ",
                                        html.A("Analysis Settings", href="/analysis_settings", style={"color": "#0267FD"}),
                                    ]),
                                    html.Li([
                                        "To run the analysis with the current settings, click ",
                                        html.A("Run Analysis", href="/run_analysis", style={"color": "#0267FD"}),
                                    ]),
                                    html.Li([
                                        "To view the dashboard with results, click ",
                                        html.A("Dashboard", href="/dashboard", style={"color": "#0267FD"}),
                                    ]),
                                    html.Li([
                                        "To visualize results on a map, click ",
                                        html.A("Map Visualization", href="/map_visualization", style={"color": "#0267FD"}),
                                    ]),
                                ],
                                className="d-flex flex-wrap",
                            ),
                        ]
                    ),
                ],
                id="study-next-step-modal",
                is_open=False,
                centered=True,
                backdrop=True,
            ),
            
            html.Hr(),
            html.Ul(
                [
                    html.Li("Select an existing study from the dropdown and click 'Use this study' to work with it."),
                    html.Li("If you want to create a new study, enter a unique name and click 'Create', then select it from the dropdown."),
                    html.Li("Study names must be 1-16 characters long, using only letters, numbers, or underscores."),
                    html.Li("No spaces allowed - if you have spaces in your study name, consider replacing them with underscores."),
                    html.Li("Names are not case sensitive. 'Study_Name' and 'study_name' would be considered the same."),
                ]
            ),
        ],
        className="py-4"
    )
