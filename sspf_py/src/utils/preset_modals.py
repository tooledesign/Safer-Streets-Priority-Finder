import dash_bootstrap_components as dbc
from dash import html
from ..global_vars import CONTACT_EMAIL

def busy_modal(text="Processing...", is_open=True, id_="busy-modal"):
    "A modal with a spinner and a text message. Cannot be closed by user, instead controlled by app callbacks logic."
    return dbc.Modal(
        [
            dbc.ModalBody(
                [
                    dbc.Spinner(size="lg", color="primary"),
                    html.Div(text, className="mt-2")
                ],
                className="text-center"
            )
        ],
        id=id_,
        is_open=is_open,
        centered=True,
        backdrop="static",
        keyboard=False,
        zindex=5000
    )

def db_connection_error_modal(support_email=None):
    if support_email is None:
        support_email = CONTACT_EMAIL
    "A modal indicating a database connection error. Needed for any pages needing login. Cannot be closed by user."
    return dbc.Modal(
        [
            dbc.ModalHeader(
                html.Div("Database Connection Error", className="w-100 text-center"),
                close_button=False,
                className="bg-danger text-white"
            ),
            dbc.ModalBody(
                html.Div(
                    [
                        f"There was an error connecting to the database. \
                        Please ensure your connection settings are correct and refresh the page. \n \
                        Please reach out to the support team at the email ",
                        html.A(support_email, href=f"mailto:{support_email}"),
                        " if you continue to experience issues.",
                    ],
                    className="text-center"
                )
            ),
        ],
        is_open=True, centered=True, backdrop="static", keyboard=False,
        zindex=5000
    )

def simple_modal(message=None, color="black"):
    "A modal with just a message. Text message color is optional. Default is black"
    return dbc.Modal(
        [
            dbc.ModalBody(
                html.Div([message], className="text-center", style={"color": color, "padding": "1.5rem"})
            ),
        ],
        is_open=True, centered=True, backdrop=True, keyboard=True, fade=True,
        zindex=5000
    )

def info_modal(title=None, message=None):
    title = title if title else ""
    message = message if message else ""
    return dbc.Modal(
        [
            dbc.ModalHeader(
                html.Div([title], className="w-100 text-center"),
                close_button=True,
                className="bg-info text-white"
            ) if title else None,
            dbc.ModalBody(
                html.Div([message], className="text-center"),

            ) if message else None,
        ],
        is_open=True, centered=True, backdrop=True, keyboard=True, fade=True,
        zindex=5000
    )

def study_upload_success_modal():
    return dbc.Modal(
        [
            dbc.ModalHeader(
                html.Div("Study area saved successfully!", className="w-100 text-center"),
                close_button=True,
                className="bg-success text-white"
            ),
            dbc.ModalBody(
                dbc.Row(
                    [
                        dbc.Col(
                            dbc.Button("Load Roads", href="#roads", color="primary", className="mt-2 w-100"),
                            width=6
                        ),
                        dbc.Col(
                            dbc.Button("Load Crashes", href="#crashes", color="primary", className="mt-2 w-100"),
                            width=6
                        ),
                    ],
                    justify="center",
                    className="g-2"
                ),
                className="text-center"
            ),
        ],
        is_open=True, centered=True, backdrop="static", keyboard=False, fade=True,
        zindex=5000
    )

def roads_upload_success_modal():
    return dbc.Modal(
        [
            dbc.ModalHeader(
                html.Div("Roads data loaded successfully!", className="w-100 text-center"),
                close_button=True,
                className="bg-success text-white"
            ),
            dbc.ModalBody(
                dbc.Row(
                    [
                        dbc.Col(
                            dbc.Button("Load Crashes", href="#crashes", color="primary", className="mt-2 w-100"),
                            width=12
                        ),
                    ],
                    justify="center",
                    className="g-2"
                ),
                className="text-center"
            ),
        ],
        is_open=True, centered=True, backdrop="static", keyboard=False, fade=True,
        zindex=5000
    )

def crashes_upload_success_modal():
    return dbc.Modal(
        [
            dbc.ModalHeader(
                html.Div("Crashes data loaded successfully!", className="w-100 text-center"),
                close_button=True,
                className="bg-success text-white"
            ),
            dbc.ModalBody(
                [
                    html.Div("You can now review your input data and proceed with the analysis."),
                        dbc.Row(
                        [
                            dbc.Col(
                                dbc.Button("Review Inputs", href="#review", color="primary", className="mt-2 w-100"),
                                width=12
                            ),
                        ],
                        justify="center",
                        className="g-2"
                    ),
                ],
                className="text-center"
            ),
        ],
        is_open=True, centered=True, backdrop=True, keyboard=True, fade=True,
        zindex=5000
    )