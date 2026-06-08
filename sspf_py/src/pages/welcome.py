from dash import html
import dash_bootstrap_components as dbc
from ..global_vars import CONTACT_EMAIL
from ..auth import current_user, AUTH_ENABLED
from flask import session


def _pill(text, color="secondary"):
    return dbc.Badge(text, color=color, pill=True, className="me-2 mb-2")


def _person_icon():
    return html.Div(
        [
            html.Div(
                style={
                    "width": "10px",
                    "height": "10px",
                    "borderRadius": "50%",
                    "backgroundColor": "currentColor",
                    "margin": "0 auto 2px auto",
                }
            ),
            html.Div(
                style={
                    "width": "16px",
                    "height": "8px",
                    "borderRadius": "8px 8px 4px 4px",
                    "backgroundColor": "currentColor",
                    "margin": "0 auto",
                }
            ),
        ],
        style={"lineHeight": "1"},
    )


def _status_tile(icon_content, title, value, color):
    return dbc.Col(
        dbc.Card(
            dbc.CardBody(
                [
                    html.Div(
                        [
                            html.Div(
                                icon_content,
                                className=(
                                    "d-flex align-items-center justify-content-center "
                                    f"rounded-circle bg-{color} text-white fw-bold"
                                ),
                                style={"width": "44px", "height": "44px", "fontSize": "1.1rem"},
                            ),
                            html.Div(
                                [
                                    html.Div(title, className="text-muted small text-uppercase"),
                                    html.Div(value, className="fw-semibold"),
                                ],
                                className="ms-3",
                            ),
                        ],
                        className="d-flex align-items-center",
                    )
                ]
            ),
            className="h-100 border-0 shadow-sm",
        ),
        xs=12,
        md=6,
    )


def _login_banner():
    """Returns a login/welcome call-to-action banner, or a greeting if already logged in."""
    try:
        user = current_user() if AUTH_ENABLED else None
    except Exception:
        user = None

    try:
        study_id = session.get("active_study_id")
    except Exception:
        study_id = None

    is_logged_in = bool(user)
    username_value = user.username if is_logged_in else "Not logged in"
    study_id = study_id if is_logged_in else None
    study_name = session.get("study_name") if study_id else None
    study_value = f"{study_name}" if study_name else "No study selected"

    tiles = dbc.Row(
        [
            _status_tile(
                icon_content=_person_icon(),
                title="Login Status",
                value=username_value,
                color="success" if is_logged_in else "secondary",
            ),
            _status_tile(
                icon_content="S",
                title="Study Status",
                value=study_value,
                color="info" if study_id else "secondary",
            ),
        ],
        className="g-2",
    )

    if is_logged_in and study_id:
        footer = html.Div(
            "Use the side navigation pane to load data, run analyses, or view results.",
            className="mt-2 small",
        )
        alert_color = "success"
    elif is_logged_in:
        footer = html.Div(
            [
                html.Span("No active study selected. Head to "),
                dbc.Button("My Studies", href="/studies", color="primary", size="sm", className="mx-1"),
                html.Span("to continue your work."),
            ],
            className="mt-2",
        )
        alert_color = "success"
    else:
        footer = html.Div(
            [
                html.Div(
                    "Log in or create an account to run analyses and save your work. You can still explore the welcome, overview, and instructions pages without logging in.",
                    className="mb-2",
                ),
                html.Div(
                    [
                        dbc.Button("Log In", href="/login", color="primary", size="sm", className="me-2"),
                        dbc.Button("Create Account", href="/register", color="secondary", size="sm"),
                    ]
                ),
            ],
            className="mt-2",
        )
        alert_color = "secondary"

    return dbc.Alert(
        [tiles, footer],
        color=alert_color,
        className="mb-3",
    )


def layout():
    return dbc.Container(
        [
            # Introduction
            html.Div(
                [
                    html.H1("Welcome!", className="display-5 fw-bold mb-2"),
                    _login_banner(),
                    html.P(
                        [
                            "The ",
                            html.Strong("Safer Streets Priority Finder"),
                            " enables you to analyze the risk to road users on your community's roads. ",
                            "With just some minimal data prep required on your end, this tool uses statistical and spatial analysis to estimate crash risk along the road network. "
                            "You can use your local road, crash, and study area data or select from nationally available datasets that are preloaded in this tool to:",
                            html.Ul(
                                [
                                    html.Li("Explore descriptive statistics related to your crash data"),
                                    html.Li(
                                        "Perform a Sliding Windows Analysis using historical crash data to inform a High Injury Network"
                                    ),
                                    html.Li(
                                        "Apply a Safer Streets Model to estimate risk along your road network, even in areas that haven't had any reported crashes recently"
                                    ),
                                ],
                                className="mb-4",
                            ),
                        ],
                    ),
                ],
            ),

            # CONTRIBUTORS + FUNDING
            dbc.Card(
                [
                    dbc.CardHeader(html.H4("SSPF Contributors", id="contributors", className="mb-0")),
                    dbc.CardBody(
                        [
                            html.Ul(
                                [
                                    html.Li(
                                        [
                                            html.Strong("City of New Orleans: "),
                                            "Daniel Jatres and Jennifer Ruley, PE",
                                        ]
                                    ),
                                    html.Li(
                                        [
                                            html.Strong("Toole Design: "),
                                            "Theja Putta, PhD; Lucas Yu, PhD; Nan Jiang, PhD; Joanna Wang; and Tariq Shihadah, PE, RSP",
                                            html.Sub("2i"),
                                            ". Additional contributions by Hugh Kelley, Jacob Nigro; Teresa Chang, Kenneth Harvey, HaoChe Hung, Andy Keiper; and Omar Peters",
                                        ]
                                    ),
                                    html.Li(
                                        [
                                            html.Strong("Safe Streets Research: "),
                                            "Jessica Schoner, PhD; Brian Almdale; and Reid Passmore, PhD. ",
                                            "Additional contributions by Rebecca Sanders, PhD, RSP", 
                                            html.Sub("2B"),
                                        ]
                                    ),
                                ],
                                className="mb-3",
                            ),
                            dbc.Alert(
                                "The update to the tool is funded through USDOT's Safer Streets for All grant program. \
                                    Phase 1 development funded through USDOT's Safety Data Initiative Grant.",
                                color="secondary",
                                className="mb-2",
                            ),
                            html.Small(
                                "Additional acknowledgements to Daniel Patterson, Rachel Finfer, Jacob Nigro, Brendan Murphy, " \
                                "Robert Stickney, Tara Tolford, Maryam Izadi, Frank Proulx and Ted Mansfield for their contributions to the phase 1 of the project.",
                                style={"color": "#6E7072", "fontSize": "0.875rem"},
                            ),
                        ]
                    ),
                ],
            ),
            html.Br(),

            # Disclaimers
            dbc.Card(
                [
                    dbc.CardHeader(html.H4("Disclaimers", className="mb-0")),
                    dbc.CardBody(
                        [
                            html.H5("Federal Safety Planning Disclaimer"),
                            html.P(
                                "This tool and the information contained herein are prepared solely for the purpose of identifying, \
                                evaluating, and planning safety improvements on public roads, which may be implemented utilizing federal aid highway funds. \
                                This information shall not be subject to discovery or admitted into evidence in Federal or State court pursuant to 23 U.S.C. §407."
                            ),
                            html.Br(),
                            html.H5("Data and Analysis Disclaimer"),
                            html.P(
                                "This tool and associated outputs are provided for informational and planning purposes only. \
                                Results, recommendations, geographic information, maps, visualizations, and analyses are based on \
                                national-scale datasets and, where applicable, user-uploaded data that may not be independently verified.",
                            ),
                            html.P(
                                "The project creators make no warranties or representations, express or implied, regarding the accuracy, \
                                completeness, reliability, timeliness, or suitability of the data, analyses, or recommendations provided through this tool. \
                                Users are responsible for independently verifying all information before making engineering, planning, policy, funding, or safety decisions."
                            ),
                            html.P(
                                "Crash data and roadway safety analyses are intended solely to support the identification and evaluation of potential safety improvements and hazardous locations. \
                                Motor vehicle crashes are complex events that may involve multiple contributing factors not reflected in the available datasets."
                            ),
                            html.Br(),
                            html.H5("Limitation of Liability"),

                            html.P(
                                "The project creators shall not be liable for any errors, omissions, inaccuracies, or interpretations \
                                arising from the use of this tool or its associated data and analyses.",
                            ),
                            html.Br(),
                            html.H5("Accessibility"),
                            html.P([
                                "If you need assistance using this tool, require accessibility accommodations, or have questions about the platform, please contact: ",
                                html.A(CONTACT_EMAIL, href=f"mailto:{CONTACT_EMAIL}", style={"color": "#0267FD"})
                            ]),
                        ]
                    ),
                ],
            ),
        ],
        fluid=True,
        className="p-3",
    )