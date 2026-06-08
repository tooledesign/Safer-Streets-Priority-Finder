from dash import html, dcc
import dash_bootstrap_components as dbc
from ..auth import current_user, AUTH_ENABLED
from ..global_vars import CONTACT_EMAIL

def layout():
    try:
        user = current_user() if AUTH_ENABLED else None
    except Exception:
        user = None

    if user:
        return dbc.Container(
            [
                html.H2("Sign in"),
                dbc.Alert(
                    [
                        html.Span("Currently logged in as "),
                        html.Strong(user.username),
                        html.Span(". If you want to sign in as a different user, please "),
                        html.A("log out", href="/logout", style={"color": "#0267FD"}),
                        html.Span(" first."),
                    ],
                    color="info",
                    className="mt-2",
                ),
                dbc.Button("Go to Studies", href="/studies", color="primary", className="mt-2 me-2"),
                dbc.Button("Log out", href="/logout", color="secondary", className="mt-2"),
            ],
            className="py-4"
        )

    return dbc.Container(
        [
            html.H2("Sign in"),
            dbc.Alert(id="login-error", color="danger", is_open=False),
            dbc.Form(
                [
                    dbc.Label("Username"),
                    dbc.Input(id="login-username", type="text", placeholder="yourname"),
                    dbc.Label("Password", className="mt-2"),
                    dbc.Input(id="login-password", type="password", placeholder="••••••••"),
                    dbc.Button("Sign in", id="login-submit", color="primary", className="mt-3"),
                    dbc.Button("Create an account", id="go-register", color="link", className="mt-3 ms-2"),
                    dcc.Location(id="login-redirect", refresh=True),
                    dcc.Location(id="login-nav", refresh=True)
                ]
            ),
            dbc.Alert(
                [
                    html.Span("Forgot your password? Email us at "),
                    html.A(CONTACT_EMAIL, href=f"mailto:{CONTACT_EMAIL}", style={"color": "#0267FD"}),
                    html.Span(" to have it reset."),
                ],
                color="info",
                className="mt-4",
            ),
        ],
        className="py-4"
    )