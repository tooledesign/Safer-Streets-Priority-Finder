from dash import html, dcc
import dash_bootstrap_components as dbc

def layout():
    return dbc.Container(
        [
            html.H2("Create account"),
            dbc.Alert(id="register-error", color="danger", is_open=False),
            dbc.Alert(id="register-success", color="success", is_open=False),
            dbc.Form(
                [
                    dbc.Row(
                        [
                            # username must be 1-16 characters long, alphanumeric. It is also not case sensitive.
                            dbc.Col([
                                dbc.Label("Username"),
                                dbc.Input(
                                    id="reg-username",
                                    type="text",
                                    maxLength=16,
                                    minLength=1,
                                    pattern="^[a-zA-Z0-9]+$",
                                ),
                            ], md=4),
                            dbc.Col([dbc.Label("Email"), dbc.Input(id="reg-email", type="email")], md=8),
                        ], className="g-3"
                    ),
                    dbc.Label("Password", className="mt-2"),
                    dbc.Input(id="reg-password", type="password"),
                    dbc.Label("Confirm Password", className="mt-2"),
                    dbc.Input(id="reg-password2", type="password"),
                    dbc.Button("Create account", id="reg-submit", color="primary", className="mt-3"),
                    dcc.Location(id="register-redirect", refresh=True),

                    html.Hr(),
                    html.B(
                        "Username must be 1-16 characters long, using only letters and numbers. "
                        "Usernames are not case sensitive (e.g., UserName and username are considered the same)."
                    ),
                ]
            ),
        ],
        className="py-4"
    )
