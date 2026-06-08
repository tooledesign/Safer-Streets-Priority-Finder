from dash import html, dcc
import dash_bootstrap_components as dbc

def layout():
    return dbc.Container(
        [
            html.H2("Change password"),
            dbc.Alert(id="chpw-error", color="danger", is_open=False),
            dbc.Alert(id="chpw-success", color="success", is_open=False),
            dbc.Form(
                [
                    dbc.Label("Current password", className="mt-2"),
                    dbc.Input(id="chpw-current", type="password"),
                    dbc.Label("New password", className="mt-2"),
                    dbc.Input(id="chpw-new", type="password"),
                    dbc.Label("Confirm new password", className="mt-2"),
                    dbc.Input(id="chpw-confirm", type="password"),
                    dbc.Button("Update password", id="chpw-submit", color="primary", className="mt-3"),
                    dcc.Location(id="chpw-redirect", refresh=True),
                ]
            ),
        ],
        className="py-4",
        style={"maxWidth": "480px"},
    )
