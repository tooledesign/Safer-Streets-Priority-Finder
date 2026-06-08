from dash import html
import dash_bootstrap_components as dbc


def layout(pathname=None):
    requested_path = pathname or "(unknown)"
    return dbc.Container(
        [
            html.H2("404 - Page Not Found", className="mb-3"),
            html.P(
                [
                    "The requested URL was not found: ",
                    html.Code(requested_path),
                ]
            )
        ],
        className="py-4",
    )
