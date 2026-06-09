# General utility functions for the SSPF application
import os
from dash import html
import dash_leaflet as dl
import dash_bootstrap_components as dbc

# check if connection to a database can be established
def check_db_connection():
    "Check if a connection to the database can be established using environment variables."
    import sqlalchemy
    
    db_user = os.getenv("DB_USER")
    db_password = os.getenv("DB_PASSWORD")
    db_host = os.getenv("DB_HOST")
    db_port = os.getenv("DB_PORT")
    db_name = os.getenv("DB_NAME")

    try:
        # SQLAlchemy will timeout after 5 seconds if no response from database
        engine = sqlalchemy.create_engine(
            f"postgresql://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}",
            connect_args={"connect_timeout": 5}
        )
        with engine.connect() as conn:
            conn.execute(sqlalchemy.text("SELECT 1"))
        return True
    except Exception as e:
        print(f"Database connection error: {e}")
        return False

def leaflet_map_card(
            layer_params=[], 
            # layer_params=[
                # {
                #   "LayerGroup": {**layer_group_options},
                #   "Overlay": {**overlay_options},
                #   "Pane": {**pane_options},
                # },
            # ], 
            header="Map",
            id_="leaflet-map",
            legend_params=[],
            # legend_params = [{"title": "Road class", "items": [("Expressway", "#E41A1C"), ("Major Arterial", "#377EB8")]},
            legend_position="bottomright"
        ):
    """Create a standardized Leaflet map card component."""
    layer_groups = [
        dl.Pane(
            children = dl.Overlay(
                children=dl.LayerGroup(**param.get("LayerGroup", {})),
                **param.get("Overlay", {}),
            ),
            **param.get("Pane", {})
        ) for param in layer_params
    ]
    legend_div = legend_from_params(legend_params)
    pos = {
        "topleft":   {"top": "12px",   "left": "12px"},
        "topright":  {"top": "12px",   "right": "12px"},
        "bottomleft":{"bottom": "12px","left": "12px"},
        "bottomright":{"bottom":"12px","right":"12px"},
    }[legend_position]

    legend_overlay = (
        html.Div(
            legend_div,
            id=f"{id_}-legend",
            style={
                "display": "None",  # initially hidden, can be toggled with callback
                "position": "absolute",
                **pos,
                "zIndex": 1000,
                "pointerEvents": "auto",
            },
        )
        if legend_div
        else None
    )

    map_children = [
        dl.Pane(name="popup_pane", style={"zIndex": 1000}),
        dl.LayersControl(
            [
                dl.BaseLayer(dl.TileLayer(), name="OpenStreetMap", checked=False),
                dl.BaseLayer(
                    dl.TileLayer(
                        url="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                        attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/'>CARTO</a>",
                    ),
                    name="Positron",
                    checked=True,
                ),
                dl.BaseLayer(
                    dl.TileLayer(
                        url="https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                        attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/'>CARTO</a>",
                    ),
                    name="Dark Matter",
                ),
                *layer_groups,
            ]
        )
    ]
    if legend_overlay:
        map_children.append(legend_overlay)

    return dbc.Card(
        [
            dbc.CardHeader(html.Strong(header)),
            dbc.CardBody(
                dl.Map(
                    id=id_,
                    center=(39.5, -98.35),  # CONUS
                    zoom=4,
                    children=map_children,
                    style={"height": "60vh", "width": "100%", "borderRadius": "12px"},
                    preferCanvas=True,
                )
            ),
        ],
        className="mb-3 shadow-sm",
    )

def legend_from_params(legend_params):
    """legend_params = [
        {"title": "Road class", "items": [("Expressway", "#E41A1C"), ("Major Arterial", "#377EB8")]},
        {"title": "Crash severity", "items": [("Fatal (K)", "#A65628"), ("Injury (A/B/C)", "#00CED1")]}
    ]"""
    if not legend_params:
        return None

    def section(sec):
        rows = [
            html.Div(
                [
                    html.Span(className="legend-swatch", style={"backgroundColor": color}),
                    html.Span(label, className="legend-label"),
                ],
                className="legend-row",
            )
            for (label, color) in sec.get("items", [])
        ]
        return html.Div([html.Div(sec.get("title", ""), className="legend-title")] + rows,
                        className="legend-section")

    return html.Div([section(sec) for sec in legend_params], className="map-legend")