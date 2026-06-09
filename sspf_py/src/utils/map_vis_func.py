# functions related to map visualization pages
import dash_leaflet as dl
from dash import html
import pandas as pd
import numpy as np
import jenkspy
import plotly.express as px
import json
from ..global_vars import *
from . import db, gen

# get crashes as a dash leaflet layer using GeoJSON + canvas rendering
def get_crash_dl_layer(username, study_name, severity_color_dict=SEVERITY_COLORS, priority_cap=MAX_MAP_CRASH_MARKERS):
    """Load all crashes. First *priority_cap* per mode are flagged ``_priority``
    so the client-side filter can show only those at low zoom levels."""
    un, sn = username, study_name
    eng = db.get_database_engine()
    table_name = f"inputs.crashes_{un}_{sn}"

    severity_levels = [sv for sv in CRASH_SEVERITIES.keys() if sv != "Omit From Analysis"]
    mode_levels = [cm for cm in CRASH_MODES.keys() if cm != "Omit From Analysis"]
    where_clause = (
        f"crash_severity IN ({', '.join(repr(sv) for sv in severity_levels)}) "
        f"AND crash_mode IN ({', '.join(repr(cm) for cm in mode_levels)})"
    )

    crashes_df = pd.read_sql(f"""
        SELECT crash_id, crash_year, crash_mode, crash_severity,
               ST_Y(ST_Transform(geom, 4326)) AS lat,
               ST_X(ST_Transform(geom, 4326)) AS lon
        FROM {table_name}
        WHERE {where_clause}
        ORDER BY crash_year DESC NULLS LAST, crash_id DESC;
    """, eng)

    crash_layers = {}   # mode → GeoJSON data dict (FeatureCollection)
    crash_legend = {}
    crash_meta = {}
    for mode in crashes_df["crash_mode"].dropna().unique().tolist():
        mdf = crashes_df[crashes_df["crash_mode"] == mode].copy()
        mode_total = len(mdf)
        mdf["fillColor"] = mdf["crash_severity"].map(severity_color_dict).fillna("#000000")
        mdf["_priority"] = (np.arange(len(mdf)) < priority_cap).tolist()
        # Sort so lower severities draw first and K (fatal) draws on top
        severity_draw_order = {s: i for i, s in enumerate(reversed(SEVERITY_ORDER))}
        mdf = mdf.sort_values(
            by="crash_severity",
            key=lambda col: col.map(severity_draw_order),
            na_position="first",
        )
        props = mdf[["crash_id", "crash_year", "crash_mode", "crash_severity", "fillColor", "_priority"]].to_dict("records")
        lons = mdf["lon"].to_numpy()
        lats = mdf["lat"].to_numpy()
        features = [
            {
                "type": "Feature",
                "geometry": {"type": "Point", "coordinates": [float(lon), float(lat)]},
                "properties": rec,
            }
            for rec, lon, lat in zip(props, lons, lats)
        ]
        crash_layers[mode] = {"type": "FeatureCollection", "features": features}
        crash_legend[mode] = [{"label": k, "color": v} for k, v in SEVERITY_COLORS.items()]
        crash_meta[mode] = {
            "total": mode_total,
            "low_zoom_visible": min(mode_total, priority_cap),
            "is_truncated": mode_total > priority_cap,
        }

    return crash_layers, crash_legend, crash_meta

def get_study_area_dl_layer(username, study_name):
    """
    Generate a dash leaflet layer for the study area boundary.
    """
    un, sn = username, study_name
    eng = db.get_database_engine()
    study_area_df = pd.read_sql(f"""
                            SELECT 
                                ST_AsGeoJSON(ST_Transform(geom, 4326)) AS geom_geojson
                            FROM inputs.study_area_{un}_{sn};
                            """, eng)

    features = []
    for _, row in study_area_df.iterrows():
        features.append({
            "type": "Feature",
            "properties": {},
            "geometry": json.loads(row["geom_geojson"])
        })
    
    study_area_layer = dl.GeoJSON(
        data={"type": "FeatureCollection", "features": features},
        id="study-area-geojson",
        options=dict(
            style=dict(
                color="#000000",
                weight=2,
                fillOpacity=0
            ),
        ),
        zoomToBounds=True
    )

    return study_area_layer

def fatality_bg_dl_layer(username, study_name, bg_color_scale="YlOrBr", max_bins=5):
    """
    Generate dash leaflet layers for fatality block groups with data-driven symbology.
    Creates separate layers for each bin using Jenks natural breaks classification.
    Returns both layers and legend data for each mode.
    """
    un, sn = username, study_name
    input_block_group_fatals = db.get_study_table_names(username=un, study_name=sn)["input_block_group_fatals"]
    eng = db.get_database_engine()
    
    df = pd.read_sql(f"""
        SELECT 
            geoid,
            COALESCE(final_pred_ped, 0) AS final_pred_ped,
            COALESCE(final_pred_bike, 0) AS final_pred_bike,
            COALESCE(final_pred_mv, 0) AS final_pred_mv,
            ST_AsGeoJSON(ST_Transform(geom, 4326)) AS geom_geojson
        FROM {input_block_group_fatals};
    """, eng)

    bg_layers = {}
    bg_legend = {}
    
    for mode in ["bike", "ped", "mv"]:
        scores = df[f"final_pred_{mode}"]
        num_bins = min(max_bins, len(scores.unique()))
        
        # Skip if no data
        if num_bins == 0:
            continue
        
        # Handle single unique value case
        if num_bins == 1:
            bins = pd.Series([0] * len(scores), index=scores.index)
            breaks = [scores.iloc[0]]
            actual_bins = 1
        else:
            breaks = jenkspy.jenks_breaks(scores, n_classes=num_bins)
            breaks = sorted(list(dict.fromkeys(breaks)))
            bins = pd.cut(scores, bins=breaks, labels=False, include_lowest=True)
            actual_bins = len(bins.unique())
        
        # Get brown color ramp
        if actual_bins == 1:
            # For single bin, use a medium brown
            colorscale_obj = px.colors.get_colorscale(bg_color_scale)
            colors = [colorscale_obj[-1][1]]
        else:
            colors = px.colors.sample_colorscale(bg_color_scale, actual_bins, low=0.3, high=1)
        
        # Create legend items
        legend_items = []
        for b in range(actual_bins):
            if b + 1 >= len(breaks):
                bin_label = f"{breaks[b]:.2f}"
            else:
                bin_label = f"{breaks[b]:.2f} - {breaks[b+1]:.2f}"
            legend_items.append({
                "label": bin_label,
                "color": colors[b]
            })
        bg_legend[mode] = legend_items
        
        # Build feature dicts grouped by bin
        mode_df = df[["geoid", f"final_pred_{mode}", "geom_geojson"]].copy()
        mode_df["_bin"] = bins
        mode_df = mode_df.dropna(subset=["_bin"])
        mode_df["_bin"] = mode_df["_bin"].astype(int)
        label_col = f"{MODE_LABELS[mode]} Fatal Estimate"
        mode_df[label_col] = mode_df[f"final_pred_{mode}"].apply(lambda v: f"{v:.2f}")
        prop_cols = ["geoid", label_col, "_bin"]
        records = mode_df[prop_cols].to_dict("records")
        geom_jsons = mode_df["geom_geojson"].tolist()
        bin_features = {b: [] for b in range(actual_bins)}
        for rec, gj in zip(records, geom_jsons):
            b = rec.pop("_bin")
            bin_features[b].append({
                "type": "Feature",
                "properties": rec,
                "geometry": json.loads(gj),
            })

        # Create separate GeoJSON layers for each bin
        mode_layers = []
        for b in range(actual_bins):
            if not bin_features[b]:
                continue

            mode_layers.append(dl.GeoJSON(
                data={"type": "FeatureCollection", "features": bin_features[b]},
                id=f"{mode}-fatality-bg-geojson-bin-{b}",
                options=dict(
                    style=dict(
                        color="#B3B2B26C",
                        weight=0.1,
                        fillColor=colors[b],
                        fillOpacity=0.7
                    ),
                    onEachFeature={"variable": "sspf_onEachFeaturePropertiesPopup"},
                ),
            ))
        
        bg_layers[mode] = mode_layers

    return bg_layers, bg_legend

def build_legend_display(title, legend_data):
    """
    Build a legend display component for the map.
    
    Args:
        title (str): Title for the legend (e.g., "Pedestrian Fatality Block Groups")
        legend_data (list): List of dicts with "label" and "color" keys
    
    Returns:
        html.Div: Formatted legend component
    """
    # Handle edge cases - if legend_data is None or empty
    if not legend_data:
        return None
    
    # If legend_data is a dict (single item), wrap it in a list
    if isinstance(legend_data, dict):
        legend_data = [legend_data]
    
    # If not a list, return None
    if not isinstance(legend_data, list):
        return None
    
    legend_items = []
    for item in legend_data:
        # Skip items that aren't dicts
        if not isinstance(item, dict):
            continue
        
        # Extract label and color with defaults
        label = item.get("label", "Unknown")
        color = item.get("color", "#808080")
        
        legend_items.append(
            html.Div(
                [
                    html.Div(
                        style={"display": "inline-block", "width": "18px", "height": "18px", "backgroundColor": color, "marginRight": "8px", "border": "1px solid rgba(100, 100, 100, 0.5)", "borderRadius": "2px"}
                    ),
                    html.Span(str(label), style={"fontSize": "12px"})
                ],
                style={"display": "flex", "alignItems": "center", "marginBottom": "4px"}
            )
        )
    
    if not legend_items:
        return None
    
    legend_content = html.Div(
        [
            html.H6(title, style={"marginBottom": "10px", "marginTop": "0"}),
            html.Div(legend_items)
        ]
    )
    
    return legend_content

def visualization_leaflet_map():
    """
    Generate an empty Leaflet map study data and results visualization.
    The layers will be populated via callbacks based on user selections.
    """
    id_prefix = "map-vis"
    # setup base map tiles
    map_children = [
        dl.Pane(name="popup_pane", style={"zIndex": 1000}),
        html.Div(
            id=f"{id_prefix}-layers-control-div",
        ),
    ]

    # Keep all dynamic content on one shared canvas so hit-testing remains intact.
    # The clientside callback rebuilds feature order as fatality BGs → roads → crashes.
    map_children.append(dl.GeoJSON(
        id=f"{id_prefix}-content-geojson",
        data={"type": "FeatureCollection", "features": []},
        options=dict(
            style={"variable": "sspf_mapVisContentStyle"},
            filter={"variable": "sspf_mapVisContentFilter"},
            pointToLayer={"variable": "sspf_mapVisContentPointToLayer"},
            onEachFeature={"variable": "sspf_mapVisContentOnEachFeature"},
        ),
        hideout={"showDetail": False},
    ))
    
    # Base legend style
    legend_base_style = {
        "position": "absolute",
        "zIndex": 1000,
        "pointerEvents": "auto",
        "backgroundColor": "white",
        "padding": "12px",
        "borderRadius": "4px",
        "boxShadow": "0 0 15px rgba(0, 0, 0, 0.2)",
        "maxHeight": "300px",
        "overflowY": "auto",
    }
    
    # Add three legend containers in different corners
    # Top-right
    legend_fatality_div = html.Div(
        id=f"{id_prefix}-legend-fatality",
        style={**legend_base_style, "display": "none", "top": "12px", "right": "12px"},
        children=[]
    )
    map_children.append(legend_fatality_div)
    
    # Bottom-left
    legend_roads_div = html.Div(
        id=f"{id_prefix}-legend-roads",
        style={**legend_base_style, "display": "none", "bottom": "12px", "left": "12px"},
        children=[]
    )
    map_children.append(legend_roads_div)
    
    # Bottom-right
    legend_crashes_div = html.Div(
        id=f"{id_prefix}-legend-crashes",
        style={**legend_base_style, "display": "none", "bottom": "12px", "right": "12px"},
        children=[]
    )
    map_children.append(legend_crashes_div)
    
    map = dl.Map(
                id=f"{id_prefix}-leaflet-map",
                center=(39.5, -98.35),  # CONUS
                zoom=4,
                children=map_children,
                style={"height": "60vh", "width": "100%", "borderRadius": "12px"},
                preferCanvas=True,
            )

    return map
