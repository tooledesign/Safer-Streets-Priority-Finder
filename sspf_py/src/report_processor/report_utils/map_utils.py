import json

import contextily as cx
import geopandas as gpd
import pandas as pd
from matplotlib.patches import Patch
import matplotlib.colors as mcolors
from shapely.geometry import shape

from src.utils import db
from src.utils.mapping import build_color_scheme
from src.global_vars import (
    TOP_CORRIDOR_CONFIG,
    SWA_MAP_CONFIG,
)

DEFAULT_MAP_ASPECT_RATIO = 4/3


def get_study_area_gdf(study_id):
    """Return the dissolved study area boundary for a study as a GeoDataFrame (EPSG:4326)."""

    un, sn = db.get_user_and_study_names(study_id=study_id)
    if not un or not sn:
        return None

    table_name = f"inputs.study_area_{un}_{sn}"
    sql = f"""
        SELECT ST_Transform(ST_Union(geom), 4326) AS geom
        FROM {table_name}
    """

    eng = db.get_database_engine()
    try:
        gdf = gpd.read_postgis(sql, eng, geom_col="geom")
        if gdf.empty or gdf.geometry.iloc[0] is None:
            return None
        return gdf
    except Exception as exc:
        print(f"Error fetching dissolved study area: {exc}")
        return None


def sliding_results_to_gdf(df, geometry_column="geom_geojson", crs="EPSG:4326"):
    """Convert sliding window results dataframe with GeoJSON into a GeoDataFrame."""

    if df is None or df.empty:
        return gpd.GeoDataFrame(columns=[], geometry=[], crs=crs)

    def _to_shape(value):
        if pd.isna(value):
            return None
        geojson = json.loads(value) if isinstance(value, str) else value
        return shape(geojson)

    geometries = df[geometry_column].apply(_to_shape)
    gdf = gpd.GeoDataFrame(df.drop(columns=[geometry_column]), geometry=geometries, crs=crs)
    return gdf[gdf.geometry.notnull()].copy()


def _project_to_web_mercator(gdf):
    if gdf is None or gdf.empty:
        return None
    try:
        return gdf.to_crs(epsg=3857)
    except ValueError:
        if gdf.crs is None:
            gdf = gdf.set_crs(epsg=4326, allow_override=True)
            return gdf.to_crs(epsg=3857)
        raise


def pad_axes_to_ratio(ax, target_ratio=DEFAULT_MAP_ASPECT_RATIO, pad_fraction=0.05):
    xmin, xmax = ax.get_xlim()
    ymin, ymax = ax.get_ylim()

    width = xmax - xmin
    height = ymax - ymin

    if width == 0 or height == 0 or target_ratio == 0:
        return

    current_ratio = width / height

    if current_ratio > target_ratio:
        new_height = width / target_ratio
        delta = new_height - height
        ymin -= delta / 2
        ymax += delta / 2
    else:
        new_width = height * target_ratio
        delta = new_width - width
        xmin -= delta / 2
        xmax += delta / 2

    if pad_fraction:
        width = xmax - xmin
        height = ymax - ymin
        padx = width * pad_fraction
        pady = height * pad_fraction
        xmin -= padx
        xmax += padx
        ymin -= pady
        ymax += pady

    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)


def _plot_study_area(ax, study_wm, style):
    if study_wm is None or study_wm.empty:
        return False

    edge_color = getattr(style, "color", "#ffffff")
    line_weight = getattr(style, "weight", 1)
    face_color = getattr(style, "fillColor", "none")
    face_alpha = getattr(style, "fillOpacity", 0.15)

    study_wm.plot(
        ax=ax,
        facecolor=face_color,
        alpha=face_alpha,
        edgecolor=edge_color,
        linewidth=line_weight,
        zorder=1,
    )
    return True


def _plot_simple_segments(ax, corridors_wm, style, zorder=2):
    if corridors_wm is None or corridors_wm.empty:
        return False

    line_weight = getattr(style, "weight", 2)
    color = getattr(style, "color", "#ad0266")

    corridors_wm.plot(ax=ax, color=color, linewidth=line_weight, zorder=zorder)
    return True


def _to_mpl_color(color_str):
    if isinstance(color_str, str) and color_str.startswith("rgb"):
        try:
            start = color_str.index("(") + 1
            end = color_str.index(")")
            r, g, b = [float(v.strip()) / 255 for v in color_str[start:end].split(",")]
            return (r, g, b)
        except Exception:
            return color_str
    return color_str


def plot_top_corridor_map(
    ax,
    corridors_gdf,
    study_area_gdf=None,
    target_ratio=DEFAULT_MAP_ASPECT_RATIO,
    pad_fraction=0.05,
    basemap_source=None,
):
    basemap_source = basemap_source or cx.providers.CartoDB.DarkMatter

    corridors_wm = _project_to_web_mercator(corridors_gdf)
    study_wm = _project_to_web_mercator(study_area_gdf)

    has_content = False

    if study_wm is not None:
        sa_layer = TOP_CORRIDOR_CONFIG.layers.get("study_area")
        has_content |= _plot_study_area(ax, study_wm, sa_layer.feature_style)

    if corridors_wm is not None:
        tc_layer = TOP_CORRIDOR_CONFIG.layers.get("top_corridors")
        has_content |= _plot_simple_segments(ax, corridors_wm, tc_layer.feature_style)

    if not has_content:
        print("No map content available.")
        return

    ax.autoscale(enable=True)
    pad_axes_to_ratio(ax, target_ratio=target_ratio, pad_fraction=pad_fraction)
    cx.add_basemap(ax, source=basemap_source, attribution=False)
    ax.set_axis_off()


def _plot_mode_segments_map(
    ax,
    mode,
    segments_gdf,
    *,
    value_column,
    study_area_gdf=None,
    show_legend=True,
    legend_title="Crash Score",
    label_formatter=None,
):
    layer_cfg = SWA_MAP_CONFIG.layers.get(mode)
    if layer_cfg is None:
        raise ValueError(f"No mapping style configured for mode '{mode}'.")

    feature_style = layer_cfg.feature_style
    score_col = value_column

    if (
        segments_gdf is None
        or segments_gdf.empty
        or score_col not in segments_gdf.columns
    ):
        print("No map data available for this mode.")
        return

    scheme = build_color_scheme(
        segments_gdf[score_col],
        max_bins=feature_style.max_bins,
        colorscale=feature_style.colorscale,
        label_formatter=label_formatter,
    )
    if scheme is None:
        print("No map content available.")
        return

    gdf = segments_gdf.copy()
    gdf["__bin"] = scheme.bins

    study_wm = _project_to_web_mercator(study_area_gdf)
    segments_wm = _project_to_web_mercator(gdf)

    has_content = False
    sa_layer = SWA_MAP_CONFIG.layers["study_area"]
    if study_wm is not None:
        has_content |= _plot_study_area(ax, study_wm, sa_layer.feature_style)

    if segments_wm is not None and not segments_wm.empty:
        for idx, color in enumerate(scheme.colors):
            subset = segments_wm[segments_wm["__bin"] == idx]
            if subset.empty:
                continue
            mpl_color = _to_mpl_color(color)
            subset.plot(ax=ax, color=mpl_color, linewidth=feature_style.weight, zorder=3)
            has_content = True

    if not has_content:
        print("No map content available.")
        return

    ax.autoscale(enable=True)
    pad_axes_to_ratio(ax, target_ratio=DEFAULT_MAP_ASPECT_RATIO)

    if show_legend and scheme.legend:
        handles = [
            Patch(
                facecolor=_to_mpl_color(item["color"]),
                edgecolor="none",
                label=item["label"],
            )
            for item in scheme.legend
        ]
        legend = ax.legend(
            handles=handles,
            title=legend_title,
            frameon=True,
            fontsize=14,
            title_fontsize=16,
            loc="upper left",
            bbox_to_anchor=(1.02, 1.0),
            borderaxespad=0.2,
        )
        legend.get_frame().set_facecolor((1, 1, 1, 0.5))
        legend.get_frame().set_edgecolor("none")
        legend.get_frame().set_linewidth(0)
        legend.get_frame().set_boxstyle("round,pad=0.4,rounding_size=1")

    # add basemap
    cx.add_basemap(ax, source=cx.providers.CartoDB.DarkMatter, attribution=False)
    ax.set_axis_off()

def plot_sliding_window_map(
    ax,
    mode,
    segments_gdf,
    study_area_gdf=None,
    show_legend=True,
):
    _plot_mode_segments_map(
        ax,
        mode,
        segments_gdf,
        value_column=f"{mode}_crash_score",
        study_area_gdf=study_area_gdf,
        show_legend=show_legend,
        legend_title="Crash Score",
    )


def plot_safer_streets_map(
    ax,
    mode,
    segments_gdf,
    study_area_gdf=None,
    show_legend=True,
):
    formatter = lambda start, end: f"${start:,.0f} - ${end:,.0f}"
    _plot_mode_segments_map(
        ax,
        mode,
        segments_gdf,
        value_column=f"{mode}_cost",
        study_area_gdf=study_area_gdf,
        show_legend=show_legend,
        legend_title="Crash Cost",
        label_formatter=formatter,
    )
