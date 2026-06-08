# from ..utils import db, analysis
from . import db, analysis
import sqlalchemy
import pandas as pd
import numpy as np
import json
import geopandas as gpd
from io import StringIO
from ..global_vars import *
import plotly.express as px
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from dash import dash_table


# =========================
# Global config variables
# =========================

CATEGORY_ORDER_BY_COL = {
    "crash_severity": SEVERITY_ORDER,
    "kabco": SEVERITY_ORDER,       # if you use "kabco" as a derived grouping
    "crash_mode": MODE_ORDER,
}

# =========================
# Helpers
# =========================

def get_dashboard_data(study_id):
    """
    Retrieve data for the dashboard from database.
    Get the crashes and roadways data as 
    """
    eng = db.get_database_engine()
    un, sn = db.get_user_and_study_names(study_id=study_id)
    crash_join_dist = pd.read_sql(
        sql=f"SELECT crash_join_distance FROM gen_management.studies WHERE study_id = {study_id}", 
        con=eng
    ).iloc[0,0] * 0.3048  # feet to meters
    
    input_crashes_table = f"inputs.crashes_{un}_{sn}"
    input_roads_table = f"inputs.roads_{un}_{sn}"

    summary_df = pd.read_sql(
        sql=f"""
            SELECT
                x.*,
                COALESCE(m.mileage, 0) AS mileage
            FROM
                (
                    SELECT
                        t.crash_year,
                        t.crash_mode,
                        t.crash_severity,
                        t.functional_class,
                        COUNT(t.crash_id) AS crash_count
                    FROM
                        (
                            SELECT DISTINCT ON (crash_id)
                                c.crash_id,
                                c.crash_year, 
                                c.crash_mode, 
                                c.crash_severity,
                                r.road_name,
                                r.functional_class
                            FROM
                                {input_crashes_table} c
                                LEFT JOIN 
                                {input_roads_table} r
                                ON
                                ST_DWithin(c.geom, r.geom, {crash_join_dist})
                                AND
                                r.functional_class != 'Omit From Analysis'
                            WHERE
                                c.crash_severity != 'Omit From Analysis'
                                AND
                                c.crash_mode != 'Omit From Analysis'
                                AND
                                c.crash_year >= (SELECT MAX(crash_year) - 4 FROM {input_crashes_table})
                            ORDER BY c.crash_id, ST_Distance(c.geom, r.geom) ASC
                        ) t
                    GROUP BY
                        t.crash_year, t.crash_mode, t.crash_severity, t.functional_class
                ) x
                LEFT JOIN
                (
                    SELECT
                        functional_class,
                        SUM(ST_Length(geom)) / 1609.34 AS mileage
                    FROM {input_roads_table}
                    GROUP BY functional_class
                ) m
                ON 
                x.functional_class = m.functional_class
        """,
        con=eng
    )

    return summary_df

def get_filter_options(df, column, include_all=True):
    """
    Get filter options for a given column from the dashboard data.
    """
    unique_values = sorted(df[column].dropna().unique())

    if column not in ["crash_severity", "crash_mode"]:
        raise ValueError("Column must be one of 'crash_severity' or 'crash_mode'.")

    options = []
    if include_all:
        options.append({"label": f"All Crashes", "value": f"All Crashes"})
    
    labels_dict = SEVERITY_LABELS if column == "crash_severity" else MODE_LABELS
    for val, label in labels_dict.items():
        if val in unique_values:
            options.append({"label": label, "value": val})

    return options

def get_year_range(df):
    """
    Get the min and max crash years from the dashboard data.
    """
    min_year = df["crash_year"].min()
    max_year = df["crash_year"].max()
    return (min_year, max_year)

def df_to_store(df):
    "Convert a dataframe to JSON for dcc.Store"
    return df.to_json(orient="split")

def store_to_df(data):
    "Convert JSON from dcc.Store back to a dataframe"
    if data is None:
        return pd.DataFrame()
    return pd.read_json(StringIO(data), orient="split")

def _readable_txt(text):
    "Make text more readable by replacing underscores with spaces and capitalizing words."
    return text.replace("_", " ").title()

def make_summary(df, group_col, order=None):
    # summary = (
    #     df.groupby(group_col)
    #       .size()
    #       .reset_index(name="Total Crashes")
    #       .rename(columns={group_col: "Category"})
    # )
    group_col_readable = _readable_txt(group_col)
    summary = df.groupby(group_col).agg(**{"Total Crashes": ("crash_count", "sum")}).reset_index().rename(columns={group_col: group_col_readable})
    total = summary["Total Crashes"].sum()
    summary["Percent of Total"] = (summary["Total Crashes"] / total * 100).round(1)

    if order is not None:
        summary[group_col_readable] = pd.Categorical(summary[group_col_readable], categories=order, ordered=True)
        summary = summary.sort_values(group_col_readable)

    return summary

def default_plotly_table_trace(df, *, align_left_cols=("Severity",), header_overrides=None, cell_overrides=None):
    """
    Returns a go.Table trace with a consistent default style.
    - df: dataframe to render
    - align_left_cols: columns to left-align; everything else right-align
    - header_overrides / cell_overrides: dicts to override defaults if needed
    """
    header_overrides = header_overrides or {}
    cell_overrides = cell_overrides or {}

    cols = list(df.columns)

    # alignment: left for label-ish columns, right for numeric columns
    align = ["left" if c in align_left_cols else "right" for c in cols]

    header = dict(
        values=cols,
        fill_color="#f5f5f5",
        align=align,
        font=dict(size=13),
        line_color="black",
        line_width=1,
        height=30,
        **header_overrides,
    )

    # zebra striping for rows
    n = len(df)
    zebra = ["white" if i % 2 == 0 else "#f9f9f9" for i in range(n)]
    fill_color = [zebra for _ in cols]  # must be column-major

    cells = dict(
        values=[df[c].tolist() for c in cols],
        fill_color=fill_color,
        align=align,
        font=dict(size=12),
        line_color="#dddddd",
        height=26,
        **cell_overrides,
    )

    return go.Table(header=header, cells=cells)
    
# =========================
# Main functions
# =========================

def create_donut_fig_table(df, group_col, y0=None, y1=None, filter="All Crashes"):
    """
    Creates a donut chart figure and summary table dataframe for the dashboard.
    
    :param df: input dataframe
    :param group_col: column to group by ("crash_mode" or "crash_severity")
    :param y0: start year (inclusive)
    :param y1: end year (inclusive)
    :param filter: filter value for the other column ("All Crashes" or specific code)
    
    :return: summary dash table and plotly Figure
    """
    if y0 is None:
        y0 = int(df["crash_year"].min())
    if y1 is None:
        y1 = int(df["crash_year"].max())

    dff = df[(df["crash_year"] >= int(y0)) & (df["crash_year"] <= int(y1))]

    # only doing donut chart for crash by mode and crash by severity
    if group_col not in ["crash_mode", "crash_severity"]:
        raise ValueError("group_col must be 'crash_mode' or 'crash_severity'.")
    # ensure filter column aligns with what is expected for group_col
    if group_col == "crash_mode":
        if filter not in ["All Crashes"] + SEVERITY_ORDER:
            raise ValueError(f"filter must be one of 'All Crashes' or valid crash severity code: ({', '.join(SEVERITY_ORDER)}).")
        filter_order = SEVERITY_ORDER
        filter_labels = SEVERITY_LABELS
        filter_col = "crash_severity"
        group_col_order = MODE_ORDER
        fig_title = "Crashes by Mode"
        fig_colors = MODE_COLORS
        fig_labels = MODE_LABELS
    elif group_col == "crash_severity":
        if filter not in ["All Crashes"] + MODE_ORDER:
            raise ValueError(f"filter must be one of 'All Crashes' or valid crash mode code: ({', '.join(MODE_ORDER)}).")
        filter_order = MODE_ORDER
        filter_labels = MODE_LABELS
        filter_col = "crash_mode"
        group_col_order = SEVERITY_ORDER
        fig_title = "Crashes by Severity"
        fig_colors = SEVERITY_COLORS
        fig_labels = SEVERITY_LABELS

    if filter not in (None, "All Crashes"):
        dff = dff[dff[filter_col] == str(filter).strip()]

    group_col_readable = _readable_txt(group_col)
    summary = make_summary(df=dff, group_col=group_col, order=group_col_order)

    if summary.empty:
        fig = go.Figure()
        fig.update_layout(
            title=fig_title,
            annotations=[dict(text="No data for this selection.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        empty_table = dash_table.DataTable(
            data=[],
            columns=[{"name": "\xa0" + c, "id": c} for c in summary.columns],
        )
        return empty_table, fig

    colors = [fig_colors.get(cat, "#cccccc") for cat in summary[group_col_readable]]

    fig = go.Figure(
        go.Pie(
            labels=[fig_labels.get(cat, cat) for cat in summary[group_col_readable]],
            values=summary["Total Crashes"],
            hole=0.6,
            textinfo="percent+label",
            sort=False,
            marker=dict(colors=colors),
            showlegend=True,
        )
    )

    filter_title = f"{filter_labels[filter]} Crashes" if filter not in (None, "All Crashes") else "All Crashes"
    filter_title += f" ({y0}-{y1})"
    fig.update_layout(
        title=f"<b>{filter_title}</b>",
        margin=dict(l=10, r=10, t=50, b=10),
        paper_bgcolor="white",
        plot_bgcolor="white",
        height=400,
        legend=dict(orientation="h", yanchor="top", y=-0.08, xanchor="left", x=0),
        uirevision=fig_title,
    )

    # recode grouped column values to labels
    summary[group_col_readable] = summary[group_col_readable].map(fig_labels)
    summary["Percent of Total"] = summary["Percent of Total"].apply(lambda x: f"{x:.1f}%")

    summary_table = dash_table.DataTable(
        data=summary.to_dict('records'),
        columns=[{"name": "\xa0" + i, "id": i} for i in summary.columns],
        sort_action="native",
        style_table={'overflowX': 'auto'},
        style_cell={'textAlign': 'left'},
        style_header={'fontWeight': 'bold'},
        export_format="csv",
    )

    return summary_table, fig

def observed_vs_estimated_bar(df):
    """
    Grouped horizontal bar chart: observed crashes vs modeled adjusted crashes.
    df must have columns: Severity, Observed Crashes, Modeled Adjusted Crashes.
    """
    if df is None or df.empty:
        fig = go.Figure()
        fig.update_layout(
            annotations=[dict(text="No data available.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        return fig

    fig = go.Figure()
    fig.add_trace(
        go.Bar(
            name="Observed Crashes",
            y=df["Severity"],
            x=df["Observed Crashes"],
            orientation="h",
            marker=dict(color=OBS_VS_EST_COLORS["Observed Crashes"]),
            hovertemplate="<b>%{y}</b><br>Observed: %{x:,.0f}<extra></extra>",
        )
    )
    fig.add_trace(
        go.Bar(
            name="Modeled Adjusted Crashes",
            y=df["Severity"],
            x=df["Modeled Adjusted Crashes"],
            orientation="h",
            marker=dict(color=OBS_VS_EST_COLORS["Modeled Adjusted Crashes"]),
            hovertemplate="<b>%{y}</b><br>Modeled: %{x:,.1f}<extra></extra>",
        )
    )

    n_bars = len(df)
    bar_height = 100
    chart_height = max(300, n_bars * bar_height + 140)

    fig.update_layout(
        barmode="group",
        bargap=0.25,
        height=chart_height,
        margin=dict(l=160, r=20, t=40, b=40),
        paper_bgcolor="white",
        plot_bgcolor="white",
        legend=dict(
            orientation="h",
            yref="container",
            yanchor="bottom",
            y=0,
            xanchor="left",
            x=0,
        ),
        uirevision="obs-vs-est",
    )
    fig.update_xaxes(title_text="Crash Count")
    fig.update_yaxes(title_text="", autorange="reversed")

    return fig


def mode_by_severity_stacked_components(df, y0=None, y1=None):
    """
    100% stacked horizontal bar:
      - y-axis: severity
      - stacked segments: mode
      - x-axis: percent
    NOTE: intentionally NOT filtered by selected mode
    """
    if y0 is None:
        y0 = int(df["crash_year"].min())
    if y1 is None:
        y1 = int(df["crash_year"].max())

    dff = df[(df["crash_year"] >= int(y0)) & (df["crash_year"] <= int(y1))]

    if dff.empty:
        fig = go.Figure()
        fig.update_layout(
            title="Crashes by Severity and Mode",
            annotations=[dict(text="No data for this selection.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        return dash_table.DataTable(columns=[], data=[]), fig

    agg = (
        dff.groupby(["crash_severity", "crash_mode"])
           .agg(**{"Count": ("crash_count", "sum")})
           .reset_index()
    )

    pivot = (
        agg.pivot(index="crash_severity", columns="crash_mode", values="Count")
           .fillna(0)
    )

    # order rows by SEVERITY_ORDER, but only include severities present in data
    present_severities = [s for s in SEVERITY_ORDER if s in pivot.index]
    pivot = pivot.reindex(present_severities)

    # fixed mode order, but keep any extras at the end
    ordered = [m for m in MODE_ORDER if m in pivot.columns]
    extras = [m for m in pivot.columns if m not in MODE_ORDER]
    pivot = pivot[ordered + extras]

    totals = pivot.sum(axis=1).replace(0, np.nan)
    pct = (pivot.div(totals, axis=0) * 100).fillna(0)

    y_labels = [SEVERITY_LABELS.get(s, s) for s in pct.index.tolist()]

    # --- Create summary table ---
    table_df = pivot.astype(int).copy()
    table_df["Total"] = table_df.sum(axis=1)
    
    # Add "All Severities" total row
    all_sev_row = table_df.sum(numeric_only=True)
    all_sev_row.name = "All Severities"
    table_df = pd.concat([table_df, all_sev_row.to_frame().T])
    
    # Format values as "Count (Pct%)" before renaming columns
    numeric_cols = [c for c in table_df.columns if c != "Total"]
    for col in numeric_cols:
        table_df[col] = table_df.apply(
            lambda row, c=col: (
                f"{int(row[c])} ({row[c]/row['Total']*100:.1f}%)"
                if row['Total'] > 0 else "0 (0.0%)"
            ),
            axis=1
        )
    
    # Now rename columns and reshape
    table_df = (
        table_df.rename(index=SEVERITY_LABELS)
             .rename(columns=MODE_LABELS)
             .reset_index()
             .rename(columns={"index": "Severity"})
    )
    # add pct value to the Total column
    table_df["Total"] = table_df.apply(lambda row: f"{row['Total']} (100%)", axis=1)

    summary_table = dash_table.DataTable(
        data=table_df.to_dict('records'),
        columns=[{"name": "\xa0" + i, "id": i} for i in table_df.columns],
        style_table={'overflowX': 'auto'},
        style_cell={'textAlign': 'left'},
        style_header={'fontWeight': 'bold'},
        export_format="csv",
    )

    # --- Create figure ---
    fig = go.Figure()
    
    # Track which modes we've already added to legend
    added_to_legend = set()
    
    # Add individual severity bars
    for m in pct.columns.tolist():
        show_in_legend = m not in added_to_legend
        if show_in_legend:
            added_to_legend.add(m)
            
        fig.add_trace(
            go.Bar(
                name=MODE_LABELS.get(m, m),
                y=y_labels,
                x=pct[m].tolist(),
                orientation="h",
                marker=dict(color=MODE_COLORS.get(m, "#cccccc")),
                customdata=[[MODE_LABELS.get(m, m), sev] for sev in y_labels],
                hovertemplate="<b>%{customdata[0]}</b><br>%{customdata[1]}<br>Share: %{x:.1f}%<extra></extra>",
                legendgroup=MODE_LABELS.get(m, m),
                showlegend=show_in_legend
            )
        )
    
    # Add "All Severities" bar (totals across all severity codes)
    # Each mode needs its own trace to stack properly
    all_sev_pct = (pivot.sum(axis=0) / pivot.sum().sum() * 100).fillna(0)
    for m in pct.columns.tolist():
        fig.add_trace(
            go.Bar(
                name=MODE_LABELS.get(m, m),
                y=["All Severities"],
                x=[all_sev_pct[m]],
                orientation="h",
                marker=dict(color=MODE_COLORS.get(m, "#cccccc")),
                customdata=[[MODE_LABELS.get(m, m), "All Severities"]],
                hovertemplate="<b>%{customdata[0]}</b><br>%{customdata[1]}<br>Share: %{x:.1f}%<extra></extra>",
                legendgroup=MODE_LABELS.get(m, m),
                showlegend=False
            )
        )

    fig.update_layout(
        title=f"<b>All Crashes ({y0}-{y1})</b>",
        barmode="stack",
        bargap=0.25,
        margin=dict(l=80, r=20, t=60, b=100),
        paper_bgcolor="white",
        plot_bgcolor="white",
        legend=dict(orientation="h", yanchor="bottom", y=-0.35, xanchor="left", x=0),
        # legend_title_text="Mode",
        uirevision="mode-by-severity-stacked",
    )
    fig.update_xaxes(title_text="Percent of crashes (within severity)", range=[0, 100])
    fig.update_yaxes(title_text="", autorange="reversed")

    return summary_table, fig

def funclass_stacked_components(df, y0=None, y1=None, filtered_mode="All Crashes"):
    """
    === would be better if it's based on crash per mile ===
    100% stacked horizontal bar:
      - y-axis: severity
      - stacked segments: functional class
      - x-axis: percent within severity
    """

    if y0 is None:
        y0 = int(df["crash_year"].min())
    if y1 is None:
        y1 = int(df["crash_year"].max())

    dff = df[(df["crash_year"] >= int(y0)) & (df["crash_year"] <= int(y1))]

    if filtered_mode not in (None, "All Crashes"):
        dff = dff[dff["crash_mode"] == str(filtered_mode).strip()]

    if dff.empty:
        fig = go.Figure()
        fig.update_layout(
            title="Crashes by Functional Classification and Severity",
            annotations=[dict(text="No data for this selection.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        return dash_table.DataTable(columns=[], data=[]), fig

    agg = (
        dff.groupby(["crash_severity", "functional_class"])
           .agg(**{"Count": ("crash_count", "sum")})
           .reset_index()
    )

    pivot = (
        agg.pivot(index="crash_severity", columns="functional_class", values="Count")
           .fillna(0)
    )

    # order rows by SEVERITY_ORDER, but only include severities present in data
    present_severities = [s for s in SEVERITY_ORDER if s in pivot.index]
    pivot = pivot.reindex(present_severities)

    # enforce functional-class order (keep extras at end so it never blanks out)
    ordered = [fc for fc in FUNCLASS_ORDER if fc in pivot.columns]
    extras = [fc for fc in pivot.columns if fc not in FUNCLASS_ORDER]
    pivot = pivot[ordered + extras]

    totals = pivot.sum(axis=1).replace(0, np.nan)
    pct = (pivot.div(totals, axis=0) * 100).fillna(0)

    y_labels = [SEVERITY_LABELS.get(s, s) for s in pct.index.tolist()]

    # --- Create summary table ---
    table_df = pivot.astype(int).copy()
    table_df["Total"] = table_df.sum(axis=1)
    
    # Add "All Severities" total row
    all_sev_row = table_df.sum(numeric_only=True)
    all_sev_row.name = "All Severities"
    table_df = pd.concat([table_df, all_sev_row.to_frame().T])
    
    # Format values as "Count (Pct%)" before renaming columns
    numeric_cols = [c for c in table_df.columns if c != "Total"]
    for col in numeric_cols:
        table_df[col] = table_df.apply(
            lambda row, c=col: (
                f"{int(row[c])} ({row[c]/row['Total']*100:.1f}%)"
                if row['Total'] > 0 else "0 (0.0%)"
            ),
            axis=1
        )
    
    # Now rename columns and reshape
    table_df = (
        table_df.rename(index=SEVERITY_LABELS)
             .reset_index()
             .rename(columns={"index": "Severity"})
    )
    # add pct value to the Total column
    table_df["Total"] = table_df.apply(lambda row: f"{row['Total']} (100%)", axis=1)

    summary_table = dash_table.DataTable(
        data=table_df.to_dict('records'),
        columns=[{"name": "\xa0" + i, "id": i} for i in table_df.columns],
        style_table={'overflowX': 'auto'},
        style_cell={'textAlign': 'left'},
        style_header={'fontWeight': 'bold'},
        export_format="csv",
    )

    # --- Create figure ---
    fig = go.Figure()

    # Track which functional classes we've already added to legend
    added_to_legend = set()
    
    # Add individual severity bars
    for fc in pct.columns.tolist():
        show_in_legend = fc not in added_to_legend
        if show_in_legend:
            added_to_legend.add(fc)
            
        fig.add_trace(
            go.Bar(
                name=str(fc),
                y=y_labels,
                x=pct[fc].tolist(),
                orientation="h",
                marker=dict(color=FCLASS_COLORS[fc]),
                customdata=[[fc, yl] for yl in y_labels],
                hovertemplate=(
                    "<b>%{customdata[0]}</b><br>"
                    "Severity: %{customdata[1]}<br>"
                    "Share: %{x:.1f}%<extra></extra>"
                ),
                legendgroup=str(fc),
                showlegend=show_in_legend
            )
        )
    
    # Add "All Severities" bar (totals across all severity codes)
    # Each functional class needs its own trace to stack properly
    all_sev_pct = (pivot.sum(axis=0) / pivot.sum().sum() * 100).fillna(0)
    for fc in pct.columns.tolist():
        fig.add_trace(
            go.Bar(
                name=str(fc),
                y=["All Severities"],
                x=[all_sev_pct[fc]],
                orientation="h",
                marker=dict(color=FCLASS_COLORS[fc]),
                customdata=[[fc, "All Severities"]],
                hovertemplate=(
                    "<b>%{customdata[0]}</b><br>"
                    "Severity: %{customdata[1]}<br>"
                    "Share: %{x:.1f}%<extra></extra>"
                ),
                legendgroup=str(fc),
                showlegend=False
            )
        )

    fig_title = f"{MODE_LABELS[filtered_mode]} Crashes" if filtered_mode not in (None, "All Crashes") else "All Crashes"
    fig_title += f" ({y0}-{y1})"
    fig.update_layout(
        title=f"<b>{fig_title}</b>",
        barmode="stack",
        bargap=0.25,
        margin=dict(l=160, r=20, t=60, b=100),
        paper_bgcolor="white",
        plot_bgcolor="white",
        legend=dict(orientation="h", yanchor="bottom", y=-0.35, xanchor="left", x=0),
        uirevision="severity-by-funclass-stacked",
    )

    fig.update_xaxes(title_text="Percent of crashes (within severity)", range=[0, 100])
    fig.update_yaxes(title_text="", autorange="reversed")

    return summary_table, fig

def severity_year_grouped_components(df, y0=None, y1=None, filtered_mode="All Crashes"):
    """
    Grouped bar chart with 2 rows:
      - row 1: K and A severity
      - row 2: B, C, O severity
    Also returns a summary table dataframe.
    """

    if y0 is None: y0 = int(df["crash_year"].min())
    if y1 is None: y1 = int(df["crash_year"].max())

    dff = df[(df["crash_year"] >= y0) & (df["crash_year"] <= y1)]

    if filtered_mode not in (None, "All Crashes"):
        dff = dff[dff["crash_mode"] == str(filtered_mode).strip()]

    if dff.empty:
        fig = go.Figure()
        fig.update_layout(
            # title="Crashes by Severity and Year",
            # annotations=[dict(text="No data for this selection.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        return dash_table.DataTable(columns=[], data=[]), fig

    # --- aggregate ---
    agg = (
        dff.groupby(["crash_severity", "crash_year"])
           .agg(**{"Count": ("crash_count", "sum")})
           .reset_index()
    )

    table = (
        agg.pivot(index="crash_severity", columns="crash_year", values="Count")
           .fillna(0)
    )
    
    # only include severities present in data
    present_severities = [s for s in SEVERITY_ORDER if s in table.index]
    table = table.reindex(present_severities).fillna(0).astype(int)

    table_df = (
        table.rename(index=SEVERITY_LABELS)
             .reset_index()
             .rename(columns={"crash_severity": "Severity"})
    )

    years = sorted(table.columns.tolist())

    # --- figure (2 rows only now) ---
    fig = make_subplots(
        rows=2, cols=1,
        specs=[[{"type": "xy"}], [{"type": "xy"}]],
        row_heights=[0.5, 0.5],
        vertical_spacing=0.150,
        subplot_titles=("Serious Injury Crashes (K/A)", "Other Crashes (B/C/O)")
    )

    fsi_codes = ["K", "A"]
    other_codes = ["B", "C", "O"]
    
    # Only include severities that are present in the data
    present_fsi = [s for s in fsi_codes if s in table.index]
    present_other = [s for s in other_codes if s in table.index]

    for sev in present_fsi:
        fig.add_bar(
            x=years,
            y=[table.loc[sev, y] for y in years],
            # width=0.6,
            name=SEVERITY_LABELS[sev],
            marker_color=SEVERITY_COLORS[sev],
            row=1,
            col=1,
        )

    for sev in present_other:
        fig.add_bar(
            x=years,
            y=[table.loc[sev, y] for y in years],
            name=SEVERITY_LABELS[sev],
            marker_color=SEVERITY_COLORS[sev],
            row=2,
            col=1,
        )

    # Calculate max values for each subplot to set appropriate tick intervals
    max_fsi = max([table.loc[s, y] for s in present_fsi for y in years]) if present_fsi else 0
    max_other = max([table.loc[s, y] for s in present_other for y in years]) if present_other else 0
    
    fig.update_layout(
        barmode="group",
        paper_bgcolor="white",
        plot_bgcolor="white",
        height=600,
        legend=dict(
            orientation="h",
            yanchor="top",
            y=-0.12,
            xanchor="left",
            x=0,
        ),
        uirevision="severity-year",
    )

    # Set tick interval based on max value to avoid duplicate labels
    dtick_fsi = 1 if max_fsi <= 10 else None
    dtick_other = 1 if max_other <= 10 else None
    
    fig.update_yaxes(title_text="Crash Count", showgrid=True, gridwidth=0.5, gridcolor="#eeeeee", 
                     tickformat="d", rangemode="nonnegative", dtick=dtick_fsi, row=1, col=1)
    fig.update_yaxes(title_text="Crash Count", showgrid=True, gridwidth=0.5, gridcolor="#eeeeee", 
                     tickformat="d", rangemode="nonnegative", dtick=dtick_other, row=2, col=1)

    summary_table = dash_table.DataTable(
        data=table_df.to_dict('records'),
        columns=[{"name": "\xa0" + str(i), "id": str(i)} for i in table_df.columns],
        style_table={'overflowX': 'auto'},
        style_cell={'textAlign': 'left'},
        style_header={'fontWeight': 'bold'},
        export_format="csv",
    )

    return summary_table, fig

def fclass_rate_chart_components(df, y0=None, y1=None, filtered_mode="All Crashes"):
    """
    Create a chart of crash rates (per mile) by functional class and mode.
    
    :param df: summary dataframe
    :param y0: start year (inclusive)
    :param y1: end year (inclusive)
    :param filtered_mode: filter by crash mode (default: "All Crashes")
    """
    return True

def get_top_corridors_gdf(study_id, mode):
    """
    Retrieve top crash corridors for the study for a specific mode as a GeoDataFrame.
    """
    un, sn = db.get_user_and_study_names(study_id=study_id)
    sliding_windows_table = db.get_study_table_names(username=un, study_name=sn)["sliding_windows_table"]

    # check table exists first
    if not db.table_exists(table_name=sliding_windows_table.split(".")[1], schema=sliding_windows_table.split(".")[0]):
        return gpd.GeoDataFrame()
    
    eng = db.get_database_engine()
    # get the number of corridors to return
    try:
        n_corridors = pd.read_sql(
            sql=f"SELECT top_streets_threshold FROM gen_management.studies WHERE study_id = {study_id}",
            con=eng
        ).iloc[0,0]
    except:
        n_corridors = 10 

    sql = f"""
        SELECT
            INITCAP(d.road_name) AS road_name,
            d.functional_class,
            d.crash_score,
            st_transform(d.geom, 4326) as geom
        FROM
            (
                SELECT DISTINCT ON (road_name, functional_class)
                    road_name,
                    functional_class,
                    {mode}_crash_score AS crash_score,
                    geom,
                    "length"
                FROM
                    {sliding_windows_table}
                WHERE
                    {mode}_crash_score > 0
                ORDER BY
                    road_name,
                    functional_class,
                    {mode}_crash_score DESC,
                    "length" DESC
            ) d
        ORDER BY
            d.crash_score DESC,
            d."length" DESC
        LIMIT {n_corridors}
    """
    
    gdf = gpd.read_postgis(sql, eng, geom_col="geom")
    return gdf

def get_top_corridors_fc(study_id):
    """
    Retrieve top crash corridors for the study for each mode. 
    Return a dictionary of feature collections keyed by mode.
    """
    mode_fc = {}
    for m in MODE_LABELS.keys():
        gdf = get_top_corridors_gdf(study_id, m)
        
        if gdf is None or gdf.empty:
            continue
            
        # Rename columns to match what dashboard expects in properties
        gdf_out = gdf.rename(columns={
            "road_name": "Road Name",
            "functional_class": "Functional Class",
            "crash_score": "Crash Score"
        })
        
        # Convert to FeatureCollection dict
        # to_json() returns a string, json.loads turns it back to dict
        mode_fc[m] = json.loads(gdf_out.to_json())

    return mode_fc

# ======= Crash by Functional Class Dashboard ========
# components function
def funclass_components(df_raw, y0=None, y1=None, filtered_mode="All Crashes"):

    df = df_raw

    if y0 is None:
        y0 = int(df["crash_year"].min())
    if y1 is None:
        y1 = int(df["crash_year"].max())

    dff = df[(df["crash_year"] >= int(y0)) & (df["crash_year"] <= int(y1))]

    if filtered_mode not in (None, "All Crashes"):
        dff = dff[dff["crash_mode"] == str(filtered_mode).strip()]

    if dff.empty:
        fig = go.Figure()
        fig.update_layout(
            title="Crashes by Functional Classification",
            annotations=[dict(text="No data for this selection.", x=0.5, y=0.5, showarrow=False)],
            paper_bgcolor="white",
        )
        return fig, None

    # --- aggregate by functional class + crash severity ---
    agg = (
        dff.groupby(["crash_severity", "functional_class"])
           .agg(**{"Count": ("crash_count", "sum")})
           .reset_index()
    )

    table = (
        agg.pivot(index="crash_severity", columns="functional_class", values="Count")
           .fillna(0)
           .reindex(SEVERITY_ORDER)
           .fillna(0)
           .astype(int)
    )

    # ---- Dash table df (severity x funclass wide) ----
    table_df = (
        table.rename(index=SEVERITY_LABELS)
             .reset_index()
             .rename(columns={"index": "Severity"})
    )

    # ordered = [fc for fc in FUNCLASS_ORDER if fc in table_df.columns]
    # extras = [c for c in table_df.columns if c not in (["Severity"] + FUNCLASS_ORDER)]
    # table_df = table_df[["Severity"] + ordered + extras]

    funclass = [fc for fc in FUNCLASS_ORDER if fc in table.columns]
  

    # --- subplots: 2 charts ---
    fig = make_subplots(
        rows=2, cols=1,
        specs=[[{"type": "xy"}], [{"type": "xy"}]],
        row_heights=[0.45, 0.55],
        vertical_spacing=0.5,
    )

    fsi_codes = ["K", "A"]
    other_codes = ["B", "C", "O"]

    def multi_x(sev_code):
        sev_label = SEVERITY_LABELS.get(sev_code, sev_code)
        return [[sev_label] * len(funclass), funclass]

    for sev in fsi_codes:
        fig.add_trace(
            go.Bar(
                x=multi_x(sev),
                y=[int(table.loc[sev, c]) for c in funclass],
                name=SEVERITY_LABELS.get(sev, sev),
                marker=dict(color=SEVERITY_COLORS.get(sev, "#cccccc"), line=dict(width=0)),
                customdata=[[SEVERITY_LABELS.get(sev, sev), c] for c in funclass],
                hovertemplate="<b>%{customdata[0]}</b><br>Functional Class: %{customdata[1]}<br>Crashes: %{y}<extra></extra>",
            ),
            row=1, col=1,
        )

    for sev in other_codes:
        fig.add_trace(
            go.Bar(
                x=multi_x(sev),
                y=[int(table.loc[sev, c]) for c in funclass],
                name=SEVERITY_LABELS.get(sev, sev),
                marker=dict(color=SEVERITY_COLORS.get(sev, "#cccccc"), line=dict(width=0)),
                customdata=[[SEVERITY_LABELS.get(sev, sev), c] for c in funclass],
                hovertemplate="<b>%{customdata[0]}</b><br>Functional Class: %{customdata[1]}<br>Crashes: %{y}<extra></extra>",
            ),
            row=2, col=1,
        )

    fig.update_yaxes(title_text="FSI Crashes (K + A)", row=1, col=1)
    fig.update_yaxes(title_text="Other Crashes (B + C + O)", row=2, col=1)

    fig.update_xaxes(title_text="", type="multicategory", row=1, col=1)
    fig.update_xaxes(title_text="", type="multicategory", row=2, col=1)

    fig.update_layout(
        title="Crashes by Functional Classification",
        barmode="group",
        bargap=0.30,
        bargroupgap=0.08,
        plot_bgcolor="white",
        paper_bgcolor="white",
        height=620,
        margin=dict(l=40, r=20, t=50, b=20),
        legend_title_text="Severity (KABCO)",
        uirevision="severity-by-functional-class",
    )

    fig.update_xaxes(showgrid=True, gridwidth=0.5, gridcolor="#eeeeee", showline=True, linecolor="#cccccc")
    fig.update_yaxes(showgrid=False, showline=True, linecolor="#cccccc")

    # dash table
    summary_table = dash_table.DataTable(
        data=table_df.to_dict('records'),
        columns=[{"name": "\xa0" + i, "id": i} for i in table_df.columns],
        style_table={'overflowX': 'auto'},
        style_cell={'textAlign': 'left'},
        style_header={'fontWeight': 'bold'},
        export_format="csv",
    )

    return summary_table, fig
