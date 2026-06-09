from dash import Input, Output, State, html, no_update, ctx, dcc, dash_table
import time
import dash
import re
import os
import sqlalchemy
import pandas as pd
import geopandas as gpd
import dash_leaflet as dl
import base64, io, zipfile, tempfile, json
from pathlib import Path
from flask import session
import dash_bootstrap_components as dbc
from shapely.geometry import shape
from ..utils import load as ld
from ..utils import preset_modals
from ..utils import dashboard_func
from ..utils import analysis as analysis_utils
from .. import utils
from ..layout import user_nav_children, study_nav_children
from ..pages.analysis_settings import analysis_settings_layout
from ..auth import login_user, logout_user, get_user_by_username, verify_password, create_user, current_user, AUTH_ENABLED
from ..global_vars import *
from ..report_processor.report_processor import render_report
from ..utils.crash_risk_estimation.config import Config
from ..utils.crash_risk_estimation.pipeline_runner import run_pipeline, ProductionPipeline
from ..utils.crash_risk_estimation.pipeline_steps.data_prep import get_study_data
from ..utils.crash_risk_estimation.datastore import DataStore

REPORT_SECTION_FLAGS = {
    "show_intro": True,
    "show_data_attributes": True,
    "show_descriptive_stats": True,
    "show_analysis_params": True,
    "show_sliding_windows": True,
    "show_models": True,
}
SSPF_PY_DIR = Path(__file__).resolve().parents[2]
STAGING_DIR = SSPF_PY_DIR / "staging"
REPORT_FILENAME_TEMPLATE = "sspf_report_{}_{}.pdf"
STUDY_DATA_FILENAME_TEMPLATE = "sspf_study_data_{}_{}.zip"
DEFAULT_REPORT_SCHEMA = "inputs"


def _build_report_yaml_params(study_name):
    params = dict(REPORT_SECTION_FLAGS)
    if study_name:
        params["study_name"] = study_name
    return params


def _build_report_py_params(study_id, study_name):
    port_value = os.getenv("DB_PORT", "5432")
    try:
        port = int(port_value)
    except ValueError:
        port = 5432

    return {
        "host": os.getenv("DB_HOST", "localhost"),
        "port": port,
        "user": os.getenv("DB_USER", "postgres"),
        "password": os.getenv("DB_PASSWORD", "password"),
        "db_name": os.getenv("DB_NAME", "sspf_db"),
        "schema": DEFAULT_REPORT_SCHEMA,
        "study_name": study_name,
        "study_id": study_id,
    }

def register_dashboard_callbacks(app):

    # Populate dashboard charts and items on page load
    @app.callback(
        Output("dashboard-busy", "is_open"),
        Output("dashboard-crashes-summary-df-store", "data"),
        Output("top-corridors-fc-store", "data"),
        # dropdown filter options
        Output("dashboard-crashes-by-mode-filter", "options"),
        Output("dashboard-crashes-by-severity-filter", "options"),
        Output("dashboard-crashes-by-fclass-filter", "options"),
        Output("dashboard-crashes-by-year-filter", "options"),
        Output("dashboard-obs-vs-est-filter", "options"),
        Output("dashboard-obs-vs-est-filter", "value"),
        # slider div contents
        Output("dashboard-crashes-by-mode-slider-div", "children"),
        Output("dashboard-crashes-by-severity-slider-div", "children"),
        Output("dashboard-crashes-by-fclass-slider-div", "children"),
        Output("dashboard-crashes-by-year-slider-div", "children"),
        Output("dashboard-crashes-by-mode-severity-slider-div", "children"),
        Output("dashboard-obs-vs-est-slider-div", "children"),
        # figures
        Output("dashboard-crashes-by-mode-figure", "children"),
        Output("dashboard-crashes-by-severity-figure", "children"),
        Output("dashboard-crashes-by-mode-severity-figure", "children"),
        Output("dashboard-crashes-by-fclass-figure", "children"),
        Output("dashboard-crashes-by-year-figure", "children"),
        Output("dashboard-obs-vs-est-figure", "children"),
        # summary tables
        Output("dashboard-crashes-by-mode-table", "children"),
        Output("dashboard-crashes-by-severity-table", "children"),
        Output("dashboard-crashes-by-mode-severity-table", "children"),
        Output("dashboard-crashes-by-fclass-table", "children"),
        Output("dashboard-crashes-by-year-table", "children"),
        Output("dashboard-obs-vs-est-table", "children"),
        Input("url", "pathname"),
        prevent_initial_call=False
    )
    def populate_dashboard_content(url_pathname):
        if url_pathname != "/dashboard":
            raise dash.exceptions.PreventUpdate
        
        study_id = session["active_study_id"]

        print("Populating dashboard content for study id ", study_id)

        df = dashboard_func.get_dashboard_data(study_id=study_id)
        # print(df)
        df_store = dashboard_func.df_to_store(df=df)
        filter_options = {}
        for col in ["crash_severity", "crash_mode"]:
            filter_options[col] = dashboard_func.get_filter_options(df=df,column=col)

        top_corridors_fc_store = dashboard_func.get_top_corridors_fc(study_id=study_id)

        (min_year, max_year) = dashboard_func.get_year_range(df=df)
        # slider div contents
        slider_divs = {}
        for id_prefix in [
            "dashboard-crashes-by-mode",
            "dashboard-crashes-by-severity",
            "dashboard-crashes-by-fclass",
            "dashboard-crashes-by-year",
            "dashboard-crashes-by-mode-severity",
            "dashboard-obs-vs-est",
        ]:
            slider_divs[id_prefix] = dcc.RangeSlider(
                id=f"{id_prefix}-slider",
                min=min_year,
                max=max_year,
                value=[min_year, max_year],
                marks={year: str(year) for year in range(min_year, max_year + 1)},
                step=1,
                tooltip={"placement": "top", "always_visible": True},
            )
        
        figs_dict = {}
        tables_dict = {}
        
        # donut charts for crash_mode and crash_severity
        for group_col in ["crash_mode", "crash_severity"]:
            summary_table, fig = dashboard_func.create_donut_fig_table(df=df, group_col=group_col, y0=None, y1=None, filter="All Crashes")
            figs_dict[group_col] = dcc.Graph(figure=fig)
            tables_dict[group_col] = summary_table

        # stacked bar chart for crashes by mode and severity
        stacked_table, fig_stacked = dashboard_func.mode_by_severity_stacked_components(df=df, y0=None, y1=None)
        figs_dict["crash_mode_sev_stacked"] = dcc.Graph(figure=fig_stacked)
        tables_dict["crash_mode_sev_stacked"] = stacked_table

        # stacked bar chart for crashes by functional class and severity
        funclass_stacked_table, funclass_stacked_fig = dashboard_func.funclass_stacked_components(df=df, y0=None, y1=None, filtered_mode="All Crashes")
        figs_dict["crash_fclass"] = dcc.Graph(figure=funclass_stacked_fig)
        tables_dict["crash_fclass"] = funclass_stacked_table
        # funclass_table, funclass_fig = dashboard_func.funclass_components(df_raw=df, y0=None, y1=None, filtered_mode="All Crashes")
        # figs_dict["crash_fclass"] = dcc.Graph(figure=funclass_fig)
        # tables_dict["crash_fclass"] = funclass_table

        # grouped bar chart for crashes by year and severity
        summary_table, fig = dashboard_func.severity_year_grouped_components(df=df, y0=None, y1=None, filtered_mode="All Crashes")
        figs_dict["crash_year_sev_grouped"] = dcc.Graph(figure=fig)
        tables_dict["crash_year_sev_grouped"] = summary_table

        # observed vs modeled crashes
        obs_vs_est_mode_options = [{"label": v, "value": k} for k, v in MODE_LABELS.items()]
        obs_vs_est_default_mode = MODE_ORDER[0]
        obs_est_df = analysis_utils.get_observed_vs_estimated_by_mode(study_id, obs_vs_est_default_mode)
        if obs_est_df is not None and not obs_est_df.empty:
            obs_est_fig = dashboard_func.observed_vs_estimated_bar(obs_est_df)
            figs_dict["obs_vs_est"] = dcc.Graph(figure=obs_est_fig)
            formatted = obs_est_df.copy()
            formatted["Observed Crashes"] = formatted["Observed Crashes"].map(lambda v: f"{v:,.0f}")
            formatted["Modeled Adjusted Crashes"] = formatted["Modeled Adjusted Crashes"].map(lambda v: f"{v:,.1f}")
            tables_dict["obs_vs_est"] = dash_table.DataTable(
                data=formatted.to_dict("records"),
                columns=[{"name": "\xa0" + i, "id": i} for i in formatted.columns],
                style_table={"overflowX": "auto"},
                style_cell={"textAlign": "left"},
                style_header={"fontWeight": "bold"},
                export_format="csv",
            )
        else:
            figs_dict["obs_vs_est"] = html.P(
                "Observed vs. modeled data is not available. Run the Safer Streets Model first.",
                style={"fontStyle": "italic"},
            )
            tables_dict["obs_vs_est"] = html.Div()

        return (
            False,
            df_store,
            top_corridors_fc_store,
            # filter dropdowns
            filter_options["crash_severity"],
            filter_options["crash_mode"],
            filter_options["crash_mode"],
            filter_options["crash_mode"],
            obs_vs_est_mode_options,
            obs_vs_est_default_mode,
            # slider divs
            slider_divs["dashboard-crashes-by-mode"],
            slider_divs["dashboard-crashes-by-severity"],
            slider_divs["dashboard-crashes-by-fclass"],
            slider_divs["dashboard-crashes-by-year"],
            slider_divs["dashboard-crashes-by-mode-severity"],
            slider_divs["dashboard-obs-vs-est"],
            # figures
            figs_dict["crash_mode"],
            figs_dict["crash_severity"],
            figs_dict["crash_mode_sev_stacked"],
            figs_dict["crash_fclass"],
            figs_dict["crash_year_sev_grouped"],
            figs_dict["obs_vs_est"],
            # summary tables
            tables_dict["crash_mode"],
            tables_dict["crash_severity"],
            tables_dict["crash_mode_sev_stacked"],
            tables_dict["crash_fclass"],
            tables_dict["crash_year_sev_grouped"],
            tables_dict["obs_vs_est"],
        )
    
    # change dashboard plots if filters change
    for id_prefix in [
        "dashboard-crashes-by-mode",
        "dashboard-crashes-by-severity"
    ]:
        @app.callback(
            Output(f"{id_prefix}-figure", "children", allow_duplicate=True),
            Output(f"{id_prefix}-table", "children", allow_duplicate=True),
            Input(f"{id_prefix}-filter", "value"),
            Input(f"{id_prefix}-slider", "value"),
            State("dashboard-crashes-summary-df-store", "data"),
            prevent_initial_call=True
        )
        def update_dashboard_plot(selected_filter, selected_years, df_store):
            if selected_filter is None or selected_years is None:
                raise dash.exceptions.PreventUpdate
            
            # Determine id_prefix from the triggered input
            triggered_id = ctx.triggered_id
            if "filter" in triggered_id:
                id_prefix = triggered_id.replace("-filter", "")
            elif "slider" in triggered_id:
                id_prefix = triggered_id.replace("-slider", "")
            else:
                raise dash.exceptions.PreventUpdate
            
            df = dashboard_func.store_to_df(data=df_store)

            if id_prefix == "dashboard-crashes-by-mode":
                group_col = "crash_mode"
            elif id_prefix == "dashboard-crashes-by-severity":
                group_col = "crash_severity"
            else:
                raise ValueError("Invalid id_prefix = " + id_prefix)
            
            summary_table, fig = dashboard_func.create_donut_fig_table(
                df=df,
                group_col=group_col,
                y0=selected_years[0],
                y1=selected_years[1],
                filter=selected_filter
            )
        
            return dcc.Graph(figure=fig), summary_table
    
    # Update mode-severity stacked chart (only has slider, no filter)
    @app.callback(
        Output("dashboard-crashes-by-mode-severity-figure", "children", allow_duplicate=True),
        Output("dashboard-crashes-by-mode-severity-table", "children", allow_duplicate=True),
        Input("dashboard-crashes-by-mode-severity-slider", "value"),
        State("dashboard-crashes-summary-df-store", "data"),
        prevent_initial_call=True
    )
    def update_mode_severity_stacked(selected_years, df_store):
        if selected_years is None:
            raise dash.exceptions.PreventUpdate
        
        df = dashboard_func.store_to_df(data=df_store)
        
        summary_table, fig = dashboard_func.mode_by_severity_stacked_components(
            df=df,
            y0=selected_years[0],
            y1=selected_years[1]
        )
        
        return dcc.Graph(figure=fig), summary_table
    
    # update functional class stacked chart
    @app.callback(
        Output("dashboard-crashes-by-fclass-figure", "children", allow_duplicate=True),
        Output("dashboard-crashes-by-fclass-table", "children", allow_duplicate=True),
        Input("dashboard-crashes-by-fclass-filter", "value"),
        Input("dashboard-crashes-by-fclass-slider", "value"),
        State("dashboard-crashes-summary-df-store", "data"),
        prevent_initial_call=True
    )
    def update_funclass_stacked(selected_filter, selected_years, df_store):
        if selected_filter is None or selected_years is None:
            raise dash.exceptions.PreventUpdate
        
        df = dashboard_func.store_to_df(data=df_store)

        summary_table, fig = dashboard_func.funclass_stacked_components(
            df=df,
            y0=selected_years[0],
            y1=selected_years[1],
            filtered_mode=selected_filter
        )
    
        return dcc.Graph(figure=fig), summary_table
    
    # update year-severity grouped chart
    @app.callback(
        Output("dashboard-crashes-by-year-figure", "children", allow_duplicate=True),
        Output("dashboard-crashes-by-year-table", "children", allow_duplicate=True),
        Input("dashboard-crashes-by-year-filter", "value"),
        Input("dashboard-crashes-by-year-slider", "value"),
        State("dashboard-crashes-summary-df-store", "data"),
        prevent_initial_call=True
    )
    def update_year_severity_grouped(selected_filter, selected_years, df_store):
        if selected_filter is None or selected_years is None:
            raise dash.exceptions.PreventUpdate
        
        df = dashboard_func.store_to_df(data=df_store)

        summary_table, fig = dashboard_func.severity_year_grouped_components(
            df=df,
            y0=selected_years[0],
            y1=selected_years[1],
            filtered_mode=selected_filter
        )
    
        return dcc.Graph(figure=fig), summary_table
    
    # update observed vs modeled chart on mode change
    @app.callback(
        Output("dashboard-obs-vs-est-figure", "children", allow_duplicate=True),
        Output("dashboard-obs-vs-est-table", "children", allow_duplicate=True),
        Input("dashboard-obs-vs-est-filter", "value"),
        prevent_initial_call=True
    )
    def update_obs_vs_est(selected_mode):
        if selected_mode is None:
            raise dash.exceptions.PreventUpdate

        study_id = session["active_study_id"]
        df = analysis_utils.get_observed_vs_estimated_by_mode(study_id, selected_mode)

        if df is None or df.empty:
            return html.P("No data available for this mode."), html.Div()

        fig = dashboard_func.observed_vs_estimated_bar(df)
        formatted = df.copy()
        formatted["Observed Crashes"] = formatted["Observed Crashes"].map(lambda v: f"{v:,.0f}")
        formatted["Modeled Adjusted Crashes"] = formatted["Modeled Adjusted Crashes"].map(lambda v: f"{v:,.1f}")
        table = dash_table.DataTable(
            data=formatted.to_dict("records"),
            columns=[{"name": "\xa0" + i, "id": i} for i in formatted.columns],
            style_table={"overflowX": "auto"},
            style_cell={"textAlign": "left"},
            style_header={"fontWeight": "bold"},
            export_format="csv",
        )

        return dcc.Graph(figure=fig), table

    # update top corridors map and table
    @app.callback(
        Output("missing-sliding-windows-scores-modal-div", "children"),
        Output("top-corridors-layer", "children"),
        Output("top-corridors-summary-table-div", "children"),
        Input("top-corridors-mode-filter", "value"),
        State("top-corridors-fc-store", "data"),
        prevent_initial_call=True
    )
    def update_top_corridors_map_table(selected_mode, fc_store):
        if selected_mode is None:
            raise dash.exceptions.PreventUpdate
        
        # if store is empty dictionary, show modal warning about missing sliding windows scores
        if not fc_store:
            modal = preset_modals.info_modal(
                title="Sliding Windows Scores Missing",
                message="Top corridors data is not available because sliding windows scores have not been computed for this study. \
                    Please run the Sliding Windows Analysis to generate the necessary data."
            )
            return modal, no_update, no_update

        mode_fc = fc_store[selected_mode]

        map_layer = dl.GeoJSON(
            data=mode_fc,
            options=dict(
                style=TOP_CORRIDOR_CONFIG.layers["top_corridors"].feature_style.to_dict(),
                onEachFeature={"variable": "sspf_onEachFeaturePopup"}
            ),
            hoverStyle=TOP_CORRIDOR_CONFIG.layers["top_corridors"].hover_style.to_dict(),
            zoomToBounds=True
        )

        # table of top corridors is basically just a list of road names and functional classes
        features = mode_fc.get("features", [])
        # read as dataframe the properties of each feature
        rows = []
        for feature in features:
            prop = feature.get("properties", {})
            rows.append({
                "Road Name": prop.get("Road Name", ""),
                "Functional Class": prop.get("Functional Class", ""),
                "Crash Score": prop.get("Crash Score", 0)
            })
        table_df = pd.DataFrame(rows)
        summary_table = dash_table.DataTable(
            data=table_df.to_dict("records"),
            columns=[{"name": i, "id": i} for i in table_df.columns],
            style_table={"overflowX": "auto"},
            style_cell={"textAlign": "left", "padding": "5px"},
            style_header={"backgroundColor": "lightgrey", "fontWeight": "bold"},
            export_format="csv",
        )
        
        return no_update, map_layer, summary_table

    # Toggle collapses for each dashboard plot card
    for id_prefix in [
        "dashboard-crashes-by-mode",
        "dashboard-crashes-by-severity",
        "dashboard-crashes-by-fclass",
        "dashboard-crashes-by-year",
        "dashboard-crashes-by-mode-severity",
        "dashboard-obs-vs-est",
        "top-corridors",
    ]:
        @app.callback(
            Output(f"{id_prefix}-collapse", "is_open"),
            Output(f"{id_prefix}-toggle", "children"),
            Input(f"{id_prefix}-toggle", "n_clicks"),
            State(f"{id_prefix}-collapse", "is_open"),
            prevent_initial_call=True
        )
        def toggle_collapse(n, is_open):
            if n:
                return not is_open, "+" if is_open else "-"
            return is_open, "-"
        
    # PDF report compile and download button callbacks
    @app.callback(
        Output("dashboard-compile-report-busy", "is_open"),
        Output("dashboard-pdf-report-output-div", "children"),
        Input("dashboard-compile-report-btn", "n_clicks"),
        prevent_initial_call=True
    )
    def compile_report(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        
        time.sleep(1) # to allow modal to show up
        study_id = session.get("active_study_id")
        un, sn = utils.db.get_user_and_study_names(study_id)

        yaml_params = _build_report_yaml_params(sn)
        py_params = _build_report_py_params(study_id=study_id, study_name=sn)
        output_name = REPORT_FILENAME_TEMPLATE.format(un, sn)
        quarto_bin = os.getenv("QUARTO_BIN", "quarto")
        STAGING_DIR.mkdir(parents=True, exist_ok=True)

        try:
            output_path = render_report(
                output_dir=STAGING_DIR,
                output_name=output_name,
                yaml_params=yaml_params,
                py_params=py_params,
                quarto_bin=quarto_bin,
            )
        except Exception as exc:
            print(f"Report compilation failed for study {study_id}: {exc}")
            return (
                False,
                utils.preset_modals.info_modal(
                    title="Report Compilation Failed",
                    message=f"{exc}"
                ),
            )

        pretty_name = sn or f"Study {study_id}"
        message_lines = [
            f"The PDF report for {pretty_name} has been generated.",
            "Use the Download PDF Report button to retrieve the file."
        ]

        return (
            False,
            utils.preset_modals.info_modal(
                title="Report Compiled",
                message="\n".join(message_lines)
            )
        )
    
    @app.callback(
        Output("dashboard-pdf-report-output-div", "children", allow_duplicate=True),
        Output("dashboard-pdf-report-url-store", "data"),
        Input("dashboard-download-report-btn", "n_clicks"),
        prevent_initial_call=True
    )

    def download_report(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate

        study_id = session.get("active_study_id")
        un, sn = utils.db.get_user_and_study_names(study_id=study_id)

        if study_id is None:
            return (
                utils.preset_modals.info_modal(
                    title="No Study Selected",
                    message="Select a study before downloading a PDF report."
                ),
                no_update,
            )

        output_path = (STAGING_DIR / REPORT_FILENAME_TEMPLATE.format(un, sn)).resolve()
        if not output_path.exists():
            return (
                utils.preset_modals.info_modal(
                    title="Report Not Found",
                    message="No compiled PDF report was found for this study. Click 'Compile PDF Report' first, then try downloading again."
                ),
                no_update,
            )

        pretty_name = sn or f"Study {study_id}"
        return (
            [],
            f"/download/{output_path.name}"
        )

    # study data compile and download button callbacks
    @app.callback(
        Output("dashboard-compile-study-data-busy", "is_open"),
        Output("dashboard-study-data-output-div", "children"),
        Input("dashboard-compile-study-data-btn", "n_clicks"),
        prevent_initial_call=True
    )
    def compile_study_data(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        
        time.sleep(1) # to allow modal to show up
        
        study_id = session["active_study_id"]
        STAGING_DIR.mkdir(parents=True, exist_ok=True)

        try:
            utils.package_study_data.compile_study_data(study_id=study_id, 
                                                        output_folder=STAGING_DIR, 
                                                        output_zip_name=STUDY_DATA_FILENAME_TEMPLATE.format(*utils.db.get_user_and_study_names(study_id)))
        except Exception as e:
            return (
                False,
                utils.preset_modals.info_modal(
                    title="Error Compiling Study Data",
                    message=str(e)
                )
            )

        return (
            False,
            utils.preset_modals.info_modal(
                title="Study Data Compiled",
                message="Use the Download Study Data button to retrieve the file."
            )
        )
    
    @app.callback(
        Output("dashboard-study-data-output-div", "children", allow_duplicate=True),
        Output("dashboard-study-data-url-store", "data"),
        Input("dashboard-download-study-data-btn", "n_clicks"),
        prevent_initial_call=True
    )
    def download_study_data(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate

        study_id = session["active_study_id"]
        un, sn = utils.db.get_user_and_study_names(study_id)

        output_path = (STAGING_DIR / STUDY_DATA_FILENAME_TEMPLATE.format(un, sn)).resolve()
        if not output_path.exists():
            return (
                utils.preset_modals.info_modal(
                    title="Study Data Not Found",
                    message="No compiled study data was found for this study. Click 'Compile Study Data' first, then try downloading again."
                ),
                no_update,
            )

        pretty_name = sn or f"Study {study_id}"
        return (
            [],
            f"/download/{output_path.name}"
        )

    app.clientside_callback(
        "function(url) { if (url) { window.location.href = url; } return ''; }",
        Output("dashboard-pdf-dl-dummy", "children"),
        Input("dashboard-pdf-report-url-store", "data"),
    )

    app.clientside_callback(
        "function(url) { if (url) { window.location.href = url; } return ''; }",
        Output("dashboard-study-data-dl-dummy", "children"),
        Input("dashboard-study-data-url-store", "data"),
    )
