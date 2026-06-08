from dash import Input, Output, State, ClientsideFunction, html, no_update, ctx, dcc
import time
import dash
import re
import os
import sqlalchemy
import pandas as pd
import geopandas as gpd
import dash_leaflet as dl
import base64, io, zipfile, tempfile, json
from flask import session
import dash_bootstrap_components as dbc
from shapely.geometry import shape
from ..utils import load as ld
from ..utils import preset_modals
from .. import utils
from ..layout import user_nav_children, study_nav_children
from ..pages.analysis_settings import analysis_settings_layout, _analysis_settings_children
from ..auth import login_user, logout_user, get_user_by_username, verify_password, create_user, current_user, AUTH_ENABLED
from ..global_vars import *
from ..utils.crash_risk_estimation.config import Config
from ..utils.crash_risk_estimation.pipeline_runner import run_pipeline, ProductionPipeline
from ..utils.crash_risk_estimation.pipeline_steps.data_prep import get_study_data
from ..utils.crash_risk_estimation.datastore import DataStore

def register_analysis_callbacks(app):
    # If reset to defaults is clicked, reset all inputs to default values
    @app.callback(
        Output("analysis-settings-container", "children"),
        Input("ap-reset", "n_clicks"),
        prevent_initial_call=True,
    )
    def update_analysis_settings_container(n_clicks):
        if n_clicks:
            return _analysis_settings_children(study_id=session["active_study_id"], use_defaults=True)
        else:
            return _analysis_settings_children(study_id=session["active_study_id"], use_defaults=False)

    # everytime an input changes, store the new parameters in dcc.Store
    @app.callback(
        Output("analysis-params", "data"),
        [
            Input(f"ap-weight-{s}", "value") for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"
        ] +
        [
            Input(f"ap-cost-{s}", "value") for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"
        ] +
        [
            Input("ap-sliding-window-length", "value"),
            Input("ap-window-length-ratio", "value"),
            Input("ap-crash-join-distance", "value"),
            Input("ap-num-streets", "value"),
        ],
        prevent_initial_call=False,
    )
    def store_analysis_params(*args):
        param_keys = []
        for s in CRASH_SEVERITIES.keys():
            if s != "Omit From Analysis":
                param_keys.append(f"crash_weights.{s}")
        for s in CRASH_SEVERITIES.keys():
            if s != "Omit From Analysis":
                param_keys.append(f"crash_costs.{s}")
        param_keys += [
            "sliding_window_length",
            "window_length_ratio",
            "crash_join_distance",
            "top_streets_threshold",
        ]
        params = {k: v for k, v in zip(param_keys, args)}
        return params

    # If apply & save is clicked, save the new parameters to the database
    @app.callback(
        Output("analysis-settings-container", "children", allow_duplicate=True),
        Output("ap-status", "children"),
        Input("ap-apply", "n_clicks"),
        State("analysis-params", "data"),
        prevent_initial_call=True,
    )
    def apply_and_save_analysis_params(n_clicks, children):
        if n_clicks > 0:
            study_id = session["active_study_id"]
            crash_weights_values = {s: children.get(f"crash_weights.{s}", None) for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"}
            crash_costs_values = {s: children.get(f"crash_costs.{s}", None) for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"}
            analysis_params = {
                "sliding_window_length": children.get("sliding_window_length", None),
                "window_length_ratio": children.get("window_length_ratio", None),
                "crash_join_distance": children.get("crash_join_distance", None),
                "top_streets_threshold": children.get("top_streets_threshold", None),
                "crash_weights": crash_weights_values,
                "crash_costs": crash_costs_values,
            }

            # do some validation checks before saving
            # crash weights
            # should all be integers between 0 and 100
            # at least one weight should be > 0
            # should maintain ordinality (K >= A >= B >= C >= O)
            weights_list = [analysis_params["crash_weights"][s] for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"]
            if any([w is None or not isinstance(w, int) or w < 0 or w > 100 for w in weights_list]):
                return no_update, preset_modals.simple_modal(message="Error: Crash Weights must be integers between 0 and 100.", color="red")
            if all([w == 0 for w in weights_list]):
                return no_update, preset_modals.simple_modal(message="Error: At least one Crash Weight must be greater than 0.", color="red")
            if not all([weights_list[i] >= weights_list[i+1] for i in range(len(weights_list)-1)]):
                return no_update, preset_modals.simple_modal(message="Error: Crash Weights must maintain ordinality (K ≥ A ≥ B ≥ C ≥ O).", color="red")

            # sliding window length - should be between 0.25 and 2 miles increments of 0.05
            swl = analysis_params["sliding_window_length"]
            if swl is None or not (0.25 <= swl <= 2) or (swl * 100) % 5 != 0:
                return no_update, preset_modals.simple_modal(message="Error: Sliding Window Length must be between 0.25 and 2 miles in increments of 0.05.", color="red")
            # window length ratio - should be between 1 and 10 integer
            wlr = analysis_params["window_length_ratio"]
            if wlr is None or not (1 <= wlr <= 10) or not isinstance(wlr, int):
                return no_update, preset_modals.simple_modal(message="Error: Sliding Window to Short Window Length Ratio must be an integer between 1 and 10.", color="red")

            # crash join distance - should be between 25 to 250 feet integer
            cjd = analysis_params["crash_join_distance"]
            if cjd is None or not (25 <= cjd <= 250) or not isinstance(cjd, int):
                return no_update, preset_modals.simple_modal(message="Error: Crash Join Distance must be an integer between 25 and 250 feet.", color="red")

            # top streets threshold - should be 5-20 integer
            tst = analysis_params["top_streets_threshold"]
            if tst is None or not (5 <= tst <= 20) or not isinstance(tst, int):
                return no_update, preset_modals.simple_modal(message="Error: # of Streets in Report must be an integer between 5 and 20.", color="red")

            # crash costs must maintain ordinality (K >= A >= B >= C >= O)
            costs_list = [analysis_params["crash_costs"][s] for s in CRASH_SEVERITIES.keys() if s != "Omit From Analysis"]
            if not all([costs_list[i] >= costs_list[i+1] for i in range(len(costs_list)-1)]):
                return no_update, preset_modals.simple_modal(message="Error: Crash Costs must maintain ordinality (K ≥ A ≥ B ≥ C ≥ O).", color="red")

            q = f"""
            UPDATE gen_management.studies
            SET
                sliding_window_length = {analysis_params['sliding_window_length']},
                window_length_ratio = {analysis_params['window_length_ratio']},
                short_window_length = {analysis_params['sliding_window_length']}::FLOAT / {analysis_params['window_length_ratio']}::FLOAT,
                crash_join_distance = {analysis_params['crash_join_distance']},
                top_streets_threshold = {analysis_params['top_streets_threshold']},
                crash_weights = '{json.dumps(analysis_params['crash_weights'])}',
                crash_costs = '{json.dumps(analysis_params['crash_costs'])}',
                analysis_params_updated = NOW()
            WHERE study_id = {study_id}
            """
            eng = utils.db.get_database_engine()
            with eng.connect() as conn:
                conn.execute(sqlalchemy.text(q))
                conn.commit()

            return _analysis_settings_children(study_id=study_id, use_defaults=False), dbc.Alert("Analysis parameters saved successfully.", color="success", is_open=True)
        else:
            return no_update, None

    # Switch between analysis sections based on navbar clicks
    @app.callback(
        Output("sliding-windows-section", "style"),
        Output("safer-streets-model-section", "style"),
        Output("nav-sliding-windows", "active"),
        Output("nav-safer-streets-model", "active"),
        Output("active-analysis-tab", "data"),
        Output("safer-streets-model-map-container", "children"),
        Output("safer-streets-model-map-container", "style"),
        Output("dummy-model-map-container", "children"),
        Input("nav-sliding-windows", "n_clicks"),
        Input("nav-safer-streets-model", "n_clicks"),
        prevent_initial_call=True,
    )
    def switch_analysis_sections(n_sw, n_ssm):
        triggered_id = ctx.triggered_id
        
        if triggered_id == "nav-sliding-windows":
            return (
                {"display": "block"}, {"display": "none"}, 
                True, False, "sliding-windows", 
                no_update,
                no_update,
                no_update
            )
                
        elif triggered_id == "nav-safer-streets-model":
            if n_ssm and n_ssm == 1:
                print("First time loading Safer Streets Model map, generating placeholder...")
                # re-render the map to force redraw as it wasn't being rendered well when hidden
                return (
                    {"display": "none"}, {"display": "block"}, 
                    False, True, "safer-streets-model", 
                    utils.analysis.placeholder_analysis_results_map(analysis_type="safer-streets-model"),
                    {"display": "block"},
                    []  # clear dummy content
                )
            else:
                return (
                    {"display": "none"}, {"display": "block"}, 
                    False, True, "safer-streets-model", 
                    no_update,
                    no_update,
                    no_update
                )
        else:
            return (
                no_update, no_update, 
                no_update, no_update, no_update, 
                no_update,
                no_update,
                no_update
            ) 
    
    # Run Sliding Window Analysis
    @app.callback(
        Output("sliding-windows-results-message", "children"),
        Output("run-sliding-windows-busy", "is_open"),
        Output("map-sliding-windows-btn", "disabled"),
        Input("run-sliding-windows-btn", "n_clicks"),
        prevent_initial_call=True,
    )
    def run_sliding_window_analysis(n_clicks):
        if n_clicks and n_clicks > 0:
            time.sleep(1)  # hold until modal kicks in
            study_id = session["active_study_id"]
            try:
                # Step 1: Run sliding windows analysis
                swa_success, output_msg = utils.analysis.run_sliding_windows_analysis(study_id=study_id)

                # update the results message with the results info
                if swa_success:
                    # update the studies table with the current timestamp for when the analysis was run
                    q = f"""
                        UPDATE gen_management.studies
                        SET
                            sliding_windows_analysis_status = 'Completed',
                            sliding_windows_analysis_updated = NOW()
                        WHERE study_id = {study_id};
                    """
                    eng = utils.db.get_database_engine()
                    with eng.connect() as conn:
                        conn.execute(sqlalchemy.text(q))
                        conn.commit()

                    return html.Div([
                        dbc.Alert(["Sliding Windows Analysis has been run successfully. Click the visualize button to see the results on the map."])
                    ]), False, False
                else:
                    q = f"""
                        UPDATE gen_management.studies
                        SET
                            sliding_windows_analysis_status = 'Process Errored out at ' || NOW()::TEXT
                        WHERE study_id = {study_id};
                    """
                    eng = utils.db.get_database_engine()
                    with eng.connect() as conn:
                        conn.execute(sqlalchemy.text(q))
                        conn.commit()

                    return html.Div([
                        dbc.Alert([f"Error running Sliding Windows Analysis: {output_msg}"], color="danger")
                    ]), False, no_update
            except Exception as exc:
                print(f"Sliding windows analysis failed for study {study_id}: {exc}")
                return html.Div([
                    dbc.Alert([f"Error running Sliding Windows Analysis: {exc}"], color="danger")
                ]), False, no_update
        else:
            return no_update, False, no_update

    @app.callback(
        Output("safer-streets-model-results-message", "children"),
        Output("run-safer-streets-model-busy", "is_open"),
        Output("map-safer-streets-model-btn", "disabled"),
        Input("run-safer-streets-model-btn", "n_clicks"),
        prevent_initial_call=True,
    )

    # Run Safer Streets Model Analysis
    def run_safer_streets_model_analysis(n_clicks):
        if n_clicks and n_clicks > 0:
            time.sleep(1)  # hold until modal kicks in
            study_id = session["active_study_id"]
            eng = utils.db.get_database_engine()

            try:
                # Step 1: Gather analysis params from database
                start_time = time.time()
                print("Retrieving analysis params...")
                analysis_params = utils.analysis.get_analysis_params(study_id=study_id)["current"]
                # Step 2: Create short windows (unit of analysis)
                print("Creating short windows...")
                (short_windows_table, model_0p05mi_segments_table) = utils.analysis.create_short_windows(study_id=study_id, create_model_0p05mi_segments=True)
                # Step 3: Do model processing and generate results
                # Step 3.1: prepare pipeline config
                print("Preparing pipeline config...")
                pipeline_config = utils.analysis.create_ssma_config(study_id, analysis_params, short_windows_table, segments_0p05mi_table=model_0p05mi_segments_table)
                # Step 3.2: create pipeline datastore
                print("Creating pipeline datastore...")
                pipeline_datastore = DataStore(config=pipeline_config)
                # Step 3.3: run pipeline
                print("Running crash risk estimation pipeline...")
                pipeline_datastore = run_pipeline(ProductionPipeline(), pipeline_datastore)

                # add crash costs to the final results table
                print("Adding crash costs to results...")
                utils.analysis.add_crash_costs(study_id=study_id)

                end_time = time.time()
                print(f"Total execution time: {end_time - start_time:.2f} seconds")

                # update studies table with completion status and timestamp
                q = f"""
                    UPDATE gen_management.studies
                    SET
                        modeling_status = 'Completed',
                        modeling_updated = NOW()
                    WHERE study_id = {study_id};
                """
                with eng.connect() as conn:
                    conn.execute(sqlalchemy.text(q))
                    conn.commit()

                # update the results message
                return html.Div([
                    dbc.Alert(["Safer Streets Model Analysis has been run successfully. Click the visualize button to see the results on the map."])
                ]), False, False
            except Exception as exc:
                print(f"Safer Streets Model analysis failed for study {study_id}: {exc}")

                q = f"""
                    UPDATE gen_management.studies
                    SET
                        modeling_status = 'Process Errored out at ' || NOW()::TEXT
                    WHERE study_id = {study_id};
                """
                with eng.connect() as conn:
                    conn.execute(sqlalchemy.text(q))
                    conn.commit()

                return html.Div([
                    dbc.Alert([f"Error running Safer Streets Model Analysis: {exc}"], color="danger")
                ]), False, no_update
        else:
            return no_update, False, no_update

    # set visualization buttons to disabled on page load only if no existing results
    @app.callback(
        Output("map-sliding-windows-btn", "disabled", allow_duplicate=True),
        Output("map-safer-streets-model-btn", "disabled", allow_duplicate=True),
        Input("page-load-trigger", "data"),
        prevent_initial_call='initial_duplicate',
    )

    def set_visualization_buttons_disabled_on_load(trigger):
        study_id = session["active_study_id"]
        un, sn = utils.db.get_user_and_study_names(study_id=study_id)        

        output_swa_short_windows = utils.db.get_study_table_names(un, sn)["output_swa_short_windows"]
        ssm_results_table = utils.db.get_study_table_names(un, sn)["output_model_results"]

        short_windows_exists = utils.db.table_exists(table_name=output_swa_short_windows)
        ssm_results_exists = utils.db.table_exists(table_name=ssm_results_table)
            
        return not short_windows_exists, not ssm_results_exists
    
    # Load analysis results onto map when visualize button is clicked
    # Load results: store single FC + mode config (geometry sent once)
    for analysis_type in ["sliding-windows", "safer-streets-model"]:
        @app.callback(
            Output(f"study-area-{analysis_type}-layer", "children"),
            Output(f"{analysis_type}-geojson-store", "data"),
            Output(f"{analysis_type}-mode-style-config", "data"),
            Output(f"{analysis_type}-legend-data", "data"),
            Output(f"{analysis_type}-mode-selector-div", "style"),
            Output(f"{analysis_type}-mode-selector", "options"),
            Output(f"{analysis_type}-mode-selector", "value"),
            Output(f"get-{analysis_type}-results-busy", "is_open"),
            Output(f"{analysis_type}-map-results-message", "children"),
            Input(f"map-{analysis_type}-btn", "n_clicks"),
            prevent_initial_call=True
        )

        def load_analysis_map(n_clicks):
            if n_clicks and n_clicks > 0:
                triggered_id = ctx.triggered_id
                print(f"Loading analysis results for {triggered_id}...")
                if "sliding-windows" in triggered_id:
                    analysis_type = "sliding-windows"
                elif "safer-streets-model" in triggered_id:
                    analysis_type = "safer-streets-model"
                else:
                    raise dash.exceptions.PreventUpdate

                time.sleep(1)  # hold until modal kicks in
                study_id = session["active_study_id"]
                try:
                    study_layer, geojson_data, mode_style_config, legend_data = utils.analysis.analysis_results_layers(study_id=study_id, analysis_type=analysis_type)

                    mode_options = [
                        {"label": MODE_LABELS[m], "value": m} for m in mode_style_config.keys()
                    ]
                    first_mode_value = mode_options[0]["value"] if mode_options else None
                    selector_style = {"marginBottom": "12px", "display": "flex", "alignItems": "center", "gap": "12px"}
                    return (
                        study_layer,
                        geojson_data,
                        mode_style_config,
                        legend_data,
                        selector_style,
                        mode_options,
                        first_mode_value,
                        False,
                        None,
                    )
                except Exception as exc:
                    print(f"Failed to load analysis map for study {study_id} ({analysis_type}): {exc}")
                    return (no_update, no_update, no_update, no_update, no_update, no_update, no_update, False,
                        dbc.Alert(f"Error loading results: {exc}", color="danger"),
                    )
            else:
                return (no_update, no_update, no_update, no_update, no_update, no_update, no_update, False, no_update)

    # Clientside mode switch: build per-bin layers from shared FC
    for analysis_type in ["sliding-windows", "safer-streets-model"]:
        app.clientside_callback(
            ClientsideFunction(namespace="sspf", function_name="buildModeLayers"),
            Output(f"{analysis_type}-results-layer", "children"),
            Output(f"{analysis_type}-legend", "children"),
            Output(f"{analysis_type}-legend", "style"),
            Input(f"{analysis_type}-mode-selector", "value"),
            State(f"{analysis_type}-geojson-store", "data"),
            State(f"{analysis_type}-mode-style-config", "data"),
            State(f"{analysis_type}-legend-data", "data"),
            prevent_initial_call=True,
        )

