from dash import Input, Output, State, html, no_update, ctx, dcc
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
from ..pages.analysis_settings import analysis_settings_layout
from ..auth import login_user, logout_user, get_user_by_username, verify_password, create_user, current_user, AUTH_ENABLED
from ..global_vars import *
from ..utils.crash_risk_estimation.config import Config
from ..utils.crash_risk_estimation.pipeline_runner import run_pipeline, ProductionPipeline
from ..utils.crash_risk_estimation.pipeline_steps.data_prep import get_study_data
from ..utils.crash_risk_estimation.datastore import DataStore

def register_busy_modals_callbacks(app):

    # Open "Getting results..." modals on visualize clicks
    for analysis_type in ["sliding-windows", "safer-streets-model"]:
        @app.callback(
            Output(f"get-{analysis_type}-results-busy", "is_open", allow_duplicate=True),
            Input(f"map-{analysis_type}-btn", "n_clicks"),
            prevent_initial_call=True,
        )
        def open_getting_results_busy(n_clicks):
            if not n_clicks:
                raise dash.exceptions.PreventUpdate
            return True
        
    # Open busy modal when running sliding windows or safer streets model analysis
    for analysis_type in ["sliding-windows", "safer-streets-model"]:
        @app.callback(
            Output(f"run-{analysis_type}-busy", "is_open", allow_duplicate=True),
            Input(f"run-{analysis_type}-btn", "n_clicks"),
            prevent_initial_call=True,
        )
        def open_analysis_run_busy_modal(n_clicks):
            if not n_clicks:
                raise dash.exceptions.PreventUpdate
            return True
    
    # dashboard compile report busy modal
    @app.callback(
        Output("dashboard-compile-report-busy", "is_open", allow_duplicate=True),
        Input("dashboard-compile-report-btn", "n_clicks"),
        prevent_initial_call=True,
    )
    def open_dashboard_compile_report_busy(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        return True
    
    # dashboard compile study data busy modal
    @app.callback(
        Output("dashboard-compile-study-data-busy", "is_open", allow_duplicate=True),
        Input("dashboard-compile-study-data-btn", "n_clicks"),
        prevent_initial_call=True,
    )
    def open_dashboard_compile_study_data_busy(n_clicks):
        if not n_clicks:
            raise dash.exceptions.PreventUpdate
        return True