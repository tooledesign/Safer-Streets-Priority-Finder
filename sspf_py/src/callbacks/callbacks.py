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
from ..auth import login_user, logout_user, get_user_by_username, verify_password, create_user, current_user, update_password, hash_password, AUTH_ENABLED
from ..global_vars import *

# Import page layouts at module level
from ..pages.welcome import layout as welcome_layout
from ..pages.overview import layout as overview_layout
from ..pages.instructions import layout as instructions_layout
from ..pages.load_data import layout as load_data_layout
from ..pages.login import layout as login_layout
from ..pages.register import layout as register_layout
from ..pages.change_password import layout as change_password_layout
from ..pages.studies import layout as studies_layout
from ..pages.analysis_settings import analysis_settings_layout
from ..pages.run_analysis import layout as run_analysis_layout
from ..pages.dashboard import layout as dashboard_layout
from ..pages.map_visualization import layout as map_visualization_layout
from ..pages.faq import layout as faq_layout
from ..pages.crash_data_sources import layout as crash_data_sources_layout
from ..pages.resources import layout as resources_layout
from ..pages.not_found import layout as not_found_layout

# Map routes to layouts
PAGES = {
    "/": welcome_layout,
    "/welcome": welcome_layout,
    "/overview": overview_layout,
    "/instructions": instructions_layout,
    "/load_data": load_data_layout,
    "/login": login_layout,
    "/register": register_layout,
    "/studies": studies_layout,
    "/analysis_settings": lambda: analysis_settings_layout(study_id=session.get("active_study_id")),
    "/run_analysis": run_analysis_layout,
    "/dashboard": dashboard_layout,
    "/map_visualization": map_visualization_layout,
    "/change_password": change_password_layout,
    "/faq": faq_layout,
    "/crash_data_sources": crash_data_sources_layout,
    "/resources": resources_layout,
}

# Pages that require auth when AUTH_ENABLED = true
PROTECTED_PATHS = {"/studies",
                   "/load_data",
                   "/analysis_settings",
                   "/run_analysis",
                   "/dashboard",
                   "/map_visualization",
                   "/change_password"}  # gated pages
# study specific pages is everything in PROTECTED_PATHS except studies selection and change_password (which should be accessible even without an active study, but still require login)
STUDY_SPECIFIC_PATHS = PROTECTED_PATHS - {"/studies", "/change_password"}

def register_callbacks(app: dash.Dash):
    @app.callback(
        Output("page-content", "children"),
        Output("user-nav", "children"),
        Output("study-nav", "children"),
        Input("url", "pathname"),
        prevent_initial_call=False,
    )
    
    def route(pathname):
        print(f"[ROUTE] pathname={pathname!r} active_study_id={session.get('active_study_id')!r} post_study_redirect={session.get('post_study_redirect')!r}")
        try:
            if pathname == "/logout":
                logout_user()
                session.clear()
                return dcc.Location(href="/login", id="after-logout"), user_nav_children(), study_nav_children("None")
            
            # Database connection check for protected paths (before auth check) and login gate
            if pathname in PROTECTED_PATHS.union({"/login", "/register"}):
                try:
                    if not utils.gen.check_db_connection():
                        db_error_modal = preset_modals.db_connection_error_modal()
                        try:
                            return db_error_modal, user_nav_children(), study_nav_children("None")
                        except:
                            return db_error_modal, dbc.Nav(), dbc.Nav()
                except Exception as e:
                    # If check itself fails, show error modal
                    print(f"DB connection check failed: {e}")
                    import traceback
                    traceback.print_exc()
                    db_error_modal = preset_modals.db_connection_error_modal()
                    try:
                        return db_error_modal, user_nav_children(), study_nav_children("None")
                    except:
                        return db_error_modal, dbc.Nav(), dbc.Nav()
            
            # Login gate - safely check current user
            try:
                is_authenticated = AUTH_ENABLED and pathname in PROTECTED_PATHS and not current_user()
            except:
                is_authenticated = AUTH_ENABLED and pathname in PROTECTED_PATHS
            
            if is_authenticated:
                page = login_layout
            else:
                if pathname in STUDY_SPECIFIC_PATHS:
                    if not session.get("active_study_id"):
                        print(f"[ROUTE] STUDY GATE FIRED for {pathname!r} — saving post_study_redirect and redirecting to /studies")
                        session["post_study_redirect"] = pathname
                        session.modified = True
                        return dcc.Location(href="/studies", id="study-gate-redirect", refresh=True), \
                               user_nav_children(), study_nav_children("None")

                page = PAGES.get(pathname)
                if page is None:
                    page = lambda: not_found_layout(pathname)

            # Safely get current user (may fail if DB is down)
            try:
                cu = current_user()
            except:
                cu = None
            
            if cu:
                cu_id = cu.user_id
                if session.get("active_study_id"):
                    study_name = session["study_name"]
                else:
                    study_name = "None"
            else:
                study_name = "None"
        
            return page() if callable(page) else page, user_nav_children(), study_nav_children(study_name)
        
        except Exception as e:
            # Catch any unexpected errors and return a safe fallback
            print(f"Error in route callback for path '{pathname}': {e}")
            import traceback
            traceback.print_exc()
            error_content = html.Div([
                html.H3("Application Error"),
                html.P(f"An error occurred: {str(e)}"),
                html.P("Please check the server logs for more details.")
            ])
            try:
                nav = user_nav_children()
            except:
                nav = dbc.Nav()
            try:
                study_nav = study_nav_children("None")
            except:
                study_nav = dbc.Nav()
            return error_content, nav, study_nav
    
    # Navigate from login to register
    @app.callback(
        Output("login-nav", "href"),
        Input("go-register", "n_clicks"),
        prevent_initial_call=True
    )
    def to_register(n):
        if not n:
            raise dash.exceptions.PreventUpdate
        return "/register"

    # Handle sign in
    @app.callback(
        Output("login-error", "children"),
        Output("login-error", "is_open"),
        Output("login-redirect", "href"),
        Input("login-submit", "n_clicks"),
        State("login-username", "value"),
        State("login-password", "value"),
        prevent_initial_call=True
    )
    def do_login(n, username, pw):
        username = username.strip().lower()
        if not n:
            raise dash.exceptions.PreventUpdate
        if not (username and pw):
            return "Please enter username and password.", True, no_update
        
        u = get_user_by_username(username)
        if not u:
            return "Unknown username.", True, no_update

        # fetch hash
        eng = utils.db.get_database_engine()
        df = pd.read_sql(
            sqlalchemy.text("SELECT password_hash FROM gen_management.accounts WHERE user_id = :id"),
            eng, params={"id": u.user_id}
        )
        if df.empty:
            return "Account record missing.", True, no_update

        if not verify_password(pw, df.iloc[0]["password_hash"]):
            return "Incorrect password.", True, no_update

        login_user(u)
        # redirect to studies selection first
        return "", False, "/studies"

    @app.callback(
        Output("register-error", "children"),
        Output("register-error", "is_open"),
        Output("register-success", "children"),
        Output("register-success", "is_open"),
        Output("register-redirect", "href"),
        Input("reg-submit", "n_clicks"),
        State("reg-username", "value"),
        State("reg-email", "value"),
        State("reg-password", "value"),
        State("reg-password2", "value"),
        prevent_initial_call=True
    )
    def do_register(n, username, email, p1, p2):
        username = username.strip().lower()
        if not n:
            raise dash.exceptions.PreventUpdate
        if not re.fullmatch(r"[a-z0-9]{1,16}", username):
            return "Username must be 1-16 chars, letters and numbers only.", True, "", False, no_update
        if not all([username, email, p1, p2]):
            return "All fields are required.", True, "", False, no_update
        if not re.fullmatch(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$", email):
            return "Please enter a valid email address.", True, "", False, no_update
        if p1 != p2:
            return "Passwords do not match.", True, "", False, no_update
        # check if the username or already exists
        if get_user_by_username(username):
            return "Username already in use.", True, "", False, no_update
        try:
            user = create_user(username, email, p1)
            # auto-login
            login_user(user)
            return "", False, "Account created.", True, "/studies"
        except Exception as e:
            # likely UNIQUE violation on username/email
            return f"Could not create account: {e}", True, "", False, no_update

    @app.callback(
        Output("chpw-error", "children"),
        Output("chpw-error", "is_open"),
        Output("chpw-success", "children"),
        Output("chpw-success", "is_open"),
        Input("chpw-submit", "n_clicks"),
        State("chpw-current", "value"),
        State("chpw-new", "value"),
        State("chpw-confirm", "value"),
        prevent_initial_call=True,
    )
    def do_change_password(n, current_pw, new_pw, confirm_pw):
        if not n:
            raise dash.exceptions.PreventUpdate
        user = current_user()
        if not user:
            return "You must be logged in to change your password.", True, "", False
        if not all([current_pw, new_pw, confirm_pw]):
            return "All fields are required.", True, "", False
        if new_pw != confirm_pw:
            return "New passwords do not match.", True, "", False
        # if len(new_pw) < 8:
        #     return "New password must be at least 8 characters.", True, "", False
        eng = utils.db.get_database_engine()
        df = pd.read_sql(
            sqlalchemy.text("SELECT password_hash FROM gen_management.accounts WHERE user_id = :id"),
            eng, params={"id": user.user_id}
        )
        if df.empty:
            return "Account record missing.", True, "", False
        if not verify_password(current_pw, df.iloc[0]["password_hash"]):
            return "Current password is incorrect.", True, "", False
        try:
            update_password(user.user_id, hash_password(new_pw))
            return "", False, "Password updated successfully.", True
        except Exception as e:
            return f"Could not update password: {e}", True, "", False

    # Load study for current user
    @app.callback(
        Output("study-msg", "children"),
        Output("study-msg", "is_open"),
        Output("study-msg", "color"),
        Output("study-select", "options"),     # <- single owner
        Input("url", "pathname"),              # refresh list when /studies is shown
        Input("study-create", "n_clicks"),     # after creating a study
        State("study-name", "value"),
        prevent_initial_call=False
    )
    def studies_page_loader(pathname, n_create, new_name):
        cu = current_user()
        if not cu:
            # not logged in; message closed, no options
            return "", False, "", []

        trig = ctx.triggered_id
        eng = utils.db.get_database_engine()

        # A) Page load or nav to /studies -> just list
        if trig in (None, "url"):
            return "", False, "", utils.db.list_user_studies(cu.user_id)

        # B) Create study -> insert then reload list
        if trig == "study-create":
            name = (new_name or "").strip().lower()
            # study names cannot be "None"
            if name == "none":
                return "Study name cannot be 'None'. Please choose another name.", True, "danger", utils.db.list_user_studies(cu.user_id)
            # study name must be alphanumeric + underscores, 1-16 chars
            if not re.fullmatch(r"[a-z0-9_]{1,16}", name):
                return "Study names must be 1-16 chars, letters/numbers/underscore only.", True, "danger", utils.db.list_user_studies(cu.user_id)
            # check for existing study for this user
            if name in [s["label"].lower() for s in utils.db.list_user_studies(cu.user_id)]:
                return f"You already have a study named '{name}'.", True, "danger", utils.db.list_user_studies(cu.user_id)
            # insert
            with eng.begin() as conn:
                row = conn.execute(
                    sqlalchemy.text("""
                        INSERT INTO gen_management.studies (study_name, user_id, username, study_created)
                        VALUES (:n, :uid, :un, NOW())
                        RETURNING study_id, study_name
                    """),
                    {"n": name, "uid": cu.user_id, "un": cu.username}
                ).mappings().first()
                # update default analysis params
                conn.execute(sqlalchemy.text(
                    f"""
                        UPDATE gen_management.studies
                        SET
                            sliding_window_length = {WINDOW_LENGTHS["Sliding"]},
                            window_length_ratio = {WINDOW_LENGTH_RATIO},
                            short_window_length = {WINDOW_LENGTHS["Sliding"]} / {WINDOW_LENGTH_RATIO},
                            crash_join_distance = {CRASH_JOIN_DIST},
                            top_streets_threshold = {NUM_STREETS},
                            crash_weights = '{json.dumps(CRASH_WEIGHTS)}'::jsonb,
                            crash_costs = '{json.dumps(CRASH_COSTS)}'::jsonb,
                            bayesian_decay = {BAYESIAN_DECAY},
                            bayesian_max_prior_weight = {BAYESIAN_MAX_PRIOR_WEIGHT},
                            analysis_params_updated = NOW()
                        WHERE study_id = {row["study_id"]}
                    """)
                )
            msg = f"Created study '{row['study_name']}'. Select it under 'Your studies' and click 'Use this study' to start working with it."
            return msg, True, "info", utils.db.list_user_studies(cu.user_id)

        # Fallback
        return "", False, "", utils.db.list_user_studies(cu.user_id)

    # Use selected study and reload to whichever page it was on.
    @app.callback(
        Output("active-study", "data"),
        Output("studies-redirect", "href"),
        Output("study-next-step-modal", "is_open"),
        Output("study-next-step-study-name", "children"),
        Input("study-use", "n_clicks"),
        State("study-select", "value"),
        State("url", "pathname"),
        prevent_initial_call=True
    )
    def use_study(n, study_id, pathname):
        if not n:
            raise dash.exceptions.PreventUpdate
        if not study_id:
            return no_update, no_update, no_update, no_update
        session["active_study_id"] = int(study_id)
        selected_study_name = utils.db.get_study_name(study_id)
        session["study_name"] = selected_study_name
        session["user_study_name"] = f"{current_user().username}_{session['study_name']}"
        session.modified = True  # force session cookie flush before redirect fires
        # Use saved destination if we arrived here via the study gate.
        # If user selected a study on /studies directly, show a next-step modal.
        saved_redirect = session.pop("post_study_redirect", None)
        show_next_step_modal = pathname == "/studies" and not saved_redirect
        redirect_path = no_update if show_next_step_modal else (saved_redirect or pathname)
        print(f"[USE_STUDY] n={n} study_id={study_id} pathname={pathname!r} post_study_redirect={saved_redirect!r} show_next_step_modal={show_next_step_modal} → redirect_path={redirect_path!r}")
        return {"study_id": int(study_id)}, redirect_path, show_next_step_modal, selected_study_name

    @app.callback(
        Output("study-nav", "children", allow_duplicate=True),
        Input("active-study", "data"),
        prevent_initial_call=True,
    )
    def refresh_study_nav(active_study):
        if not active_study:
            raise dash.exceptions.PreventUpdate
        return study_nav_children(session.get("study_name", "None"))

    # ---------- LOAD DATA: tab switching via URL hash ----------
    @app.callback(
        Output("load_data-content", "children"),                 # matches page id
        Output("ld-step-study", "className"),
        Output("ld-step-roads", "className"),
        Output("ld-step-crashes", "className"),
        Output("ld-step-review", "className"),
        Input("url", "hash"),
        State("url", "pathname"),
        prevent_initial_call=False
    )
    def switch_load_section(url_hash, page_url):
        # only need to switch if on load data page
        if page_url != "/load_data":
            raise dash.exceptions.PreventUpdate
        from ..pages.load_data import study_area_panel, roads_panel, crashes_panel, review_panel
        section = (url_hash or "#study").lower()

        active = "ld-step"
        muted  = "ld-step ld-step-muted"

        if section.startswith("#roads"):
            return roads_panel(), muted, active, muted, muted
        if section.startswith("#crash"):
            return crashes_panel(), muted, muted, active, muted
        if section.startswith("#review"):
            return review_panel(), muted, muted, muted, active
        # default
        return study_area_panel(), active, muted, muted, muted
    

    # ---------- Toggle preset vs local study area controls ----------
    @app.callback(
        Output("preset-controls", "style"),
        Output("local-controls", "style"),
        Input("study-source", "value"),
        prevent_initial_call=False
    )
    def toggle_source(src):
        if src == "local":
            return {"display": "none"}, {"display": "block"}
        return {"display": "block"}, {"display": "none"}

    # ---------- Populate counties whenever the state changes ----------
    @app.callback(
        Output("preset-county", "options"),
        Output("preset-county", "value"),
        Output("study-state-load-busy", "is_open"),
        Input("preset-state", "value"),
        prevent_initial_call=False
    )
    def update_county_dropdown(state_name):
        time.sleep(1)  # to show busy
        if not state_name:
            return [], None, False
        counties = ld.get_county_list(state_name)  # uses your DB
        opts = [{"label": c, "value": c} for c in counties]
        return opts, None, False
    
    # -----------------------------------------------------------------------------------------------------------------------------
    # mapping study area selection (preset county or upload) to the leaflet layer
    # Note: this single callback handles both the preset county selection and the upload, based on which Input triggered it
    @app.callback(
        Output("load-study-status", "children"),
        Output("study-layer", "children"),
        Output("study-area-store", "data"),
        Output("confirm-study", "disabled"), 
        Output("study-county-load-busy", "is_open"),
        Output("study-shp-load-busy", "is_open"),
        Input("preset-county", "value"),
        Input("upload-study", "contents"),
        Input("upload-study", "filename"),
        State("preset-state", "value"),
        prevent_initial_call=True
    )
    def update_study_layer(county_value, upload_contents, upload_filename, state_value):
        trigger = ctx.triggered_id

        # Style helper
        style = dict(color="#fcba03", weight=2, fillColor="#a6bddb", fillOpacity=0.35)

        # 1) County selection path
        if trigger == "preset-county":
            time.sleep(1)  # to show busy
            if not (state_value and county_value):
                raise dash.exceptions.PreventUpdate
            geom = ld.get_county_geojson(state_value, county_value)  # dict (EPSG:4326)
            msg = f"Loaded {county_value}, {state_value}."
            store = {"source": f"Preset County ({county_value}, {state_value})", "geometry": geom}
            disable_confirm = False


        # 2) Upload path
        if trigger == "upload-study":
            time.sleep(1)  # to show busy
            if not (upload_contents and upload_filename):
                raise dash.exceptions.PreventUpdate

            # validate the shapefile upload
            try:
                valid, gdf, msg = ld.validate_and_read_shp(upload_contents, upload_filename, max_mb=20, geom_types=["Polygon","MultiPolygon"])
            except Exception as e:
                msg = preset_modals.simple_modal(message=f"Error reading shapefile upload: {e}", color="red")
                return msg, [], None, True, False, False

            if not valid:
                msg = preset_modals.simple_modal(message=msg, color="red")
                layer = []
                store = None
                disable_confirm = True
                geom = None
            else:
                # load it to a staging table and read back the dissolved geometry
                try:
                    cu = current_user()
                    un = cu.username
                    sn = session["study_name"]
                    ld.gdf_to_table(gdf=gdf, username=un, study_name=sn, table_name_prefix="_tmp_study_area", schema="tmp_staging")
                    geom=ld.get_dissolved_geojson(table=f"_tmp_study_area_{un}_{sn}".lower(), schema="tmp_staging")
                    store = {"source": "User Upload", "geometry": geom}
                    msg = f"Loaded geometry from {upload_filename}."
                    disable_confirm = False
                except Exception as e:
                    msg = preset_modals.simple_modal(message=f"Error loading shapefile: {e}", color="red")
                    layer = []
                    store = None
                    disable_confirm = True
                    geom = None

        if geom:
            feature = {"type": "Feature", "properties": {}, "geometry": geom}
            layer = [dl.GeoJSON(data=feature, id="study-geojson-layer", zoomToBounds=True, options=dict(style=style))]
        else:
            layer = []

        return msg, layer, store, disable_confirm, False, False

    # Show busy modal when reading upload
    @app.callback(
        Output("study-shp-load-busy", "is_open", allow_duplicate=True),
        Input("upload-study", "contents"),
        Input("upload-study", "filename"),
        prevent_initial_call=True
    )
    def show_study_busy(upload_contents, upload_filename):
        if not (upload_contents and upload_filename):
            raise dash.exceptions.PreventUpdate
        return True
    
    # show busy when loading preset county or state
    @app.callback(
        Output("study-state-load-busy", "is_open", allow_duplicate=True),
        Input("preset-state", "value"),
        prevent_initial_call=True
    )
    def show_state_busy(state_value):
        if not state_value:
            raise dash.exceptions.PreventUpdate
        return True
    
    @app.callback(
        Output("study-county-load-busy", "is_open", allow_duplicate=True),
        Input("preset-county", "value"),
        prevent_initial_call=True
    )
    def show_county_busy(county_value):
        if not county_value:
            raise dash.exceptions.PreventUpdate
        return True

    # ---------- Confirm study area selection ----------
    @app.callback(
        Output("confirm-study-msg", "children"),
        Output("study-confirm-busy", "is_open"),
        Input("confirm-study", "n_clicks"),
        State("study-area-store", "data"),
        prevent_initial_call=True
    )
    def confirm_study_area(n, store):
        time.sleep(1)  # to show busy
        if not n:
            raise dash.exceptions.PreventUpdate
        if not store:
            return "Nothing to save — pick a county or upload a ZIP first.", False

        cu = current_user()
        un = cu.username
        sn = session["study_name"]

        geometry = store.get("geometry")
        if not geometry:
            return "No geometry to save.", False

        state_abbr = ld._get_state_from(shape(geometry))
        if state_abbr is None:
            return "Please ensure the provided study area is located within the United States.", False

        try:
            # Save to DB
            ld.upload_study_area(store=store, username=un, study_name=sn, state_abbr=state_abbr)

            # copy staging area table to received
            if store.get("source", "").startswith("User Upload"):
                eng = utils.db.get_database_engine()
                with eng.begin() as conn:
                    conn.execute(sqlalchemy.text(f"""
                        DROP TABLE IF EXISTS received.study_area_{un}_{sn};
                        CREATE TABLE received.study_area_{un}_{sn} AS (
                            SELECT * FROM tmp_staging._tmp_study_area_{un}_{sn}
                        );
                        DROP TABLE IF EXISTS tmp_staging._tmp_study_area_{un}_{sn};
                    """))
            else:
                # if preset county, ensure any received tables from before are dropped
                utils.db.drop_table(table_name=f"study_area_{un}_{sn}", schema="received")

            return preset_modals.study_upload_success_modal(), False
        except Exception as exc:
            print(f"Error saving study area for {un}/{sn}: {exc}")
            return preset_modals.simple_modal(message=f"Error saving study area: {exc}", color="red"), False
    
    # show busy modal when user presses confirm
    @app.callback(
        Output("study-confirm-busy", "is_open", allow_duplicate=True),
        Input("confirm-study", "n_clicks"),
        prevent_initial_call=True
    )
    def show_study_busy(n):
        if not n:
            raise dash.exceptions.PreventUpdate
        return True
    
    # -----------------------------------------------------------------------------------------------------------------------------

    # ---------- Callbacks for roads uploads  ----------
    @app.callback(
        Output("roads-upload-wrap", "style"),
        Output("upload-roads-status", "children"),
        Output("roads-options-wrap", "children"),
        Output("roads-options-wrap", "style"),
        Output("roads-col-select-wrap", "children"),
        Output("roads-col-select-wrap", "style"),
        Output("roads-fclass-mapping-wrap", "children"),
        Output("roads-fclass-mapping-wrap", "style"),
        Output("fclass-assign", "disabled"),
        Output("fclass-assign-msg", "children"),
        Output("confirm-roads", "disabled"),
        Output("confirm-roads-msg", "children"),
        Output("roads-store", "data"),
        Output("roads-shp-load-busy", "is_open"),

        # inputs
        Input("roads-source", "value"),
        Input("upload-roads", "contents"),
        Input("fclass-assign", "n_clicks"),
        Input("roads-fclass-col", "value"),

        # states
        State("upload-roads", "filename"),
        State("roads-store", "data"),
        State("roads-id-col", "value"),
        State("roads-name-col", "value"),
        prevent_initial_call=True
    )

    def manage_roads_inputs(roads_source, upload_contents, fclass_n, fclass_col,
                            upload_filename, store, id_col, name_col):
        trig = ctx.triggered_id
        cu = current_user()
        sn = session["study_name"] if cu else None

        # set all outputs to no_update initially
        roads_upload_wrap_style = dash.no_update
        upload_status_children = dash.no_update
        roads_options_children = dash.no_update
        roads_options_style = dash.no_update
        roads_col_select_children = dash.no_update
        roads_col_select_style = dash.no_update
        roads_fclass_mapping_children = dash.no_update
        roads_fclass_mapping_style = dash.no_update
        fclass_assign_disabled = dash.no_update
        fclass_assign_msg = dash.no_update
        confirm_disabled = dash.no_update
        confirm_msg_children = dash.no_update
        roads_store = store or dash.no_update

        # if the trigger is roads-source, reset page to initial state for that source
        if trig == "roads-source":
            if roads_source == "upload":
                roads_upload_wrap_style = {"display": "block"}
                fclass_assign_disabled = True
            else:
                roads_upload_wrap_style = {"display": "none"}
                fclass_assign_disabled = False
            # hide everything else
            roads_options_style = {"display": "none"}
            confirm_disabled = True
            confirm_msg_children = []
            upload_status_children = []
            # also reset store
            roads_store = {"source": roads_source} if roads_source == "builtin" else None

        # if the trigger is upload-roads, validate and stage the upload
        elif trig in ("upload-roads"):
            time.sleep(1)  # to show busy
            try:
                valid, gdf, msg = ld.validate_and_read_shp(upload_contents=upload_contents, upload_filename=upload_filename, max_mb=20, geom_types=["LineString","MultiLineString"])
                if not valid:
                    upload_status_children = dbc.Alert(msg, color="danger")
                    roads_upload_wrap_style = dash.no_update
                    roads_options_style = {"display": "none"}
                else:
                    # stage into received schema
                    ld.gdf_to_table(gdf=gdf, username=cu.username, study_name=sn, table_name_prefix="_tmp_roads", schema="tmp_staging")
                    # update store with staging info
                    roads_store = {
                        "source": "upload",
                        "staging_table": f"tmp_staging._tmp_roads_{cu.username}_{sn}",
                        "received_table": None,
                        "staging_crs": gdf.crs.to_epsg()
                    }
                    upload_status_children = dbc.Alert(f"Loaded {upload_filename} (~{int(len(upload_contents.split(',',1)[1])*3/4/1024):,} KB).", color="success")
                    roads_col_select_children = ld.roads_cols_card(
                        id_col_options=ld.get_col_list(table=f"_tmp_roads_{cu.username}_{sn}".lower(), schema="tmp_staging", col_types=["integer","bigint", "double precision", "text"]),
                        name_col_options=ld.get_col_list(table=f"_tmp_roads_{cu.username}_{sn}".lower(), schema="tmp_staging", col_types=["text"]),
                        fclass_col_options=ld.get_col_list(table=f"_tmp_roads_{cu.username}_{sn}".lower(), schema="tmp_staging", col_types=["text"]),
                    )

                    roads_upload_wrap_style = {"display": "none"}
                    roads_options_style = {"display": "block"}
                    roads_col_select_style = {"display": "block"}
                    if fclass_col:
                        fclass_assign_disabled = False
                        roads_fclass_mapping_style = {"display": "None"}
                    else:
                        fclass_assign_disabled = True
            except Exception as e:
                upload_status_children = dbc.Alert(f"Error processing roads upload: {e}", color="danger")
                roads_upload_wrap_style = {"display": "block"}
                roads_options_style = {"display": "none"}
                roads_col_select_style = {"display": "none"}
                roads_fclass_mapping_style = {"display": "none"}
                fclass_assign_disabled = True
                confirm_disabled = True
                roads_store = None

        # if the trigger is fclass-assign, validate inputs and generate the fclass mapping UI
        elif trig == "fclass-assign":
            if roads_source == "builtin":
                roads_options_style = {"display": "block"}
                roads_col_select_style = {"display": "none"}
                roads_fclass_mapping_style = {"display": "block"}
                fclass_assign_msg = dbc.Alert("Using built-in OSM roads; adjust functional class assignment as needed", color="info")
                roads_fclass_mapping_children = [ld.attribute_map_card(attr_type="fclass", source="builtin")]
                
            elif roads_source == "upload" and store and store.get("staging_table"):
                fclass_assign_msg = dbc.Alert("Please assign functional class values to standardized values", color="info")
                roads_fclass_mapping_children = [ld.attribute_map_card(attr_type="fclass", source="upload", source_table=store.get("staging_table"), attr_col=fclass_col)]
                roads_fclass_mapping_style = {"display": "block"}
                    
            fclass_assign_disabled = True
            confirm_disabled = False
            confirm_msg_children = preset_modals.info_modal(title="", message="Verify all the functional class assignments before confirming. \
                                                                            Click 'Confirm Roads' when ready to upload roads for your study. \
                                                                            Click anywhere outside of this message box to dismiss this message.")                

        elif trig == "roads-fclass-col" and roads_source == "upload":
            fclass_assign_disabled = False

        return (
            roads_upload_wrap_style,
            upload_status_children,
            roads_options_children,
            roads_options_style,
            roads_col_select_children,
            roads_col_select_style,
            roads_fclass_mapping_children,
            roads_fclass_mapping_style,
            fclass_assign_disabled,
            fclass_assign_msg,
            confirm_disabled,
            confirm_msg_children,
            roads_store,
            False
        )

    # Call a busy when user uploads their shapefile
    @app.callback(
        Output("roads-shp-load-busy", "is_open", allow_duplicate=True),

        Input("upload-roads", "contents"),
        Input("upload-roads", "filename"),

        State("roads-source", "value"),
        prevent_initial_call=True
    )
    def roads_shp_busy(contents, filename, source):
        if source != "upload":
            raise dash.exceptions.PreventUpdate
        if not (contents and filename):
            raise dash.exceptions.PreventUpdate
        return True

    # ---------- Collect fclass mapping everytime it changes ----------
    @app.callback(
        Output("roads-store", "data", allow_duplicate=True),

        Input({"type": "fclass-dd", "index": dash.dependencies.ALL}, "value"),

        # get the raw values from the form state
        State({"type": "fclass-raw", "index": dash.dependencies.ALL}, "children"),
        State("roads-store", "data"),
        prevent_initial_call=True
    )
    def collect_fclass_mapping(mapped_values, raw_values, store):
        fclass_map = {raw: val for raw, val in zip(raw_values, mapped_values) if val}
        new_store = (store or {}).copy()
        new_store["fclass_map"] = fclass_map
        return new_store

    # ---------- Confirm roads upload ----------
    @app.callback(
        Output("confirm-roads-msg", "children", allow_duplicate=True),
        Output("roads-confirm-busy", "is_open"),

        Input("confirm-roads", "n_clicks"),

        State("roads-store", "data"),
        State("roads-id-col", "value"),
        State("roads-name-col", "value"),
        State("roads-fclass-col", "value"),
        prevent_initial_call=True
    )

    def confirm_roads(n, store, id_col, name_col, fclass_col):
        time.sleep(1)
        if not n:
            raise dash.exceptions.PreventUpdate
        if not store:
            return preset_modals.simple_modal(message="No roads datastore found!", color="red"), False
        if store.get("source") == "upload":
            if not id_col:
                return preset_modals.simple_modal(message="No ID column selected!", color="red"), False
            if not name_col:
                return preset_modals.simple_modal(message="No Name column selected!", color="red"), False
            if not fclass_col:
                return preset_modals.simple_modal(message="No Functional Class column selected!", color="red"), False
        if not store.get("fclass_map"):
            return preset_modals.simple_modal(message="No functional class mapping found!", color="red"), False

        un = current_user().username
        sn = session["study_name"]
        
        # need a study area first
        if not utils.db.table_exists(table_name=f"study_area_{un}_{sn}", schema="inputs"):
            return preset_modals.simple_modal(message="No study area found! Please upload a study area before loading roads.", color="red"), False
        
        # ensure ID column is unique and no nulls are present
        if store.get("source") == "upload":
            is_unique, no_nulls = ld.check_uniqueness(store.get("staging_table"), id_col)
            # check id uniqueness
            if not is_unique:
                return preset_modals.simple_modal(message=f"ID column '{id_col}' contains duplicate values or NULL values. Please select a different column.", color="red"), False

        load_success, message = ld.upload_roads_formatted(username=un, study_name=sn, store=store, id_col=id_col, name_col=name_col, fclass_col=fclass_col)
        if not load_success:
            return preset_modals.simple_modal(message=message, color="red"), False
        else:
            return preset_modals.roads_upload_success_modal(), False
        
    # Show busy when confirming roads
    @app.callback(
        Output("roads-confirm-busy", "is_open", allow_duplicate=True),
        Input("confirm-roads", "n_clicks"),
        prevent_initial_call=True
    )
    def roads_confirm_busy(n):
        if not n:
            raise dash.exceptions.PreventUpdate
        return True
        
    # ------------- Crash data upload callbacks -------------
    # toggle crash attribute options based on source
    @app.callback(
        Output("crashes-upload-wrap", "style"),
        Output("crashes-store", "data", allow_duplicate=True),
        Input("crashes-source", "value"),
        prevent_initial_call=True
    )
    def toggle_crash_source(src):
        if src == "upload":
            return {"display": "block"}, {"source": "upload"}
        else:
            return {"display": "none"}, {"source": "builtin"}

    # Once file uploads, enable column selection
    @app.callback(
        Output("upload-crashes-status", "children"),
        Output("crashes-col-select-wrap", "children"),
        Output("crashes-store", "data"),
        Output("crashes-shp-load-busy", "is_open"),

        Input("upload-crashes", "contents"),
        Input("upload-crashes", "filename"),
        prevent_initial_call=True
    )
    def on_crashes_upload(contents, filename):
        time.sleep(1)
        # first validate shp
        if not contents or not filename:
            raise dash.exceptions.PreventUpdate

        try:
            valid, gdf, msg = ld.validate_and_read_shp(contents, filename, max_mb=20, geom_types=["Point"])
            if not valid:
                upload_status = dbc.Alert(msg, color="danger")
                return upload_status, dash.no_update, dash.no_update, False

            # stage into staging schema
            cu = current_user()
            un = cu.username
            sn = session["study_name"]
            ld.gdf_to_table(gdf=gdf, username=un, study_name=sn, table_name_prefix="_tmp_crashes", schema="tmp_staging")

            # update store with staging info
            new_store = {"source": "upload", "staging_table": f"tmp_staging._tmp_crashes_{un}_{sn}", "received_table": None, "staging_crs": gdf.crs.to_epsg()}

            [s, t] = new_store["staging_table"].split(".")
            crashes_col_select_children = [
                ld.column_dropdown(id_="crash-id-col", label="ID Column", options=ld.get_col_list(table=t, schema=s, col_types=["integer","bigint", "double precision", "text"]), value=None),
                ld.column_dropdown(id_="crash-year-col", label="Year Column", options=ld.get_col_list(table=t, schema=s, col_types=["integer","bigint", "double precision"]), value=None),
                ld.column_dropdown(id_="crash-mode-col", label="Crash Mode Column", options=ld.get_col_list(table=t, schema=s, col_types=["integer","bigint", "double precision", "text"]), value=None),
                ld.column_dropdown(id_="crash-severity-col", label="Injury Severity Column", options=ld.get_col_list(table=t, schema=s, col_types=["integer","bigint", "double precision", "text"]), value=None),
            ]

            upload_status = dbc.Alert(f"Loaded {filename} (~{int(len(contents.split(',',1)[1])*3/4/1024):,} KB). Make your column selection", color="success")

            return upload_status, crashes_col_select_children, new_store, False
        except Exception as e:
            upload_status = dbc.Alert(f"Error processing crashes upload: {e}", color="danger")
            return upload_status, dash.no_update, dash.no_update, False
    
    # Show busy when user uploads their shapefile
    @app.callback(
        Output("crashes-shp-load-busy", "is_open", allow_duplicate=True),
        Input("upload-crashes", "contents"),
        Input("upload-crashes", "filename"),
        prevent_initial_call=True
    )
    def crashes_shp_busy(contents, filename):
        if not (contents and filename):
            raise dash.exceptions.PreventUpdate
        return True
    
    # Whenever crash mode/severity cols change, toggle assignments button
    @app.callback(
        Output("crash-mode-assign", "disabled"),
        Input("crash-mode-col", "value"),
        prevent_initial_call=True
    )
    def toggle_crash_mode_button(mode_col):
        return False if mode_col else True

    @app.callback(
        Output("crash-severity-assign", "disabled"),
        Input("crash-severity-col", "value"),
        prevent_initial_call=True
    )
    def toggle_crash_severity_button(severity_col):
        return False if severity_col else True
        
    # Create crash mode mapping when button is clicked
    @app.callback(
        Output("crash-mode-mapping-wrap", "children"),
        Output("crash-mode-mapping-wrap", "style"),
        Output("crash-mode-assign", "disabled", allow_duplicate=True),
        Output("crash-attr-assign-msg", "children", allow_duplicate=True),

        Input("crash-mode-assign", "n_clicks"),

        State("crash-mode-col", "value"),
        State("crashes-store", "data"),
        prevent_initial_call=True
    )
    def assign_crash_mode(n, mode_col, store):
        if not n:
            raise dash.exceptions.PreventUpdate
        if not store:
            return dash.no_update, dash.no_update, dash.no_update, preset_modals.simple_modal(message="No crashes datastore found!", color="red")
        if not mode_col:
            return dash.no_update, dash.no_update, dash.no_update, preset_modals.simple_modal(message="No crash mode column selected!", color="red")

        table = store.get("staging_table")
        mapping_card = ld.attribute_map_card(attr_type="crash_mode", source="upload", source_table=table, attr_col=mode_col)

        return mapping_card, {"display": "block"}, True, preset_modals.simple_modal(message="Please assign crash mode values to standardized values", color="info")
    
    # Create crash severity mapping when button is clicked
    @app.callback(
        Output("crash-severity-mapping-wrap", "children"),
        Output("crash-severity-mapping-wrap", "style"),
        Output("crash-severity-assign", "disabled", allow_duplicate=True),
        Output("crash-attr-assign-msg", "children", allow_duplicate=True),

        Input("crash-severity-assign", "n_clicks"),

        State("crash-severity-col", "value"),
        State("crashes-store", "data"),
        prevent_initial_call=True
    )
    def assign_crash_severity(n, severity_col, store):
        if not n:
            raise dash.exceptions.PreventUpdate
        if not store:
            return dash.no_update, dash.no_update, dash.no_update, preset_modals.simple_modal(message="No crashes datastore found!", color="red")
        if not severity_col:
            return dash.no_update, dash.no_update, dash.no_update, preset_modals.simple_modal(message="No crash severity column selected!", color="red")

        table = store.get("staging_table")
        mapping_card = ld.attribute_map_card(attr_type="crash_severity", source="upload", source_table=table, attr_col=severity_col)

        return mapping_card, {"display": "block"}, True, preset_modals.simple_modal(message="Please assign crash severity values to standardized values", color="info")
    
    # Collect crash mode mapping everytime it changes
    @app.callback(
        Output("crashes-store", "data", allow_duplicate=True),
        Input({"type": "crash_mode-dd", "index": dash.dependencies.ALL}, "value"),
        State({"type": "crash_mode-raw", "index": dash.dependencies.ALL}, "children"),
        State("crashes-store", "data"),
        prevent_initial_call=True
    )

    def collect_crash_mode_mapping(mapped_values, raw_values, store):
        crash_mode_map = {raw: val for raw, val in zip(raw_values, mapped_values) if val}
        new_store = (store or {}).copy()
        new_store["crash_mode_map"] = crash_mode_map
        return new_store
    
    # Collect crash severity mapping everytime it changes
    @app.callback(
        Output("crashes-store", "data", allow_duplicate=True),
        Input({"type": "crash_severity-dd", "index": dash.dependencies.ALL}, "value"),
        State({"type": "crash_severity-raw", "index": dash.dependencies.ALL}, "children"),
        State("crashes-store", "data"),
        prevent_initial_call=True
    )
    def collect_crash_severity_mapping(mapped_values, raw_values, store):
        crash_severity_map = {raw: val for raw, val in zip(raw_values, mapped_values) if val}
        new_store = (store or {}).copy()
        new_store["crash_severity_map"] = crash_severity_map
        return new_store
    
    # Confirm crashes upload
    @app.callback(
        Output("confirm-crashes-msg", "children"),
        Output("crashes-confirm-busy", "is_open"),
        Input("confirm-crashes", "n_clicks"),
        State("crashes-store", "data"),
        State("crash-id-col", "value"),
        State("crash-year-col", "value"),
        State("crash-mode-col", "value"),
        State("crash-severity-col", "value"),
        prevent_initial_call=True
    )

    def confirm_crashes(n, store, id_col, year_col, mode_col, severity_col):
        time.sleep(1)
        if not n:
            raise dash.exceptions.PreventUpdate
        if not store:
            return preset_modals.simple_modal(message="No crashes datastore found!", color="red"), False
        if store.get("source") == "upload":
            if not store.get("staging_table"):
                return preset_modals.simple_modal(message="No crashes upload found!", color="red"), False
            if not id_col:
                return preset_modals.simple_modal(message="No ID column selected!", color="red"), False
            if not year_col:
                return preset_modals.simple_modal(message="No Year column selected!", color="red"), False
            if not mode_col:
                return preset_modals.simple_modal(message="No Crash Mode column selected!", color="red"), False
            if not severity_col:
                return preset_modals.simple_modal(message="No Injury Severity column selected!", color="red"), False
            if not store.get("crash_mode_map"):
                return preset_modals.simple_modal(message="No crash mode assignment found!\nPlease assign crash modes", color="red"), False
            if not store.get("crash_severity_map"):
                return preset_modals.simple_modal(message="No crash severity assignment found!\nPlease assign crash severities", color="red"), False

        un = current_user().username
        sn = session["study_name"]

        # need a study area first
        if not utils.db.table_exists(table_name=f"study_area_{un}_{sn}", schema="inputs"):
            return preset_modals.simple_modal(message="No study area found! Please upload a study area before loading crashes.", color="red"), False

        load_success, message = ld.upload_crashes_formatted(username=un, study_name=sn, 
                                                            store=store, id_col=id_col, year_col=year_col, 
                                                            mode_col=mode_col, severity_col=severity_col)

        if not load_success:
            return preset_modals.simple_modal(message=message, color="red"), False
        else:
            return preset_modals.crashes_upload_success_modal(), False
        
    # Show busy when confirming crashes
    @app.callback(
        Output("crashes-confirm-busy", "is_open", allow_duplicate=True),
        Input("confirm-crashes", "n_clicks"),
        prevent_initial_call=True
    )
    def crashes_confirm_busy(n):
        if not n:
            raise dash.exceptions.PreventUpdate
        return True
        
    # ------------------------ Review Inputs Page ------------------------
    # Populate data checks on page load
    @app.callback(
        Output("data-issues-output", "children"),
        Output("map-input-data", "disabled"),
        Output("data-issues-busy", "is_open"),
        Input("url", "hash"),
        prevent_initial_call=False
    )
    def populate_data_checks(url_hash):
        # only run when on review tab
        if url_hash != "#review":
            raise dash.exceptions.PreventUpdate

        cu = current_user()
        un = cu.username
        sn = session["study_name"]

        try:
            passed, critical_issues, info_msgs, success_msgs = ld.perform_data_checks(username=un, study_name=sn)
        except Exception as exc:
            print(f"Error performing data checks for {un}/{sn}: {exc}")
            return dbc.Alert(f"Error loading data checks: {exc}", color="danger"), True, False

        if not passed:
            alerts = dbc.Alert(
                [
                    html.P([
                        html.Span("❌ ", style={"color": "#dc3545", "fontWeight": "bold", "fontSize": "1.2em"}),
                        "Some critical issues were found with your data:"
                    ], className="mb-2"),
                    html.Ul([html.Li(issue) for issue in critical_issues])
                ],
                color="danger"
            )
            disable_map = True
        else:
            success_alert = []
            info_alert = []
            disable_map = False
            if len(success_msgs) > 0:
                success_alert = dbc.Alert(
                    [
                        html.P([
                            html.Span("✅ ", style={"color": "#28a745", "fontWeight": "bold", "fontSize": "1.2em"}),
                            "All critical checks passed! Here are some details:"
                        ], className="mb-2"),
                        html.Ul([html.Li(msg) for msg in success_msgs])
                    ],
                    color="success"
                )
            if len(info_msgs) > 0:
                info_alert = dbc.Alert(
                    [
                        html.P([
                            html.Span("⚠️ ", style={"color": "#ffc107", "fontWeight": "bold", "fontSize": "1.2em"}),
                            "Notes about your input data:"
                        ], className="mb-2"),
                        html.Ul([html.Li(msg) for msg in info_msgs])
                    ],
                    color="warning"
                )
            alerts = [
                info_alert,
                html.Hr() if info_alert and success_alert else None,
                success_alert
            ]
        return alerts, disable_map, False
    
    # update map panel with input geometries
    @app.callback(
        Output("input-study-layer", "children"),
        Output("input-roads-layer", "children"),
        Output("input-crashes-layer", "children"),
        Output("inputs-review-map-truncation-notice", "children"),
        Output("inputs-review-map-legend", "style"),
        Output("inputs-review-map-busy", "is_open"),

        Input("map-input-data", "n_clicks"),
        State("inputs-review-map-legend", "style"),
        prevent_initial_call=True
    )

    def map_input_data(n, legend_style):
        time.sleep(1)  # to show busy
        if not n:
            raise dash.exceptions.PreventUpdate

        cu = current_user()
        un = cu.username
        sn = session["study_name"]

        try:
            study_layer, roads_layer, crashes_layer, truncation_meta = ld.load_inputs_review_map(
                username=un,
                study_name=sn,
                max_crash_markers=MAX_MAP_CRASH_MARKERS,
            )

            legend_style = legend_style or {}
            legend_style["display"] = "block"

            if truncation_meta:
                truncation_notice = dbc.Alert(
                    f"Crash points were limited for map performance. Showing {truncation_meta['rendered']:,} of {truncation_meta['total']:,} crashes (max {truncation_meta['max']:,}).",
                    color="warning",
                    className="mb-2",
                )
            else:
                truncation_notice = None

            return study_layer, roads_layer, crashes_layer, truncation_notice, legend_style, False
        except Exception as exc:
            print(f"Error loading input data map for {un}/{sn}: {exc}")
            return no_update, no_update, no_update, dbc.Alert(f"Error loading map: {exc}", color="danger"), no_update, False
        # return [study_layer], [roads_layer], [crashes_layer], False

    # show busy when Mapping Input Data button is clicked
    @app.callback(
        Output("inputs-review-map-busy", "is_open", allow_duplicate=True),
        Input("map-input-data", "n_clicks"),
        prevent_initial_call=True
    )
    def map_input_data_busy(n):
        if not n:
            raise dash.exceptions.PreventUpdate
        return True
    
    # # ---------- Busy modal toggle ----------
    # @app.callback(
    #     Output("busy-modal", "is_open"),
    #     Output("busy-text", "children"),
    #     Input("busy", "data"),
    #     prevent_initial_call=False
    # )
    # def toggle_busy(is_busy):
    #     return bool(is_busy), ("Please wait while we process your data." if is_busy else "")
