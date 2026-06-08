import os
from pathlib import Path
from dotenv import load_dotenv
from .global_vars import CANONICAL_URL

# Load environment variables from multiple possible locations (priority order)
env_locations = [
    "/etc/sspf/sspf.env",  # Production location
    Path(__file__).resolve().parents[1] / ".env",  # Standard dotenv location
]

env_loaded = False
for env_path in env_locations:
    try:
        if Path(env_path).exists():
            load_dotenv(env_path)
            env_loaded = True
            print(f"✓ Loaded environment from: {env_path}")
            break
    except PermissionError:
        # Can't read this file (e.g., /etc/sspf/sspf.env without root), try next
        continue

if not env_loaded:
    print("⚠️  No accessible environment file found. Using system environment variables.")

import dash
import dash_bootstrap_components as dbc
from flask import Flask, request, session, redirect, render_template_string, Response, abort, send_file
from .layout import make_layout
from .callbacks import (
    register_callbacks, 
    register_analysis_callbacks, 
    register_busy_modals_callbacks,
    register_dashboard_callbacks,
    register_map_vis_callbacks
)
from .auth import AUTH_ENABLED, current_user
from .auth import login_manager

server = Flask(__name__)
server.secret_key = os.getenv("FLASK_SECRET_KEY", "dev-secret-change-me")

login_manager.init_app(server)

og_image_url = f"{CANONICAL_URL}/assets/Network%20Output_v4.png"

app = dash.Dash(
    __name__,
    server=server,
    assets_folder=str(Path(__file__).parent / "assets"),
    external_stylesheets=[
        dbc.themes.BOOTSTRAP,
        # "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"
        "/assets/bootstrap-icons.css",
    ],
    suppress_callback_exceptions=True,
    title="SSPF",
    meta_tags=[
        {"property": "og:type", "content": "website"},
        {"property": "og:url", "content": f"{CANONICAL_URL}/"},
        {"property": "og:title", "content": "Safer Streets Priority Finder"},
        {
            "property": "og:description",
            "content": "A free and open-source safety analysis tool.",
        },
        {"property": "og:image", "content": og_image_url},
        {"name": "twitter:card", "content": "summary_large_image"},
        {"name": "twitter:title", "content": "Safer Streets Priority Finder"},
        {
            "name": "twitter:description",
            "content": "A free and open-source safety analysis tool.",
        },
        {"name": "twitter:image", "content": og_image_url},
    ],
)

app.layout = make_layout()

from .pages.welcome import layout as welcome_layout
from .pages.overview import layout as overview_layout
from .pages.instructions import layout as instructions_layout
from .pages.load_data import layout as load_data_layout
from .pages.login import layout as login_layout

@server.route("/logout")
def logout():
    session.clear()
    return redirect("/")

_STAGING_DIR = (Path(__file__).resolve().parent.parent / "staging").resolve()

@server.route("/download/<path:filename>")
def download_file(filename):
    if AUTH_ENABLED:
        user = current_user()
        if not user:
            abort(403)
        if not (filename.startswith(f"sspf_report_{user.username}_") or
                filename.startswith(f"sspf_study_data_{user.username}_")):
            abort(403)
    target = (_STAGING_DIR / filename).resolve()
    if not str(target).startswith(str(_STAGING_DIR)):
        abort(403)
    if not target.exists():
        abort(404)
    if request.headers.get("X-Real-IP"):
        resp = Response()
        resp.headers["X-Accel-Redirect"] = f"/protected-downloads/{filename}"
        resp.headers["Content-Disposition"] = f'attachment; filename="{target.name}"'
        resp.headers["Content-Type"] = "application/octet-stream"
        return resp
    return send_file(target, as_attachment=True)

# --- Register callbacks AFTER app is created and layout defined ---
register_callbacks(app)
register_busy_modals_callbacks(app)
register_analysis_callbacks(app)
register_dashboard_callbacks(app)
register_map_vis_callbacks(app)

if __name__ == "__main__":
    port = int(os.getenv("PORT", 8050))      # default 8050
    host = os.getenv("HOST", "127.0.0.1")    # default localhost
    debug = os.getenv("DASH_DEBUG", "1") == "1"
    app.run(host=host, port=port, debug=debug)