# src/layout.py
from dash import html, dcc
import dash_bootstrap_components as dbc
from .auth import current_user
from .global_vars import CONTACT_EMAIL

def user_nav_children():
    try:
        user = current_user()
    except:
        user = None
    
    if user:
        # User from Flask-Login; show their username
        name = getattr(user, "username", "User")
        return dbc.Nav(
            [
                # mode_badge,
                dbc.DropdownMenu(
                    label=name,
                    nav=True,
                    in_navbar=True,
                    align_end=True,
                    children=[
                        dbc.DropdownMenuItem("Change password", href="/change_password"),
                        dbc.DropdownMenuItem("Sign out", href="/logout"),
                    ],
                ),
            ],
            className="d-flex align-items-center",
            navbar=True,
        )
    # Signed out view
    return dbc.Nav(
        [dbc.NavItem(dbc.NavLink("Sign in", href="/login", className="fw-semibold"))],
        className="d-flex align-items-center",
        navbar=True,
    )

def study_nav_children(label="None"):
    if label == "None":
        txt = "No Study Selected"
    else:
        txt = f"Study: {label}"
    return dbc.Nav(
        [
            dbc.Badge(txt, pill=True),
        ],
        className="d-flex align-items-center",
        navbar=True,
    )

def header_bar():
    return dbc.Navbar(
        dbc.Container(
            [
                dbc.NavbarBrand("Safer Streets Priority Finder v2.0", className="fw-semibold", href="/"),

                html.Div(id="study-nav", className="ms-auto me-3"),
                html.Div(id="user-nav"),
            ],
            fluid=True,
        ),
        color="light",
        className="mb-3 shadow-sm",
    )

def sidebar():
    nav = dbc.Nav(
        [
            dbc.NavLink([html.I(className="bi bi-house-door-fill me-2"), "Welcome"], href="/", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-book me-2"), "Overview"], href="/overview", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-info-circle me-2"), "Instructions"], href="/instructions", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-folder me-2"), "My Studies"], href="/studies", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-cloud-upload me-2"), "Load Data"], href="/load_data", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-sliders me-2"), "Analysis Settings"], href="/analysis_settings", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-database-fill-gear me-2"), "Run Analysis"], href="/run_analysis", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-bar-chart-line me-2"), "Dashboard"], href="/dashboard", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-map me-2"), "Map Visualization"], href="/map_visualization", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-question-circle me-2"), "FAQ"], href="/faq", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-cone-striped me-2"), "Crash Data Sources"], href="/crash_data_sources", active="exact", className="py-2"),
            dbc.NavLink([html.I(className="bi bi-briefcase me-2"), "Resources and Case Studies"], href="/resources", active="exact", className="py-2"),
        ],
        vertical=True,
        pills=True,
        className="mt-3",
    )
    return html.Div(
        [
            nav,
        ],
        id="sidebar",
        className="p-3 bg-white shadow-sm rounded-3",
        style={"position": "sticky", "top": "1rem"},
    )

def footer_bar():
    icon_links = [
        {
            "href": f"mailto:{CONTACT_EMAIL}?subject=SSPF Developer Message",
            "icon": "bi bi-envelope-fill",
            "label": "Email",
            "target": "_self",
        },
        {
            "href": "https://www.facebook.com/sharer/sharer.php?u=https%3A//sspf.tooledesign.com/",
            "icon": "bi bi-facebook",
            "label": "Facebook",
            "target": "_blank",
        },
        {
            "href": "https://twitter.com/intent/tweet?text=Check%20out%20this%20great%20web%20application!%20The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20https%3A//sspf.tooledesign.com/",
            "icon": "bi bi-twitter-x",
            "label": "X (Twitter)",
            "target": "_blank",
        },
        {
            "href": "https://www.linkedin.com/shareArticle?mini=true&url=https%3A//sspf.tooledesign.com/&title=Safer%20Streets%20Priority%20Finder&summary=The%20Safer%20Streets%20Priority%20Finder%20enables%20you%20to%20analyze%20the%20risk%20to%20vulnerable%20road%20users%20(bicyclists%20and%20pedestrians)%20on%20your%20community%E2%80%99s%20roads.%20&source=https%3A//sspf.tooledesign.com/",
            "icon": "bi bi-linkedin",
            "label": "LinkedIn",
            "target": "_blank",
        },
        {
            "href": "https://github.com/tooledesign/Safer-Streets-Priority-Finder/",
            "icon": "bi bi-github",
            "label": "GitHub",
            "target": "_blank",
        },
    ]
    return html.Footer(
        dbc.Container(
            [
                html.Div(
                    [
                        html.A(
                            html.I(className=lnk["icon"], title=lnk["label"]),
                            href=lnk["href"],
                            target=lnk["target"],
                            rel="noopener noreferrer",
                            className="text-muted me-3 fs-5",
                            **{"aria-label": lnk["label"]},
                        )
                        for lnk in icon_links
                    ],
                    className="mb-1 text-center",
                ),
                html.Div(
                    html.Small("Safer Streets Priority Finder v2.0", className="text-muted"),
                    className="text-center",
                ),
            ],
            fluid=True,
            className="py-3",
        ),
        className="mt-4 border-top",
    )


def make_layout():
    # This function is called when app.layout is set; calling current_user()
    # here is safe because it runs in a request context in normal use.
    return html.Div(
        [
            dcc.Location(id="url"),
            header_bar(),
            dbc.Container(
                dbc.Row(
                    [
                        dbc.Col(sidebar(), md=3, lg=3, xl=3, className="mb-3"),
                        dbc.Col(html.Div(id="page-content"), md=9, lg=9, xl=9),
                    ],
                    className="g-4",
                ),
                fluid=True,
                className="pb-4",
            ),
            footer_bar(),
            # dcc.Store(id="busy", data=False), # tracks busy state
            # busy_modal(),
        ],
        className="bg-body-tertiary",
    )
