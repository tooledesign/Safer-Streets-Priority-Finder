from dash import html, dcc
import dash_bootstrap_components as dbc
from ..global_vars import *
from .. import utils

def _analysis_settings_children(study_id=None, use_defaults=False):
    """Return the children list for the analysis-settings-container (no container wrapper).
    Used by callbacks that update the container's children to avoid nested duplicate IDs."""
    analysis_params = utils.analysis.get_analysis_params(study_id)
    return [
        html.H2("Analysis Parameters"),
        html.Hr(),
        html.Ul(
            [
                html.Li(["These parameters control how crashes are analyzed and scored. ",
                         html.B("Adjusting these parameters is optional"), " - the tool uses the default values if no adjustments are made"]),
                html.Li("Each new study starts with default parameters. These default values are chosen based on values that are applicable to most studies."),
                html.Li("Adjusting these parameters allows you to tailor the analysis to your specific study area or policy question. \
                        For example, you may want to increase the weight of K and A crashes if you are particularly concerned about severe crashes, or adjust the sliding window lengths based on the street density in your study area. \
                        If you are unsure about how to set these parameters, we recommend starting with the defaults and then adjusting based on the results you see in the report and dashboard. \
                        You can also refer to the documentation for more guidance on how each parameter affects the analysis."),
                html.Li("Adjust the parameters as needed, then click 'Apply & Save' to use them in your study."),
                html.Li("Click 'Reset to Defaults' to reset all parameters to their default values. Click 'Apply & Save' after resetting to save the default values."),
                html.Li("Proceed to the next page to run the analysis and generate your report. You can return to this page at any time to adjust parameters and re-run the analysis."),
            ]
        ),
        html.Hr(),
        dbc.Card(
            [
                dbc.CardHeader(html.H4("Sliding Windows Analysis Settings")),
                dbc.CardBody(
                    [
                        html.H5("Severity Weights"),
                        html.P("These weights control how much each severity contributes to the crash density score in the sliding windows analysis.\
                                Weights must be integers between 0 and 100.\
                                At least one weight should be greater than zero.\
                                Weights must maintain ordinality (K >= A >= B >= C >= O)."),
                        dbc.Row(
                            [
                                dbc.Col(html.B([dbc.Label("Severity"),]),className="mt-2"),
                                dbc.Col(html.B("Default"), width=2),
                                dbc.Col(html.B("Current"), width=2),
                                dbc.Col(html.B("New Value"), width=2),
                            ]
                        ),
                        *[
                            _num_row(label=s_label, input_id=f"ap-weight-{s}",
                                     value_default=analysis_params["default"]["crash_weights"][s],
                                     value_current=analysis_params["current"]["crash_weights"].get(s, None),
                                     value_new=None, max_=100, use_defaults=use_defaults)
                            for s, s_label in CRASH_SEVERITIES.items() if s != "Omit From Analysis"
                        ],
                        html.Hr(),

                        html.H5("Window Lengths"),
                        html.P("These lengths control how long the sliding and short windows (step length in sliding windows analysis) are (in miles).\
                                Sliding window length must be in increments of 0.05 miles between 0.25 and 2 miles.\
                                Sliding Window to Short Window Length Ratio must be an integer between 1 and 10. \
                                The ratio determines your short window length which is used as the unit of analysis."),
                        dbc.Row(
                            [
                                dbc.Col(html.B([dbc.Label(""),]),className="mt-2"),
                                dbc.Col(html.B("Default"), width=2),
                                dbc.Col(html.B("Current"), width=2),
                                dbc.Col(html.B("New Value"), width=2),
                            ]
                        ),
                        _num_row("Sliding Window Length (miles)", input_id="ap-sliding-window-length",
                                 value_default=analysis_params["default"]["sliding_window_length"],
                                 value_current=analysis_params["current"]["sliding_window_length"],
                                 value_new=None, min_=0.25, max_=2, step=0.05, use_defaults=use_defaults),
                        _num_row("Sliding Window to Short Window Length Ratio", input_id="ap-window-length-ratio",
                                 value_default=analysis_params["default"]["window_length_ratio"],
                                 value_current=analysis_params["current"]["window_length_ratio"],
                                 value_new=None, min_=1, max_=10, step=1, use_defaults=use_defaults),
                    ]
                ),
            ],
            className="mb-3",
        ),
        dbc.Card(
            [
                dbc.CardHeader(html.H4("Crash Costs (for Safer Streets Model)")),
                dbc.CardBody(
                    [
                        html.P("These costs represent the estimated societal cost (in USD) associated with crashes of different severities.\
                               They must maintain ordinality (K >= A >= B >= C >= O)."),
                        dbc.Row(
                            [
                                dbc.Col(html.B([dbc.Label("Severity"),]),className="mt-2"),
                                dbc.Col(html.B("Default"), width=2),
                                dbc.Col(html.B("Current"), width=2),
                                dbc.Col(html.B("New Value"), width=2),
                            ]
                        ),
                        *[
                            _num_row(label=s_label, input_id=f"ap-cost-{s}",
                                     value_default=analysis_params["default"]["crash_costs"][s],
                                     value_current=analysis_params["current"]["crash_costs"].get(s, None),
                                     value_new=None, use_defaults=use_defaults, display_as_currency=True)
                            for s, s_label in CRASH_SEVERITIES.items() if s != "Omit From Analysis"
                        ],
                    ]
                ),
            ],
            className="mb-3",
        ),
        dbc.Card(
            [
                dbc.CardHeader(html.H4("Other Settings")),
                dbc.CardBody(
                    [
                        html.P("Crash join distance controls how close crashes must be to a roadway segment to be considered associated with that segment (in feet).\
                               Distance must be between 25 and 250 feet."),
                        html.P("Number of streets in report controls how many of the highest scoring streets are included in the highest crash corridor map and table. \
                               Value must be an integer between 5 and 20 streets. \
                               This setting does not affect the analysis itself, it is only for the report and dashboard visualization."),
                        dbc.Row(
                            [
                                dbc.Col(html.B([dbc.Label(""),]),className="mt-2"),
                                dbc.Col(html.B("Default"), width=2),
                                dbc.Col(html.B("Current"), width=2),
                                dbc.Col(html.B("New Value"), width=2),
                            ]
                        ),
                        _num_row("Crash Join Distance (feet)", input_id="ap-crash-join-distance",
                                 value_default=analysis_params["default"]["crash_join_distance"],
                                 value_current=analysis_params["current"]["crash_join_distance"],
                                 value_new=None, use_defaults=use_defaults),
                        _num_row("Number of Streets in Report", input_id="ap-num-streets",
                                 value_default=analysis_params["default"]["top_streets_threshold"],
                                 value_current=analysis_params["current"]["top_streets_threshold"],
                                 value_new=None, min_=5, max_=20, use_defaults=use_defaults),
                    ]
                ),
            ],
            className="mb-3",
        ),
        dbc.Button("Reset to Defaults", id="ap-reset", color="primary", className="me-2"),
        dbc.Button("Apply & Save", id="ap-apply", color="primary", className="me-2"),
        dbc.Button("Proceed to Analysis", href="/run_analysis", external_link=False, color="secondary"),

        # local persistence of analysis params
        dcc.Store(id="analysis-params", storage_type="local"),
    ]


def analysis_settings_layout(study_id=None, use_defaults=False):
    return html.Div(
        [
            dbc.Container(
                id="analysis-settings-container",
                children=_analysis_settings_children(study_id=study_id, use_defaults=use_defaults),
                className="py-4",
            ),
            html.Div(id="ap-status", className="mt-2 text-muted"),
        ]
    )


# ---------- small UI helpers (UI-only; no callbacks here) ----------
def _format_currency(value):
    if value is None:
        return ""
    try:
        return f"${int(round(float(value))):,}"
    except Exception:
        return str(value)


def _num_row(label, input_id, value_default, value_current, value_new=None, min_=0, max_=None, step=1, use_defaults=False, display_as_currency=False):
    if value_new is None and use_defaults:
        value_new = value_default
    elif value_new is None:
        value_new = value_default if value_current is None else value_current
    tooltip_text = f"Min: {min_}"
    if max_ is not None:
        tooltip_text += f", Max: {max_}"
    if step != 1:
        tooltip_text += f", Step: {step}"

    default_cell = dbc.Input(
        id=f"{input_id}-default",
        type="text" if display_as_currency else "number",
        value=_format_currency(value_default) if display_as_currency else value_default,
        disabled=True,
    )
    current_cell = dbc.Input(
        id=f"{input_id}-current",
        type="text" if display_as_currency else "number",
        value=_format_currency(value_current) if display_as_currency else value_current,
        disabled=True,
    )

    return dbc.Row(
        [
            dbc.Col(
                html.Span([
                    dbc.Label(label),
                    # html.I(
                    #     className="bi bi-info-circle ms-2",
                    #     id=f"{input_id}-info",
                    #     style={"cursor": "pointer"}
                    # ),
                    # dbc.Tooltip(
                    #     tooltip_text,
                    #     target=f"{input_id}-info",
                    #     placement="right",
                    # ),
                ]),
                className="mt-2"
            ),
            # default value (non-editable)
            dbc.Col(
                default_cell,
                width=2
            ),
            # current value (non-editable)
            dbc.Col(
                current_cell,
                width=2
            ),
            # new value (editable)
            dbc.Col(
                dbc.Input(id=input_id, type="number", value=value_new, min=min_, max=max_, step=step),
                width=2
            ),
        ]
    )

def _dropdown_row(label, dd_id, options, value, top=True):
    return dbc.Row(
        [
            dbc.Col(dbc.Label(label), className=("mt-2" if top else "")),
            dbc.Col(dcc.Dropdown(id=dd_id, options=options, value=value, clearable=False), width=4),
        ]
    )