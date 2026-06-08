from dash import Input, Output, State, no_update
import dash
import traceback
import dash_leaflet as dl
from flask import session
from ..utils import preset_modals
from .. import utils
from ..global_vars import *


def _tag_feature(feature, feature_type, extra_properties=None):
    tagged = dict(feature)
    tagged["properties"] = {
        **feature.get("properties", {}),
        "_featureType": feature_type,
        **(extra_properties or {}),
    }
    return tagged


def _merge_dl_layers(dl_layers, feature_type):
    """Flatten a list of dl.GeoJSON components into a single tagged FeatureCollection."""
    merged = []
    for layer in dl_layers:
        style = (layer.options or {}).get("style", {})
        for f in layer.data.get("features", []):
            merged.append(_tag_feature(f, feature_type, extra_properties={"_binStyle": style}))
    return {"type": "FeatureCollection", "features": merged}


def register_map_vis_callbacks(app):

    # ---- Step 1: init (lightweight — no geometry) ----------------------------
    @app.callback(
        Output("map-vis-layers-control-div", "children"),
        Output("map-vis-init-store", "data"),
        Output("map-vis-page-error", "children"),
        Input("url", "pathname"),
        prevent_initial_call=False,
    )
    def init_map_vis(url_pathname):
        if url_pathname != "/map_visualization":
            raise dash.exceptions.PreventUpdate

        print("[map_vis] Step 1: init — building study area layer")
        try:
            study_id = session["active_study_id"]
            un, sn = utils.db.get_user_and_study_names(study_id=study_id)
            study_table_names = utils.db.get_study_table_names(username=un, study_name=sn)

            has_swa = utils.db.table_exists(table_name=study_table_names["output_swa_short_windows"])
            has_ssm = utils.db.table_exists(table_name=study_table_names["output_model_results"])

            study_layer = utils.map_vis_func.get_study_area_dl_layer(username=un, study_name=sn)
            layer_control_children = [
                dl.BaseLayer(dl.TileLayer(), name="OpenStreetMap", checked=False),
                dl.BaseLayer(
                    dl.TileLayer(
                        url="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
                        attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/'>CARTO</a>",
                    ),
                    name="Positron",
                    checked=True,
                ),
                dl.BaseLayer(
                    dl.TileLayer(
                        url="https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
                        attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/'>CARTO</a>",
                    ),
                    name="Dark Matter",
                ),
                dl.Overlay(dl.LayerGroup(children=[study_layer]), name="Study Area", checked=True),
            ]
            layer_control_div = [dl.LayersControl(collapsed=True, position="topleft", children=layer_control_children)]

            init_data = {"un": un, "sn": sn, "has_swa": has_swa, "has_ssm": has_ssm}
            return layer_control_div, init_data, no_update

        except Exception as e:
            traceback.print_exc()
            return (
                [],
                None,
                preset_modals.info_modal(title="Error Loading Map Visualization", message=f"{type(e).__name__}: {e}"),
            )

    # ---- Step 2: load crashes ------------------------------------------------
    @app.callback(
        Output("map-vis-crash-data", "data"),
        Output("map-vis-crash-done", "data"),
        Output("map-vis-crash-meta-store", "data"),
        Output("map-vis-crash-dataset-dropdown", "options"),
        Input("map-vis-init-store", "data"),
        prevent_initial_call=True,
    )
    def load_crashes(init_data):
        if not init_data:
            raise dash.exceptions.PreventUpdate
        print("[map_vis] Step 2: loading crashes")
        un, sn = init_data["un"], init_data["sn"]
        crash_dropdowns = [{"label": "No Crashes", "value": "No Crashes"}]
        try:
            crash_layers, _, crash_meta = utils.map_vis_func.get_crash_dl_layer(username=un, study_name=sn)
            layers = {}
            legends = {}
            for mode in crash_layers:
                crash_dropdowns.append({"label": f"{MODE_LABELS[mode]} Crashes", "value": mode})
                layers[f"crashes_{mode}"] = {
                    "type": "FeatureCollection",
                    "features": [_tag_feature(f, "crash") for f in crash_layers[mode].get("features", [])],
                }
                legends[f"crashes_{mode}"] = utils.map_vis_func.build_legend_display(
                    title=f"{MODE_LABELS[mode]} Crash Severity",
                    legend_data=[{"label": sev, "color": color} for sev, color in SEVERITY_COLORS.items()],
                )
            return {"layers": layers, "legends": legends}, True, crash_meta, crash_dropdowns
        except Exception as e:
            traceback.print_exc()
            return {"layers": {}, "legends": {}}, True, {}, crash_dropdowns

    # ---- Step 3: load SWA results --------------------------------------------
    # Input is crash-done (boolean), NOT crash-data (avoids large POST body)
    @app.callback(
        Output("map-vis-swa-data", "data"),
        Output("map-vis-swa-done", "data"),
        Input("map-vis-crash-done", "data"),
        State("map-vis-init-store", "data"),
        prevent_initial_call=True,
    )
    def load_swa(_crash_done, init_data):
        if not init_data or not init_data.get("has_swa"):
            print("[map_vis] Step 3: skipping SWA (no results table)")
            return {}, True
        print("[map_vis] Step 3: loading SWA results")
        try:
            study_id = session["active_study_id"]
            _, geojson_data, mode_style_config, legend_data = utils.analysis.analysis_results_layers(
                study_id=study_id, analysis_type="sliding-windows",
            )
            dropdown_entries = [
                {"label": f"{MODE_LABELS[m]} Sliding Windows Analysis", "value": f"swa_{m}"}
                for m in mode_style_config
            ]
            return {"fc": geojson_data, "modeConfig": mode_style_config, "legendData": legend_data,
                    "dropdownEntries": dropdown_entries}, True
        except Exception as e:
            traceback.print_exc()
            return {}, True

    # ---- Step 4: load SSM results --------------------------------------------
    @app.callback(
        Output("map-vis-ssm-data", "data"),
        Output("map-vis-ssm-done", "data"),
        Input("map-vis-swa-done", "data"),
        State("map-vis-init-store", "data"),
        prevent_initial_call=True,
    )
    def load_ssm(_swa_done, init_data):
        if not init_data or not init_data.get("has_ssm"):
            print("[map_vis] Step 4: skipping SSM (no results table)")
            return {}, True
        print("[map_vis] Step 4: loading SSM results")
        try:
            study_id = session["active_study_id"]
            _, geojson_data, mode_style_config, legend_data = utils.analysis.analysis_results_layers(
                study_id=study_id, analysis_type="safer-streets-model",
            )
            dropdown_entries = [
                {"label": f"{MODE_LABELS[m]} Safer Streets Model", "value": f"model_{m}"}
                for m in mode_style_config
            ]
            return {"fc": geojson_data, "modeConfig": mode_style_config, "legendData": legend_data,
                    "dropdownEntries": dropdown_entries}, True
        except Exception as e:
            traceback.print_exc()
            return {}, True

    # ---- Step 5: load fatality block groups -----------------------------------
    @app.callback(
        Output("map-vis-fatality-data", "data"),
        Input("map-vis-ssm-done", "data"),
        State("map-vis-init-store", "data"),
        prevent_initial_call=True,
    )
    def load_fatality(_ssm_done, init_data):
        if not init_data:
            raise dash.exceptions.PreventUpdate
        print("[map_vis] Step 5: loading fatality block groups")
        un, sn = init_data["un"], init_data["sn"]
        try:
            fatality_layers, fatality_legend = utils.map_vis_func.fatality_bg_dl_layer(username=un, study_name=sn)
            layers = {}
            legends = {}
            for m, ml in MODE_LABELS.items():
                if m in fatality_layers:
                    layers[f"fatality_bg_{m}"] = _merge_dl_layers(fatality_layers[m], "fatality_bg")
                    legends[f"fatality_bg_{m}"] = utils.map_vis_func.build_legend_display(
                        title=f"Estimated {ml} Fatal Crashes",
                        legend_data=fatality_legend.get(m, []),
                    )
            return {"layers": layers, "legends": legends}
        except Exception as e:
            traceback.print_exc()
            return {"layers": {}, "legends": {}}

    # ---- Step 6: clientside merge — combine per-type stores, close modal -----
    from dash import ClientsideFunction
    app.clientside_callback(
        ClientsideFunction(namespace="sspf", function_name="mergeMapVisStores"),
        Output("map-vis-available-layers-store", "data"),
        Output("map-vis-rendered-legends-store", "data"),
        Output("map-vis-page-load-busy", "is_open"),
        Output("map-vis-roads-dataset-dropdown", "options"),
        Input("map-vis-crash-data", "data"),
        Input("map-vis-swa-data", "data"),
        Input("map-vis-ssm-data", "data"),
        Input("map-vis-fatality-data", "data"),
        prevent_initial_call=True,
    )
                
    
    # Open the map render modal before expensive client-side redraws so the spinner
    # can paint before Leaflet rebuilds a large crash layer.
    app.clientside_callback(
        """
        function(selectedCrashMode, selectedRoadsDataset, selectedFatalityMode,
                 hideout, crashMetaStore) {
            if (!window.sspf_isHeavyCrashRender(selectedCrashMode, crashMetaStore)) {
                return window.dash_clientside.no_update;
            }
            return true;
        }
        """,
        Output("map-vis-update-busy", "is_open", allow_duplicate=True),
        Input("map-vis-crash-dataset-dropdown", "value"),
        Input("map-vis-roads-dataset-dropdown", "value"),
        Input("map-vis-fatality-model-dropdown", "value"),
        State("map-vis-content-geojson", "hideout"),
        State("map-vis-crash-meta-store", "data"),
        prevent_initial_call=True,
    )

    # --- Clientside callback to swap layers on dropdown change ----------------
    # All GeoJSON data + pre-rendered legends live in dcc.Store (browser memory).
    # This callback picks the right key — no server round-trip, no payload transfer.
    app.clientside_callback(
        """
        async function(selectedCrashMode, selectedRoadsDataset, selectedFatalityMode,
                       layersStore, renderedLegends, hideout, crashMetaStore) {
            var emptyFC = {type: "FeatureCollection", features: []};
            var noUpdate = window.dash_clientside.no_update;

            if (!layersStore || !renderedLegends) {
                return [noUpdate,
                        noUpdate, noUpdate,
                        noUpdate, noUpdate,
                        noUpdate, noUpdate,
                        noUpdate];
            }

            var showDetail = !!(hideout && hideout.showDetail);
            if (window.sspf_isHeavyCrashRender(selectedCrashMode, crashMetaStore)) {
                await window.sspf_waitForPaint();
            }
            window.sspf._showDetail = showDetail;
            var base = {
                position: "absolute", zIndex: 1000, pointerEvents: "auto",
                backgroundColor: "white", padding: "12px", borderRadius: "4px",
                boxShadow: "0 0 15px rgba(0, 0, 0, 0.2)",
                maxHeight: "300px", overflowY: "auto"
            };
            var hidden = Object.assign({}, base, {display: "none"});
            var combinedFeatures = [];

            window.sspf.clickedRoadLayer = null;

            // Crashes
            var crashData = emptyFC;
            var crashLegend = null;
            var crashStyle = hidden;
            if (selectedCrashMode && selectedCrashMode !== "No Crashes") {
                var ck = "crashes_" + selectedCrashMode;
                if (layersStore[ck]) crashData = layersStore[ck];
                if (renderedLegends[ck]) crashLegend = renderedLegends[ck];
                crashStyle = Object.assign({}, base, {bottom: "12px", right: "12px"});
            }

            // Roads — single FC + mode config stored under "swa" / "model" key;
            // clientside bins and tags features on the fly
            var roadsData = emptyFC;
            var roadsLegend = null;
            var roadsStyle = hidden;
            if (selectedRoadsDataset && selectedRoadsDataset !== "No Roads") {
                var idx = selectedRoadsDataset.indexOf("_");
                var prefix = selectedRoadsDataset.substring(0, idx);
                var mode = selectedRoadsDataset.substring(idx + 1);
                var entry = layersStore[prefix];
                if (entry && entry.fc && entry.modeConfig && entry.modeConfig[mode]) {
                    roadsData = window.sspf_tagRoadFeatures(entry.fc, entry.modeConfig[mode]);
                    var legendItems = entry.legendData && entry.legendData[mode];
                    roadsLegend = window.sspf_buildLegendComponent(
                        legendItems, entry.modeConfig[mode].legend_title
                    );
                }
                roadsStyle = Object.assign({}, base, {bottom: "12px", left: "12px"});
            }

            // Fatality BGs
            var fatData = emptyFC;
            var fatLegend = null;
            var fatStyle = hidden;
            if (selectedFatalityMode && selectedFatalityMode !== "No Fatality Model") {
                var fk = "fatality_bg_" + selectedFatalityMode;
                if (layersStore[fk]) fatData = layersStore[fk];
                if (renderedLegends[fk]) fatLegend = renderedLegends[fk];
                fatStyle = Object.assign({}, base, {top: "12px", right: "12px"});
            }

            if (fatData.features) {
                combinedFeatures = combinedFeatures.concat(fatData.features);
            }
            if (roadsData.features) {
                combinedFeatures = combinedFeatures.concat(roadsData.features);
            }
            if (crashData.features) {
                combinedFeatures = combinedFeatures.concat(crashData.features);
            }

            return [{type: "FeatureCollection", features: combinedFeatures},
                    crashLegend, crashStyle,
                    roadsLegend, roadsStyle,
                    fatLegend, fatStyle,
                    noUpdate];
        }
        """,
        # combined dynamic content
        Output("map-vis-content-geojson", "data"),
        # crashes legend
        Output("map-vis-legend-crashes", "children"),
        Output("map-vis-legend-crashes", "style"),
        # roads legend
        Output("map-vis-legend-roads", "children"),
        Output("map-vis-legend-roads", "style"),
        # fatality bg legend
        Output("map-vis-legend-fatality", "children"),
        Output("map-vis-legend-fatality", "style"),
        # error (always no_update from clientside)
        Output("map-vis-page-error", "children", allow_duplicate=True),

        # Inputs
        Input("map-vis-crash-dataset-dropdown", "value"),
        Input("map-vis-roads-dataset-dropdown", "value"),
        Input("map-vis-fatality-model-dropdown", "value"),
        # State — data stays in browser, only read client-side
        State("map-vis-available-layers-store", "data"),
        State("map-vis-rendered-legends-store", "data"),
        State("map-vis-content-geojson", "hideout"),
        State("map-vis-crash-meta-store", "data"),
        prevent_initial_call=True,
    )

    app.clientside_callback(
        """
        function(selectedCrashMode, hideout, crashMetaStore) {
            if (!selectedCrashMode || selectedCrashMode === "No Crashes" || !crashMetaStore) {
                return ["", false];
            }

            var meta = crashMetaStore[selectedCrashMode];
            if (!meta || !meta.is_truncated) {
                return ["", false];
            }

            var showDetail = !!(hideout && hideout.showDetail);
            if (showDetail) {
                return ["", false];
            }

            return [
                "Showing " + meta.low_zoom_visible.toLocaleString() + " of "
                + meta.total.toLocaleString() + " crashes. Zoom in to see all crashes.",
                true
            ];
        }
        """,
        Output("map-vis-crash-truncation-notice", "children"),
        Output("map-vis-crash-truncation-notice", "is_open"),
        Input("map-vis-crash-dataset-dropdown", "value"),
        Input("map-vis-content-geojson", "hideout"),
        Input("map-vis-crash-meta-store", "data"),
    )

    app.clientside_callback(
        """
        function(selectedCrashMode, crashMetaStore) {
            if (!selectedCrashMode || selectedCrashMode === "No Crashes" || !crashMetaStore) {
                return ["", false];
            }
            var meta = crashMetaStore[selectedCrashMode];
            if (!meta) {
                return ["", false];
            }
            var threshold = __LARGE_CRASH_DATASET_THRESHOLD__;
            if (meta.total <= threshold) {
                return ["", false];
            }
            return [
                "This crash mode contains " + meta.total.toLocaleString() + " crashes — map rendering may be slow. "
                + "If you are experiencing issues, visit instruction tab for tips on working with large dataset. ",
                true
            ];
        }
        """.replace("__LARGE_CRASH_DATASET_THRESHOLD__", str(LARGE_CRASH_DATASET_THRESHOLD)),
        Output("map-vis-large-dataset-notice", "children"),
        Output("map-vis-large-dataset-notice", "is_open"),
        Input("map-vis-crash-dataset-dropdown", "value"),
        Input("map-vis-crash-meta-store", "data"),
    )

    app.clientside_callback(
        """
        function(zoom, selectedCrashMode, hideout, crashMetaStore) {
            var threshold = __CRASH_DETAIL_MIN_ZOOM__;
            var showDetail = (zoom != null && zoom >= threshold);
            var currentShowDetail = !!(hideout && hideout.showDetail);
            if (showDetail === currentShowDetail) {
                return window.dash_clientside.no_update;
            }

            if (!window.sspf_isHeavyCrashRender(selectedCrashMode, crashMetaStore)) {
                return window.dash_clientside.no_update;
            }

            return true;
        }
        """.replace("__CRASH_DETAIL_MIN_ZOOM__", str(CRASH_DETAIL_MIN_ZOOM)),
        Output("map-vis-update-busy", "is_open", allow_duplicate=True),
        Input("map-vis-leaflet-map", "zoom"),
        State("map-vis-crash-dataset-dropdown", "value"),
        State("map-vis-content-geojson", "hideout"),
        State("map-vis-crash-meta-store", "data"),
        prevent_initial_call=True,
    )

    app.clientside_callback(
        """
        async function(zoom, selectedCrashMode, hideout, crashMetaStore) {
            var threshold = __CRASH_DETAIL_MIN_ZOOM__;
            var showDetail = (zoom != null && zoom >= threshold);
            var currentShowDetail = !!(hideout && hideout.showDetail);
            if (showDetail === currentShowDetail) {
                return window.dash_clientside.no_update;
            }

            if (window.sspf_isHeavyCrashRender(selectedCrashMode, crashMetaStore)) {
                await window.sspf_waitForPaint();
            }

            window.sspf._showDetail = showDetail;
            return {showDetail: showDetail};
        }
        """.replace("__CRASH_DETAIL_MIN_ZOOM__", str(CRASH_DETAIL_MIN_ZOOM)),
        Output("map-vis-content-geojson", "hideout"),
        Input("map-vis-leaflet-map", "zoom"),
        State("map-vis-crash-dataset-dropdown", "value"),
        State("map-vis-content-geojson", "hideout"),
        State("map-vis-crash-meta-store", "data"),
        prevent_initial_call=True,
    )

    app.clientside_callback(
        """
        async function(data, hideout, isOpen) {
            if (!isOpen) {
                return window.dash_clientside.no_update;
            }

            await new Promise(function(resolve) {
                window.setTimeout(resolve, __MAP_VIS_RENDER_SPINNER_MS__);
            });
            return false;
        }
        """.replace("__MAP_VIS_RENDER_SPINNER_MS__", "150"),
        Output("map-vis-update-busy", "is_open", allow_duplicate=True),
        Input("map-vis-content-geojson", "data"),
        Input("map-vis-content-geojson", "hideout"),
        State("map-vis-update-busy", "is_open"),
        prevent_initial_call=True,
    )
