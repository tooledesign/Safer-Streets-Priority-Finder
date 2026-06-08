// Ensure global namespace exists
window.sspf = window.sspf || {};

window.dash_clientside = window.dash_clientside || {};
window.dash_clientside.sspf = window.dash_clientside.sspf || {};

window.dash_clientside.sspf.buildModeLayers = function(selectedMode, geojsonData, modeStyleConfig, legendData) {
  return window.sspf_buildModeLayers(selectedMode, geojsonData, modeStyleConfig, legendData);
};

window.dash_clientside.sspf.mergeMapVisStores = function(crashData, swaData, ssmData, fatalityData) {
  var layers = {};
  var legends = {};

  function merge(data) {
    if (!data || !data.layers) return;
    Object.keys(data.layers).forEach(function(k) { layers[k] = data.layers[k]; });
    Object.keys(data.legends || {}).forEach(function(k) { legends[k] = data.legends[k]; });
  }

  merge(crashData);
  merge(fatalityData);

  if (swaData && swaData.fc) { layers["swa"] = swaData; }
  if (ssmData && ssmData.fc) { layers["model"] = ssmData; }

  // Build roads dropdown from SWA/SSM entries that have actual mode data
  var roadsOptions = [{label: "No Roads", value: "No Roads"}];
  if (swaData && swaData.dropdownEntries) {
    roadsOptions = roadsOptions.concat(swaData.dropdownEntries);
  }
  if (ssmData && ssmData.dropdownEntries) {
    roadsOptions = roadsOptions.concat(ssmData.dropdownEntries);
  }

  var modalOpen = (fatalityData === null || fatalityData === undefined);
  console.log("[map_vis] Step 6: merge stores — modal " + (modalOpen ? "open" : "closed"));
  return [layers, legends, modalOpen, roadsOptions];
};

window.sspf_waitForPaint = function() {
  return new Promise(function(resolve) {
    // Yield two paint opportunities so the spinner is visible before a heavy redraw.
    window.requestAnimationFrame(function() {
      window.requestAnimationFrame(resolve);
    });
  });
};

window.sspf_isHeavyCrashRender = function(selectedCrashMode, crashMetaStore) {
  if (!selectedCrashMode || selectedCrashMode === "No Crashes" || !crashMetaStore) {
    return false;
  }
  var meta = crashMetaStore[selectedCrashMode];
  return !!(meta && meta.is_truncated);
};

// Increase default Canvas renderer click tolerance (pixels) so thin
// polylines are easier to click/tap with preferCanvas enabled.
if (L && L.Canvas && L.Canvas.prototype && L.Canvas.prototype.options) {
  L.Canvas.prototype.options.tolerance = 5;
}

// Build legend Dash component tree from raw legend data [{label, color}, ...].
// Reusable by both analysis page (buildModeLayers) and map-vis dropdown callback.
window.sspf_buildLegendComponent = function(items, title) {
  if (!items || !items.length) return null;
  var children = [{
    namespace: "dash_html_components", type: "Div",
    props: {children: title || "", style: {fontWeight: "bold", marginBottom: "8px"}}
  }];
  items.forEach(function(item) {
    children.push({
      namespace: "dash_html_components", type: "Div",
      props: {
        style: {marginBottom: "6px", display: "flex", alignItems: "center"},
        children: [
          {namespace: "dash_html_components", type: "Span",
           props: {style: {display: "inline-block", width: "16px", height: "16px",
                           backgroundColor: item.color, marginRight: "8px", borderRadius: "2px",
                           border: "1px solid rgba(100, 100, 100, 0.5)"}}},
          {namespace: "dash_html_components", type: "Span",
           props: {children: item.label, style: {fontSize: "12px"}}}
        ]
      }
    });
  });
  return children;
};

// Tag features from a single FC with _featureType and _binStyle for map-vis unified layer.
// Same binning logic as buildModeLayers but outputs a tagged FeatureCollection.
window.sspf_tagRoadFeatures = function(fc, modeCfg) {
  var breaks = modeCfg.breaks;
  var colors = modeCfg.colors;
  var baseStyle = modeCfg.base_style;
  var scoreProp = modeCfg.score_prop;
  var showProp = modeCfg.show_prop;
  var numBins = colors.length;

  var features = [];
  var src = (fc && fc.features) || [];
  for (var i = 0; i < src.length; i++) {
    var f = src[i];
    if (showProp && !f.properties[showProp]) continue;
    var val = f.properties[scoreProp];
    if (val === null || val === undefined || val <= 0) continue;

    var bin = 0;
    for (var j = 1; j < breaks.length; j++) {
      if (val > breaks[j]) bin = j;
      else { bin = j - 1; break; }
    }
    if (bin >= numBins) bin = numBins - 1;

    var style = Object.assign({}, baseStyle, {color: colors[bin]});
    features.push({
      type: "Feature",
      geometry: f.geometry,
      properties: Object.assign({}, f.properties, {
        _featureType: "road",
        _binStyle: style
      })
    });
  }
  // Draw order on the shared canvas = array order, so sort ascending by score:
  // higher-score segments end up last and render on top of lower-score ones.
  // Per-mode equivalent of the bin-layer stacking on the analysis results map.
  features.sort(function(a, b) {
    return (a.properties[scoreProp] || 0) - (b.properties[scoreProp] || 0);
  });
  return {type: "FeatureCollection", features: features};
};

// Clientside mode switch: build per-bin dl.GeoJSON layers from shared FeatureCollection.
// Called with (selectedMode, geojsonData, modeStyleConfig, legendData, analysisType)
window.sspf_buildModeLayers = function(selectedMode, geojsonData, modeStyleConfig, legendData) {
  var empty = [[], [], {display: "none"}];
  if (!selectedMode || !geojsonData || !modeStyleConfig || !legendData) return empty;
  var cfg = modeStyleConfig[selectedMode];
  if (!cfg) return empty;

  var breaks = cfg.breaks;
  var colors = cfg.colors;
  var baseStyle = cfg.base_style;
  var scoreProp = cfg.score_prop;
  var showProp = cfg.show_prop;
  var displayLabel = cfg.display_label;
  var numBins = colors.length;

  var binned = [];
  for (var b = 0; b < numBins; b++) binned.push([]);

  var features = geojsonData.features || [];
  for (var i = 0; i < features.length; i++) {
    var f = features[i];
    if (showProp && !f.properties[showProp]) continue;
    var val = f.properties[scoreProp];
    if (val === null || val === undefined || val <= 0) continue;

    var bin = 0;
    for (var j = 1; j < breaks.length; j++) {
      if (val > breaks[j]) bin = j;
      else { bin = j - 1; break; }
    }
    if (bin >= numBins) bin = numBins - 1;

    binned[bin].push({
      type: "Feature",
      geometry: f.geometry,
      properties: {
        "Road Name": f.properties["Road Name"] || "",
        "Functional Class": f.properties["Functional Class"] || "",
        [displayLabel]: f.properties[displayLabel]
      }
    });
  }

  var layers = [];
  for (var b = 0; b < numBins; b++) {
    if (binned[b].length === 0) continue;
    var style = Object.assign({}, baseStyle, {color: colors[b]});
    layers.push({
      namespace: "dash_leaflet",
      type: "GeoJSON",
      props: {
        data: {type: "FeatureCollection", features: binned[b]},
        options: {style: style, onEachFeature: {variable: "sspf_onEachFeaturePopup"}},
        zoomToBounds: false
      }
    });
  }

  var items = legendData[selectedMode] || [];
  var legendChildren = window.sspf_buildLegendComponent(items, cfg.legend_title) || [];
  var legendStyle = {
    display: "block", position: "absolute", bottom: "12px", right: "12px",
    zIndex: 1000, pointerEvents: "auto", backgroundColor: "white",
    padding: "12px", borderRadius: "4px",
    boxShadow: "0 0 15px rgba(0, 0, 0, 0.2)",
    maxHeight: "300px", overflowY: "auto"
  };

  return [layers, legendChildren, legendStyle];
};

// Build popup HTML for road features (Road Name, Functional Class, score/cost fields).
window.sspf_buildRoadPopupHtml = function(properties) {
  const p = properties || {};
  let html = "";
  if (p["Road Name"]) {
    html += `<b>${p["Road Name"]}</b><br/>`;
  }
  if (p["Functional Class"]) {
    html += `Functional Class: ${p["Functional Class"]}<br/>`;
  }
  Object.keys(p).forEach(k => {
    if (k.includes("Crash Score") || k.includes("Cost")) {
      html += `${k}: ${p[k]}<br/>`;
    }
  });
  return html;
};

window.sspf_onEachFeaturePopup = function(feature, layer){
  var html = window.sspf_buildRoadPopupHtml(feature.properties);
  if (html) layer.bindPopup(html);
};

// Popup + click highlight for roads in map visualization.
// Keeps one selected segment highlighted at a time.
window.sspf_onEachFeaturePopupClickHighlight = function(feature, layer){
  var html = window.sspf_buildRoadPopupHtml(feature.properties);
  if (html) layer.bindPopup(html);

  function resetLayerHighlight(targetLayer) {
    if (
      targetLayer &&
      targetLayer.__sspfOriginalStyle &&
      typeof targetLayer.setStyle === "function"
    ) {
      targetLayer.setStyle(targetLayer.__sspfOriginalStyle);
    }
  }

  layer.on("click", function(e) {
    const clickedLayer = e.target;
    const state = window.sspf;

    // Reset the previously selected segment style.
    if (
      state.clickedRoadLayer &&
      state.clickedRoadLayer !== clickedLayer &&
      state.clickedRoadLayer.__sspfOriginalStyle &&
      typeof state.clickedRoadLayer.setStyle === "function"
    ) {
      state.clickedRoadLayer.setStyle(state.clickedRoadLayer.__sspfOriginalStyle);
    }

    // Capture the original style once, then apply selected style.
    if (!clickedLayer.__sspfOriginalStyle) {
      const o = clickedLayer.options || {};
      clickedLayer.__sspfOriginalStyle = {
        color: o.color,
        weight: o.weight,
        dashArray: o.dashArray,
        opacity: o.opacity,
        fillColor: o.fillColor,
        fillOpacity: o.fillOpacity,
      };
    }

    if (typeof clickedLayer.setStyle === "function") {
      clickedLayer.setStyle({
        color: "#666666",
        weight: 5,
        dashArray: "",
        opacity: 1,
      });
    }

    state.clickedRoadLayer = clickedLayer;
  });

  // If popup is dismissed (close button, map click elsewhere, ESC), clear highlight.
  layer.on("popupclose", function(e) {
    const closedLayer = e.target;
    const state = window.sspf;
    resetLayerHighlight(closedLayer);
    if (state.clickedRoadLayer === closedLayer) {
      state.clickedRoadLayer = null;
    }
  });
};

// Generic popup function that displays all properties from a feature
window.sspf_onEachFeaturePropertiesPopup = function(feature, layer){
  const p = feature.properties || {};
  
  let html = "";
  
  // Iterate through all properties and display them
  Object.keys(p).forEach(key => {
    const value = p[key];
    // Skip null, undefined, empty values, internal keys, and specific keys
    if (value !== null && value !== undefined && value !== "" && key !== "id" && key !== "cluster" && key.charAt(0) !== "_") {
      html += `<b>${key}:</b> ${value}<br/>`;
    }
  });
  
  // Only bind popup if there are properties to show
  if (html) {
    layer.bindPopup(html);
  }
};

// Style function for roads layer — reads per-feature bin style from properties.
window.sspf_roadsStyle = function(feature) {
  return (feature.properties && feature.properties._binStyle) || {};
};

// Style function for fatality block-group layer.
window.sspf_fatalityBgStyle = function(feature) {
  return (feature.properties && feature.properties._binStyle) || {};
};

window.sspf_mapVisContentStyle = function(feature) {
  var p = (feature && feature.properties) || {};
  if (p._featureType === "road") {
    return window.sspf_roadsStyle(feature);
  }
  if (p._featureType === "fatality_bg") {
    return window.sspf_fatalityBgStyle(feature);
  }
  return {};
};

window.sspf_mapVisContentFilter = function(feature) {
  var p = (feature && feature.properties) || {};
  if (p._featureType !== "crash") {
    return true;
  }
  return window.sspf_crashFilter(feature);
};

window.sspf_mapVisContentPointToLayer = function(feature, latlng) {
  var p = (feature && feature.properties) || {};
  if (p._featureType === "crash") {
    return window.sspf_crashPointToLayer(feature, latlng);
  }
  return L.circleMarker(latlng, {
    radius: 0,
    opacity: 0,
    fillOpacity: 0,
    interactive: false,
  });
};

window.sspf_bindFatalityBgHover = function(feature, layer) {
  layer.on("mouseover", function() {
    if (typeof layer.setStyle === "function") {
      layer.setStyle({
        // color: "#00CC44",
        weight: 3,
      });
    }
    if (typeof layer.bringToFront === "function") {
      layer.bringToFront();
    }
  });
  layer.on("mouseout", function() {
    if (typeof layer.setStyle === "function") {
      layer.setStyle(window.sspf_fatalityBgStyle(feature));
    }
  });
};

window.sspf_mapVisContentOnEachFeature = function(feature, layer) {
  var p = (feature && feature.properties) || {};
  if (p._featureType === "road") {
    window.sspf_onEachFeaturePopupClickHighlight(feature, layer);
    return;
  }
  if (p._featureType === "fatality_bg") {
    window.sspf_onEachFeaturePropertiesPopup(feature, layer);
    window.sspf_bindFatalityBgHover(feature, layer);
    return;
  }
  if (p._featureType === "crash") {
    window.sspf_onEachFeatureCrashPopup(feature, layer);
  }
};

// Zoom-based filter for crash GeoJSON.  Priority features (_priority === true)
// are always shown; other features only appear when sspf._showDetail is true.
// The flag is set by a clientside callback on zoom threshold crossing, and
// the layer re-renders because the same callback updates the hideout prop.
window.sspf_crashFilter = function(feature) {
  if (feature.properties && feature.properties._priority) return true;
  return !!window.sspf._showDetail;
};

// Canvas-rendered circle marker for crash points via dl.GeoJSON pointToLayer.
// Reads fillColor from feature.properties (pre-computed server-side from severity).
window.sspf_crashPointToLayer = function(feature, latlng) {
  var p = feature.properties || {};
  var options = {
    radius: 5,
    color: "#FFFFFF",
    weight: 1,
    fillColor: p.fillColor || "#000000",
    fillOpacity: 0.85,
  };
  if (p._pane) {
    options.pane = p._pane;
  }
  return L.circleMarker(latlng, options);
};

// Popup binding for crash GeoJSON features.
window.sspf_onEachFeatureCrashPopup = function(feature, layer) {
  var p = feature.properties || {};
  var html = "";
  if (p.crash_id !== undefined && p.crash_id !== null) {
    html += "<b>Crash ID:</b> " + p.crash_id + "<br/>";
  }
  if (p.crash_year !== undefined && p.crash_year !== null) {
    html += "<b>Year:</b> " + p.crash_year + "<br/>";
  }
  if (p.crash_mode) {
    html += "<b>Mode:</b> " + p.crash_mode + "<br/>";
  }
  if (p.crash_severity) {
    html += "<b>Severity:</b> " + p.crash_severity + "<br/>";
  }
  if (html) {
    layer.bindPopup(html, {pane: "popup_pane"});
  }
};
