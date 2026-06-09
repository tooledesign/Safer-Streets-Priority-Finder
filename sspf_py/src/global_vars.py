from datetime import datetime
from dataclasses import dataclass, asdict

ts_print = lambda message: print(f"[{datetime.now().strftime('%Y-%m-%d %H:%M:%S')}] {message}")

##########################################################################################################
# ANALYSIS PARAMETERS
##########################################################################################################
CRASH_MODES = {
    "bike": "Bicycle", 
    "ped": "Pedestrian", 
    "mv": "Motor Vehicle", 
    # "other": "Other",
    "Omit From Analysis": "Omit From Analysis"
}

# need to check and confirm with project team if these mappings are okay
FARS_CRASH_MODE_MAP = {
    "Bicyclist": "bike",
    "Motorist": "mv",
    "Solo-Motorist": "mv",
    "Pedestrian": "ped",
    "Personal Conveyance": "Omit From Analysis",
    "Other": "Omit From Analysis",
}

CRASH_SEVERITIES = {
    "K": "Fatality (K)", 
    "A": "Serious Injury (A)", 
    "B": "Non Serious Injury (B)", 
    "C": "Possible Injury (C)", 
    "O": "No Injury (O)",
    "Omit From Analysis": "Omit From Analysis"
}

FUNCTIONAL_CLASSES=[
    "Expressway", 
    "Major Arterial", 
    "Minor Arterial", 
    "Major Collector", 
    "Minor Collector", 
    "Local Road", 
    "Omit From Analysis"
]
OSM_FCLASS_MAP={
    "motorway": "Expressway",
    "motorway_link": "Expressway",
    "trunk": "Major Arterial",
    "trunk_link": "Major Arterial",
    "primary": "Major Arterial",
    "primary_link": "Major Arterial",
    "secondary": "Minor Arterial",
    "secondary_link": "Minor Arterial",
    "tertiary": "Major Collector",
    "tertiary_link": "Major Collector",
    "residential": "Local Road",
    "living_street": "Local Road",
    "unclassified": "Local Road",
    "road": "Local Road",
    "service": "Omit From Analysis",
}

CRASH_WEIGHTS = {"K": 3, "A": 3, "B": 1, "C": 0, "O": 0}
WINDOW_LENGTHS = {"Sliding": 0.5}
WINDOW_LENGTH_RATIO = 5  # Sliding Window Length / Short Window Length
CRASH_JOIN_DIST = 50  # to join crashes to roads (feet)

# the user can't change this, so leaving this in meters instead of feet
ROAD_BUFFER_DIST = 15 # to join roads to block groups (meters)

# bayesian params
BAYESIAN_DECAY = 1
BAYESIAN_MAX_PRIOR_WEIGHT = 0.8

CRASH_COSTS = {
    #FHWA defaults from https://highways.dot.gov/sites/fhwa.dot.gov/files/2025-10/CrashCostFactSheet_508_OCT2025.pdf
    "K": 15988000, 
    "A": 1705100, 
    "B": 384000, 
    "C": 204600, 
    "O": 18100
}

MIN_COST_THRESHOLD = CRASH_COSTS["O"]  # minimum total crash cost to show street in results map

NUM_STREETS = 10 # how many default top crash score streets to show (e.g top 10 streets)

##########################################################################################################
# DASHBOARD
##########################################################################################################
FCLASS_COLORS = {
    "Expressway": "#E41A1C",
    "Major Arterial": "#377EB8",
    "Minor Arterial": "#2ca02c",
    "Major Collector": "#EB6E00",
    "Minor Collector": "#984EA3",
    # "Local Road": "#FFFF33",
    "Local Road": "#B08D03"
}

SEVERITY_COLORS = {
    "K": "#d62728",
    "A": "#EB6E00",
    "B": "#1f77b4",
    "C": "#2ca02c",
    "O": "#7f7f7f",
}

MODE_COLORS = {
    "ped": "#1f77b4",
    "bike": "#2ca02c",
    "mv": "#EB6E00"
}

OBS_VS_EST_COLORS = {
    "Observed Crashes": "#1f77b4",
    "Modeled Adjusted Crashes": "#EB6E00",
}

SEVERITY_ORDER = ["K", "A", "B", "C", "O"]
SEVERITY_LABELS = {k: CRASH_SEVERITIES[k] for k in SEVERITY_ORDER}

MODE_ORDER = ["ped", "bike", "mv"]
MODE_LABELS = {k: CRASH_MODES[k] for k in MODE_ORDER}

FUNCLASS_ORDER = [f for f in FUNCTIONAL_CLASSES if f != "Omit From Analysis"]
MAX_MAP_CRASH_MARKERS = 10000       # priority crash count shown at all zoom levels (per mode)
CRASH_DETAIL_MIN_ZOOM = 14          # zoom level at which remaining crashes become visible
LARGE_CRASH_DATASET_THRESHOLD = 50000  # total crash count above which map performance warnings are shown

##########################################################################################################
# MAPPING CONFIGURATIONS
##########################################################################################################

@dataclass
class FeatureStyle:
    weight: int = 2
    color: str = "#000000"
    dashArray: str = ""
    opacity: float = 1.0

    def to_dict(self):
        return asdict(self)

@dataclass
class GraduatedFeatureStyle(FeatureStyle):
    colorscale: str = ""
    max_bins: int = 5
    weight: float = 2
    na_color: str = "#ffffff00"

@dataclass
class BoundaryStyle(FeatureStyle):
    weight: int = 2
    color: str = "#B88905"
    fillColor: str = "#a6bddb"
    fillOpacity: float = 0.35

@dataclass
class BasemapConfig:
    name: str
    tile_url: str
    attribution: str
    default: bool = False

@dataclass
class LayerConfig:
    layer_name: str
    feature_style: FeatureStyle
    hover_style: FeatureStyle = None


@dataclass
class GraduatedLayerConfig:
    layer_name: str
    feature_style: GraduatedFeatureStyle
    hover_style: FeatureStyle = None

@dataclass
class MappingConfig:
    basemaps: list[BasemapConfig]
    layers: dict[str, LayerConfig|GraduatedLayerConfig]
    legend: bool


MODE_COLORMAPS = {
    "ped": "Oranges",
    "bike": "YlGnBu",
    "mv": "RdPu",
}

osm_basemap = BasemapConfig(
    name="OSM",
    tile_url="https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
    attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors",
)

dark_matter_basemap = BasemapConfig(
    name="Dark Matter",
    tile_url="https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png",
    attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/attributions'>CARTO</a>"
)
positron_basemap = BasemapConfig(
    name="Positron",
    tile_url="https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png",
    attribution="&copy; <a href='https://www.openstreetmap.org/copyright'>OpenStreetMap</a> contributors &copy; <a href='https://carto.com/attributions'>CARTO</a>",
    default=True,
)

TOP_CORRIDOR_CONFIG = MappingConfig(
    basemaps=[osm_basemap, positron_basemap, dark_matter_basemap],
    layers={
        "top_corridors": LayerConfig(
            layer_name="Top Crash Score Corridors",
            feature_style=FeatureStyle(weight=2, color="#D32F2F"),
            hover_style=FeatureStyle(weight=5, color="#666666", dashArray=""),
        ),
        "study_area": LayerConfig(
            layer_name="Study Area Boundary",
            feature_style=BoundaryStyle(),
        ),
    },
    legend=False,
)

SWA_MAP_CONFIG = MappingConfig(
    basemaps=[osm_basemap, positron_basemap, dark_matter_basemap],
    layers={
        "ped": GraduatedLayerConfig(
            layer_name="Sliding Window Crash Scores (Pedestrian)",
            feature_style=GraduatedFeatureStyle(
                weight=2, max_bins=5, colorscale="Oranges"
            ),
            hover_style=FeatureStyle(weight=5, color="#666666", dashArray=""),
        ),
        "bike": GraduatedLayerConfig(
            layer_name="Sliding Window Crash Scores (Bicycle)",
            feature_style=GraduatedFeatureStyle(
                weight=2, max_bins=5, colorscale="YlGnBu"
            ),
            hover_style=FeatureStyle(weight=5, color="#666666", dashArray=""),
        ),
        "mv": GraduatedLayerConfig(
            layer_name="Sliding Window Crash Scores (Motor Vehicle)",
            feature_style=GraduatedFeatureStyle(
                weight=2, max_bins=5, colorscale="RdPu"
            ),
            hover_style=FeatureStyle(weight=5, color="#666666", dashArray=""),
        ),
        "study_area": LayerConfig(
            layer_name="Study Area Boundary",
            feature_style=BoundaryStyle(),
        ),
    },
    legend=True,
)

##########################################################################################################
# OTHER GLOBAL CONSTANTS
##########################################################################################################
CONTACT_EMAIL = "saferstreetspriorityfinder@tooledesign.com"

GIT_REPO_URL = "https://github.com/tooledesign/Safer-Streets-Priority-Finder/sspf_py"
CANONICAL_URL = "https://sspf.tooledesign.com"