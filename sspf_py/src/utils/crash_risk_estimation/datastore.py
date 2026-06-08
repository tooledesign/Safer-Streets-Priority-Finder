from dataclasses import dataclass, field
import geopandas as gpd
from .config import Config
from sqlalchemy import Table, Column

@dataclass(frozen=True)
class DataStore:
    # using geodatafram type for python pipeline
    # sqlalchmey table type for sql pipeline
    roadway_network:            gpd.GeoDataFrame | Table = None
    estimated_fatal_by_bg:      gpd.GeoDataFrame | Table = None

    joined_roadway_and_bg:      gpd.GeoDataFrame | Table = None

    fatal_estimates:            gpd.GeoDataFrame | Table = None
    all_severity_estimates:     gpd.GeoDataFrame | Table = None

    local_crash_counts:         gpd.GeoDataFrame | Table = None
    local_crash_data:          gpd.GeoDataFrame | Table = None
    bayesian_crash_estimates:   gpd.GeoDataFrame | Table = None

    segments_0p05mi:            gpd.GeoDataFrame | Table = None

    config:                     Config = field(default_factory=Config)
