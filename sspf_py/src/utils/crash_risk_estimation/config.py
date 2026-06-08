from dataclasses import dataclass, field
from pyproj import CRS
from enum import Enum
from sqlalchemy.engine import Engine
from sqlalchemy import create_engine, MetaData
from functools import cached_property


class FatalModes(Enum):
    """Enum for fatality modes."""
    PED = "ped"
    BIKE = "bike"
    MV = "mv"

class BGEstimatesAffix(Enum):
    """
    Enum for BG estimates affix. 
    Fatal estimate columns are formatted as {pred_prefix}{mode}{suffix},
    """
    PRED_PREFIX = "final_pred_"
    SE_PREFIX = "final_se_"
    SUFFIX = ""

@dataclass(frozen=True)
class Config:

    # debugging
    debug: bool = False  # whether to print/persist debug information
    debug_schema: str = "debug"  # schema for debug tables
    # debug_table_names
    roadway_assignment_debug_table: str = None
    crash_sjoin_debug_table: str = None
    bayesian_update_debug_table: str = None

    # database connection
    db_engine: Engine = None

    # run information
    user_name: int = None  # user ID for the run
    study_name: int = None   # unique run ID for the analysis

    # projection system
    study_crs: CRS = None                   # projection system of local analysis area

    # study area information
    state_name: str = None 
    study_area_table: str = None  # table name for study area polygons

    # disaggregation parameters
    roadway_buffer_width_m: float = 15.0  # in meters, area of influence around road segments

    # database source data parameters
    input_roadway_table: str = None
    input_bg_fatal_table: str = None

    # local crash data from sliding window results
    input_crash_table: str = None  # table name for local crash data
    short_window_pk: str = "short_window_id"  # primary key joining sliding window data to estimated crashes
    mode_col_name: str = "crash_mode"
    mode_mapping: dict = field(default_factory=lambda: {   # map list of prefixes in the sliding window to different modes
        'ped': ['pedestrian'],
        'bike': ['bicycle'],
        'mv': ['motor vehicle'],
        'all': ['pedestrian', 'bicycle', 'motor vehicle'],
    })
    severity_col_name: str = "crash_severity"
    severity_mapping: dict = field(default_factory=lambda: {
        'k': 'K',
        'a': 'A',
        'b': 'B',
        'c': 'C',
        'o': 'O',
    })
    sjoin_dist: float = 50.0  # distance in ft for spatial join to local crash data (this will be converted to meters in analysis)
    local_crash_study_period: int = 5  # number of years to consider for local crash data

    # bayesian updating parameters
    max_est_weight: float = 0.8  # maximum weight for prior estimate
    decay: float = 1.0  # decay parameter for weight calculations

    # crash count distribution (convolution) parameters
    distribute_blur_size: int = 4           # half-window size for centered rolling distribution (0 = disabled)
    distribute_blur_style: str = "linear"   # kernel shape: "linear" or "uniform"
    dissolved_pkey_col: str = "dissolved_pkey"
    segment_sequence_col: str = "segment_sequence"

    # 0.05mi segment and dissolved road tables
    input_segments_0p05mi_table: str = None
    segments_0p05mi_pk: str = "short_0p05mi_window_id"
    start_distance_col: str = "start_distance_m"
    end_distance_col: str = "end_distance_m"
    dissolved_length_col: str = "length_meters"

    # database upload parameters
    output_schema: str = None 
    fatal_estimates_table: str = None
    all_severity_estimates_table: str = None
    output_observed_crash_counts_table: str = None
    bayesian_estimates_table: str = None

    # schema for storing static information (default fatal rate, severity ratio)
    static_schema: str = "static"
    fatal_rate_table: str = "national_fatal_rates"
    severity_ratio_table: str = "severity_ratio_by_state"

    # flag for handling missing non-fatal crashes, e.g., if only fatal crashes are provided
    crash_missing_non_fatal: bool = False
