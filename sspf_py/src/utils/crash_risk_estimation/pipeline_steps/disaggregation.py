from ..datastore import DataStore
from ..config import FatalModes, BGEstimatesAffix
from dataclasses import replace
import geopandas as gpd
import pandas as pd
from shapely.ops import unary_union

def assign_roadway_to_bg(
    datastore: DataStore,
) -> DataStore:
    """
    Spatial process to assign roadway to block group, and compute effective mileage of roadway in each block group.
    """

    # pull input data from datastore
    buffer_width = datastore.config.roadway_buffer_width_m  # in meters
    roadway_network = datastore.roadway_network.copy()
    estimated_fatal_by_bg_gdf = datastore.estimated_fatal_by_bg.copy()

    # ensure roadway and bg data is in the same study CRS
    # this should just be a fail-safe, dataset should be in the correct CRS when loading into datastore
    study_crs = datastore.config.study_crs
    if roadway_network.crs != study_crs:
        roadway_network = roadway_network.to_crs(study_crs)
    if estimated_fatal_by_bg_gdf.crs != study_crs:
        estimated_fatal_by_bg_gdf = estimated_fatal_by_bg_gdf.to_crs(study_crs)

    # create a buffer around the roadway network
    roadway_network['roadway_length_mi'] = roadway_network.geometry.length / 1609.34

    # NOTE: roadway global id is a unique identifier for each roadway segment, using index as global id
    # since disaggregation will clip roadway segments to block group geometries, and assign crashes to the clipped geometry
    # this is useful for joining back to the segment level crash estimates from different block groups later
    roadway_network['roadway_global_id'] = roadway_network.index
    buffered_roadway = roadway_network.copy()
    buffered_roadway.geometry = buffered_roadway.geometry.buffer(buffer_width, cap_style='flat')

    # Ensure spatial index is actually enabled
    # Accessing sindex builds it if not present
    buffered_roadway.sindex
    estimated_fatal_by_bg_gdf.sindex

    # sjoin buffered roadway with block group data
    joined_roadway_and_bg = gpd.sjoin(
        buffered_roadway,
        estimated_fatal_by_bg_gdf,
        how='inner',
        predicate='intersects',
    )
    # merge bg geometry to joined_roadway_and_bg on geoid
    joined_roadway_and_bg = joined_roadway_and_bg.merge(
        estimated_fatal_by_bg_gdf[['geoid', 'geom']],
        how='left',
        left_on='geoid',
        right_on='geoid',
        suffixes=('', '_bg'),
    )

    # Calculate individual intersections (the "pieces" of the road in each BG)
    # We perform this temporary geometry operation to get exact areas and handle overlaps correctly
    # without doing a global union.
    joined_roadway_and_bg['segment_in_bg_geom'] = joined_roadway_and_bg.geometry.intersection(joined_roadway_and_bg['geom_bg'])
    joined_roadway_and_bg['intersection_area'] = joined_roadway_and_bg['segment_in_bg_geom'].area

    # Calculate the Total Valid Area (Denominator) by dissolving the pieces for each roadway segment.
    # Area(Union(Intersection_i)) is equivalent to Area(Buffer Intersect Union(All BGs)) in the Global approach,
    # but much faster because it groups locally.
    # Create a minimal GDF for the dissolve operation
    dissolve_source = joined_roadway_and_bg[['roadway_global_id', 'segment_in_bg_geom']].set_geometry('segment_in_bg_geom')

    # dissolve() combines the geometries for each road segment into a single shape (handling invalid overlaps)
    dissolved_areas = dissolve_source.dissolve(by='roadway_global_id').area

    # Map the calculated union areas back to the main dataframe
    joined_roadway_and_bg['total_buffer_area'] = joined_roadway_and_bg['roadway_global_id'].map(dissolved_areas)

    joined_roadway_and_bg['effective_mileage'] = joined_roadway_and_bg['roadway_length_mi'] * (
        joined_roadway_and_bg['intersection_area'] / joined_roadway_and_bg['total_buffer_area']
    )

    # add joined data to datastore
    datastore = replace(
        datastore,
        joined_roadway_and_bg=joined_roadway_and_bg,
    )

    # debugging section
    if datastore.config.debug:
        # rename buffer column this is to give the buffered geometry a meaningful name before persisting for debugging
        joined_roadway_and_bg.set_geometry('segment_in_bg_geom', inplace=True)

        # add effective ratio for debugging
        joined_roadway_and_bg['effective_ratio'] = joined_roadway_and_bg['intersection_area'] / joined_roadway_and_bg['total_buffer_area']

        # upload to db
        with datastore.config.db_engine.connect() as conn:
            joined_roadway_and_bg.to_postgis(
                name=datastore.config.roadway_assignment_debug_table,
                con=conn,
                if_exists='replace',
                index=False,
                schema=datastore.config.debug_schema,
            )

    return datastore


def compute_initial_fatal_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Compute initial crash estimates based on roadway and block group data.
    """
    # pull input data from datastore
    joined_roadway_and_bg = datastore.joined_roadway_and_bg.copy()

    # load fatal rate lookup table
    fatal_rate_lookup_table = pd.read_sql(
        f"SELECT * FROM {datastore.config.static_schema}.{datastore.config.fatal_rate_table} WHERE state = %(state)s",
        con=datastore.config.db_engine,
        params={"state": datastore.config.state_name},
    )

    # since we will use lookup table index for join, set it as index
    fatal_rate_lookup_table.set_index("functional_class", inplace=True)

    # ensure fatal rate columns are numeric (guard against TEXT columns in DB)
    for col in fatal_rate_lookup_table.columns:
        if "fatal_rate" in col:
            fatal_rate_lookup_table[col] = pd.to_numeric(fatal_rate_lookup_table[col], errors="coerce")

    # add fatal rates to the joined table, first drop any existing columns that might conflict
    joined_roadway_and_bg = joined_roadway_and_bg.drop(
        columns=[c for c in fatal_rate_lookup_table.columns if c in joined_roadway_and_bg.columns],
        errors="ignore"
    )
    
    joined_roadway_and_bg = joined_roadway_and_bg.merge(
        fatal_rate_lookup_table,
        how="left",
        left_on="functional_class",
        right_index=True,
    )

    for mode in FatalModes:
        # compute initial crash estimates
        joined_roadway_and_bg[f"init_{mode.value}_k"] = (
            joined_roadway_and_bg['effective_mileage'] * joined_roadway_and_bg[f"{mode.value}_fatal_rate"] * 5 # 5-year estimates
        )

    # add joined data to datastore
    datastore = replace(
        datastore,
        joined_roadway_and_bg=joined_roadway_and_bg,
    )
    return datastore


def compute_scaled_fatal_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Compute scaled crash estimates based on initial estimates and block group data.
    """
    # pull input data from datastore
    joined_roadway_and_bg = datastore.joined_roadway_and_bg.copy()
    
    # get the block group estimates
    grouped_by_geoid = joined_roadway_and_bg.groupby('geoid')

    for geoid, group in grouped_by_geoid:
        # compute the sum of initial crash estimates for each mode
        init_crash_sums = {mode.value: group[f"init_{mode.value}_k"].sum() for mode in FatalModes}
        # compute scaling factor for each mode: scaling_factor = bg_estimate / initial_crash_sum
        scaling_factors = {}
        for mode in FatalModes:
            if init_crash_sums[mode.value] == 0:
                scaling_factors[mode.value] = 0
            else:
                # note: both bg_estimate and initial_crash_sum are 5-year estimates, so no need to adjust for time period here 
                scaling_factors[mode.value] = group[f"{BGEstimatesAffix.PRED_PREFIX.value}{mode.value}"].iloc[0] / init_crash_sums[mode.value]

        # apply scaling factors to initial crash estimates
        for mode in FatalModes:
            col_init = f"init_{mode.value}_k"
            col_est  = f"est_{mode.value}_k"
            col_scl_factor  = f"{mode.value}_scaling_factor"
            idx      = group.index
            joined_roadway_and_bg.loc[idx, col_est] = (
                joined_roadway_and_bg.loc[idx, col_init] 
                * scaling_factors[mode.value]
            )
            joined_roadway_and_bg.loc[idx, col_scl_factor] = scaling_factors[mode.value]

    datastore = replace(
        datastore,
        joined_roadway_and_bg=joined_roadway_and_bg,
    )
    return datastore

def aggregate_segment_level_fatal_estimates(
    datastore: DataStore,
) -> DataStore:
    # pull input data from datastore
    joined_roadway_and_bg = datastore.joined_roadway_and_bg.copy()
    roadway_network = datastore.roadway_network.copy()

    study_crs = datastore.config.study_crs

    # define estimation output columns
    est_output_columns = ["roadway_global_id"]
    for mode in FatalModes:
        est_output_columns.append(f"est_{mode.value}_k")

    # groupby roadway_global_id and sum the estimates
    est_output_df = joined_roadway_and_bg[est_output_columns].groupby('roadway_global_id').sum() # 5-year estimates

    # join estimates back to roadway network
    roadway_network = roadway_network.merge(
        est_output_df,
        how='left',
        left_index=True,
        right_on='roadway_global_id', # since roadway_global_id is created from roadway_network index
    )

    # add coefficient of variation for block group estimates 
    cv_output_column = ["roadway_global_id"]
    for mode in FatalModes:
        col_cv = f"{mode.value}_cv"
        cv_output_column.append(col_cv)
        # cv_pred = se_pred/pred 
        joined_roadway_and_bg[col_cv] = (
            joined_roadway_and_bg[f"{BGEstimatesAffix.SE_PREFIX.value}{mode.value}"] /
            joined_roadway_and_bg[f"{BGEstimatesAffix.PRED_PREFIX.value}{mode.value}"]
        )

    # groupby roadway_global_id and compute weighted cv for each segment
    cv_output_df = (
        joined_roadway_and_bg[cv_output_column[1:]]
        .multiply(joined_roadway_and_bg["effective_mileage"], axis=0)
        .groupby(joined_roadway_and_bg["roadway_global_id"]).sum()
        .div(joined_roadway_and_bg.groupby("roadway_global_id")["effective_mileage"].sum(), axis=0)
    )

    # join weighted CV back to roadway network
    roadway_network = roadway_network.merge(
        cv_output_df,
        how='left',
        left_index=True,
        right_on='roadway_global_id', # since roadway_global_id is created from roadway_network index
    )

    fatal_estimates_gdf = gpd.GeoDataFrame(
        roadway_network[[col for col in roadway_network.columns if col != roadway_network.geometry.name]],
        geometry=roadway_network.geometry,
        crs=study_crs,
    )
    fatal_estimates_gdf.rename_geometry("geom", inplace=True)

    datastore = replace(
        datastore,
        fatal_estimates=fatal_estimates_gdf,
        estimated_fatal_by_bg=None,
        joined_roadway_and_bg=None,
    )
    return datastore

def upload_fatal_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Upload crash estimates to the database.
    """
    with datastore.config.db_engine.connect() as conn:
        datastore.fatal_estimates.to_postgis(
            name=datastore.config.fatal_estimates_table,
            con=conn,
            if_exists="replace",
            index=False,
            schema=datastore.config.debug_schema,
            chunksize=5000,
        )
    return replace(datastore, fatal_estimates=None)
