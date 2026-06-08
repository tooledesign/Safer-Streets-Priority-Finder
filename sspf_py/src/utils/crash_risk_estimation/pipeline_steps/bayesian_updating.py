from ..datastore import DataStore
from ..config import FatalModes
from dataclasses import replace
from itertools import chain
import duckdb
import geopandas as gpd
import numpy as np
import pandas as pd


def _build_kernel(blur_size: int, blur_style: str) -> np.ndarray:
    """
    Build raw (un-normalized) symmetric kernel of length 2*blur_size+1.
    """
    if blur_size == 0:
        return np.array([1.0])
    offsets = np.abs(np.arange(-blur_size, blur_size + 1))
    if blur_style == "linear":
        return (blur_size - offsets + 1) / (blur_size + 1)
    elif blur_style == "uniform":
        return np.ones(2 * blur_size + 1)
    else:
        raise ValueError(f"Unknown blur_style: {blur_style!r}")


def _distribute_crash_counts(
    df: pd.DataFrame,
    value_columns: list,
    dissolved_pkey_col: str,
    segment_sequence_col: str,
    blur_size: int,
    blur_style: str,
) -> pd.DataFrame:
    """
    Scatter crash counts to nearby segments along each dissolved road.

    For each source segment, its value is distributed to neighbors using a
    centered kernel. At road edges the kernel is truncated and weights are
    renormalized so each source's full value is still distributed, preserving
    the total count per dissolved road.
    """
    if blur_size == 0:
        return df

    raw_kernel = _build_kernel(blur_size, blur_style)
    k = len(raw_kernel)
    half_k = k // 2

    # Sort once globally, then identify group boundaries
    df_sorted = df.sort_values([dissolved_pkey_col, segment_sequence_col])
    group_ids = df_sorted[dissolved_pkey_col].values
    breaks = np.flatnonzero(group_ids[1:] != group_ids[:-1]) + 1

    # Build padded array: insert blur_size zeros at each group break
    n_total = len(df_sorted)
    n_pads = len(breaks)
    padded_len = n_total + n_pads * blur_size

    # Map each original position to its padded position (vectorized)
    group_num = np.zeros(n_total, dtype=int)
    group_num[breaks] = 1
    group_num = np.cumsum(group_num)
    padded_pos = np.arange(n_total) + group_num * blur_size

    # Indicator: 1 at real positions, 0 at padding
    indicator = np.zeros(padded_len)
    indicator[padded_pos] = 1.0

    # Norm: kernel weight sum at each position (handles group edges + inter-group padding)
    norm_full = np.convolve(indicator, raw_kernel, mode='full')[half_k : half_k + padded_len]
    norm_full = np.where(indicator > 0, norm_full, 1.0)  # avoid div-by-zero at padding

    result = df.copy()
    sorted_idx = df_sorted.index

    for col in value_columns:
        padded_vals = np.zeros(padded_len)
        padded_vals[padded_pos] = df_sorted[col].values / norm_full[padded_pos]
        distributed = np.convolve(padded_vals, raw_kernel, mode='full')[half_k : half_k + padded_len]
        result.loc[sorted_idx, col] = distributed[padded_pos]

    return result

def _filter_nearest_per_road(
    joined: gpd.GeoDataFrame,
    local_crash_data: gpd.GeoDataFrame,
    dissolved_pkey_col: str,
) -> gpd.GeoDataFrame:
    """
    For each (crash, dissolved_road) pair in the sjoin result, keep only
    the nearest 0.05mi segment.

    'joined' is the result of gpd.sjoin(segments_0p05mi, local_crash_data, how='inner'),
    which has segment geometry and 'index_right' referencing crash rows.
    """
    joined = joined.reset_index(drop=True)  # ensure unique index after sjoin
    crash_geoms = local_crash_data.geometry.iloc[joined['index_right'].values].values
    joined['_dist'] = joined.geometry.distance(
        gpd.GeoSeries(crash_geoms, index=joined.index, crs=joined.crs)
    )
    idx = joined.groupby(['index_right', dissolved_pkey_col])['_dist'].idxmin()
    return joined.loc[idx].drop(columns=['_dist'])


def _proportional_aggregate(
    segment_counts: pd.DataFrame,
    short_windows: pd.DataFrame,
    dissolved_pkey_col: str,
    beg_col: str,
    end_col: str,
    count_columns: list,
    sw_pk: str,
) -> pd.DataFrame:
    """
    Aggregate 0.05mi segment crash counts to short_windows proportionally
    based on LRS overlap length. Uses DuckDB range join to avoid the
    cartesian-product memory blowup of pandas equality merge.
    """
    sw_subset = short_windows[[sw_pk, dissolved_pkey_col, beg_col, end_col]]

    weighted_sums = ",\n        ".join(
        f'SUM("{col}" * _proportion) AS "{col}"' for col in count_columns
    )

    sql = f"""
    WITH overlap_pairs AS (
        SELECT
            w."{sw_pk}" AS sw_pk,
            s.*,
            (LEAST(s."{end_col}", w."{end_col}") - GREATEST(s."{beg_col}", w."{beg_col}"))
              / NULLIF(s."{end_col}" - s."{beg_col}", 0) AS _proportion
        FROM segment_counts AS s
        JOIN sw_subset AS w
          ON s."{dissolved_pkey_col}" = w."{dissolved_pkey_col}"
         AND s."{end_col}" > w."{beg_col}"
         AND s."{beg_col}" < w."{end_col}"
    )
    SELECT sw_pk AS "{sw_pk}",
        {weighted_sums}
    FROM overlap_pairs
    GROUP BY sw_pk
    """

    con = duckdb.connect()
    try:
        con.register("segment_counts", segment_counts)
        con.register("sw_subset", sw_subset)
        result = con.execute(sql).df()
    finally:
        con.close()
    return result


def _build_empty_crash_counts(datastore: DataStore, roadway_network: gpd.GeoDataFrame) -> DataStore:
    """Build an empty crash counts GeoDataFrame when no crashes are found."""
    config = datastore.config
    config_modes = set(chain.from_iterable(config.mode_mapping.values()))
    config_severities = set(config.severity_mapping.values())
    count_columns = [
        f"{mode}_{sev}" for mode in sorted(config_modes) for sev in sorted(config_severities)
    ]
    crash_counts = roadway_network[[config.short_window_pk, "geom"]].copy()
    for col in count_columns:
        crash_counts[col] = 0.0
    crash_counts = gpd.GeoDataFrame(crash_counts, geometry="geom", crs=roadway_network.crs)
    return replace(datastore, local_crash_counts=crash_counts)


def compute_crash_counts(
    datastore: DataStore,
) -> DataStore:
    """
    Distribute crash counts on 0.05mi segments using sjoin, then aggregate
    back to short_window level with proportional length-based splitting.

    Steps:
    1. sjoin crashes to 0.05mi segments (dwithin)
    2. Filter to nearest segment per (crash, dissolved_road)
    3. Count crashes by mode/severity on 0.05mi segments
    4. Distribute/blur along dissolved roads
    5. Aggregate 0.05mi counts to short_windows proportionally by LRS overlap
    """
    config = datastore.config
    local_crash_data = datastore.local_crash_data.copy()
    roadway_network = datastore.roadway_network.copy()
    segments_0p05mi = datastore.segments_0p05mi.copy()

    dissolved_pkey_col = config.dissolved_pkey_col
    seq_col = config.segment_sequence_col
    beg_col = config.start_distance_col
    end_col = config.end_distance_col
    seg_pk = config.segments_0p05mi_pk

    # --- Step 1: sjoin crashes to 0.05mi segments ---
    sjoin_dist_meters = config.sjoin_dist * 0.3048
    joined = gpd.sjoin(
        segments_0p05mi,
        local_crash_data,
        how="inner",
        predicate="dwithin",
        distance=sjoin_dist_meters,
    )

    if len(joined) == 0:
        return _build_empty_crash_counts(datastore, roadway_network)

    # Debug upload
    if config.debug:
        with config.db_engine.connect() as conn:
            joined.to_postgis(
                name=config.crash_sjoin_debug_table,
                con=conn,
                if_exists='replace',
                index=False,
                schema=config.debug_schema,
            )

    # --- Step 2: nearest segment per (crash, dissolved_road) ---
    joined = _filter_nearest_per_road(joined, local_crash_data, dissolved_pkey_col)

    # --- Step 3: count crashes by mode/severity on 0.05mi segments ---
    mode_col = config.mode_col_name
    severity_col = config.severity_col_name

    config_modes = set(chain.from_iterable(config.mode_mapping.values()))
    config_severities = set(config.severity_mapping.values())
    count_columns = []

    for mode_full in sorted(config_modes):
        for sev_full in sorted(config_severities):
            col_name = f"{mode_full}_{sev_full}"
            joined[col_name] = (
                (joined[mode_col] == mode_full) & (joined[severity_col] == sev_full)
            ).astype(float)
            count_columns.append(col_name)

    # Sum counts per 0.05mi segment
    segment_counts = joined.groupby(seg_pk)[count_columns].sum().reset_index()

    # Merge back to full segments table to include segments with zero crashes
    segment_counts = segments_0p05mi[[seg_pk, dissolved_pkey_col, seq_col, beg_col, end_col]].merge(
        segment_counts,
        on=seg_pk,
        how='left',
    )
    segment_counts[count_columns] = segment_counts[count_columns].fillna(0)

    # --- Step 4: distribute/blur along dissolved roads ---
    if config.distribute_blur_size > 0:
        segment_counts = _distribute_crash_counts(
            df=segment_counts,
            value_columns=count_columns,
            dissolved_pkey_col=dissolved_pkey_col,
            segment_sequence_col=seq_col,
            blur_size=config.distribute_blur_size,
            blur_style=config.distribute_blur_style,
        )

    # --- Step 5: proportional aggregate 0.05mi -> short_windows ---
    aggregated = _proportional_aggregate(
        segment_counts=segment_counts,
        short_windows=roadway_network,
        dissolved_pkey_col=dissolved_pkey_col,
        beg_col=beg_col,
        end_col=end_col,
        count_columns=count_columns,
        sw_pk=config.short_window_pk,
    )

    # --- Build final GeoDataFrame ---
    crash_counts = roadway_network[[config.short_window_pk, "geom"]].merge(
        aggregated,
        on=config.short_window_pk,
        how='left',
    )
    crash_counts[count_columns] = crash_counts[count_columns].fillna(0)

    crash_counts = gpd.GeoDataFrame(
        crash_counts,
        geometry="geom",
        crs=roadway_network.crs,
    )

    datastore = replace(
        datastore,
        local_crash_counts=crash_counts,
        segments_0p05mi=None,
        local_crash_data=None,
        roadway_network=None,
    )

    return datastore


def update_crash_estimates_from_observations(
    datastore: DataStore,
) -> DataStore:
    """
    Update crash estimates based on new observations.
    """
    # pull input data from datastore
    all_severity_estimates = datastore.all_severity_estimates.copy()
    local_crash_counts = datastore.local_crash_counts.copy()

    if datastore.config.crash_missing_non_fatal:
        all_severities = ['k']
    else:
        all_severities = list(datastore.config.severity_mapping.keys())

    # join all severity estimates with local crash counts
    joined_estimates = all_severity_estimates.merge(
        local_crash_counts,
        how='left',
        on=[datastore.config.short_window_pk],
        suffixes=['', '_obs'],
    )

    # sum observed crash counts by mode and severity
    # In the config the severity_mapping is used to code severity in the crash table (severity full name)
    # to consistent kabco values (lower case, one letter)
    # Same for the mode, which maps a list of different mode values (mode full names) to ped, bike, mv, and all
    obs_columns = []
    for mode in datastore.config.mode_mapping:
        for severity in all_severities:
            obs_col = f"{mode}_{severity}_obs"
            obs_columns.append(obs_col)
            severity_full_name = datastore.config.severity_mapping[severity]
            mode_cols = [f"{mode_full_name}_{severity_full_name}" for mode_full_name in datastore.config.mode_mapping[mode]]
            joined_estimates[obs_col] = joined_estimates[mode_cols].sum(axis=1)

    # compute posterior estimates with overdispersion
    post_columns = [f"adj_{mode.value}_{severity}" for mode in FatalModes for severity in all_severities]
    prior_columns = [f"est_{mode.value}_{severity}" for mode in FatalModes for severity in all_severities]
    weight_columns = [f"w_{mode.value}_{severity}" for mode in FatalModes for severity in all_severities]
    cv_columns = [f"{mode.value}_cv" for mode in FatalModes for _ in all_severities]
    max_weight = datastore.config.max_est_weight
    decay = datastore.config.decay

    for post_col, prior_col, obs_col, cv_col, weight_col in zip(post_columns, prior_columns, obs_columns, cv_columns, weight_columns):
        # compute weight
        joined_estimates[weight_col] = max_weight / (1 + joined_estimates[cv_col] * joined_estimates[prior_col]) ** decay
        # compute posterior estimate
        joined_estimates[post_col] = (
            joined_estimates[weight_col] * joined_estimates[prior_col] + (1 - joined_estimates[weight_col]) * joined_estimates[obs_col]
        )

    # debug: upload joined estimates
    if datastore.config.debug:
        with datastore.config.db_engine.connect() as conn:
            joined_estimates.to_postgis(
                name=datastore.config.bayesian_update_debug_table,
                con=conn,
                if_exists='replace',
                index=False,
                schema=datastore.config.debug_schema,
            ) 

    # format output
    output_gdf = datastore.all_severity_estimates.copy()
    output_gdf[obs_columns] = joined_estimates[obs_columns]  # add observed crash counts
    output_gdf[weight_columns] = joined_estimates[weight_columns]  # add overdispersion parameters
    output_gdf[post_columns] = joined_estimates[post_columns] # add posterior estimates

    datastore = replace(
        datastore,
        bayesian_crash_estimates=output_gdf,
    )

    return datastore


def upload_updated_crash_estimates(
    datastore: DataStore,
) -> DataStore:
    """
    Upload the updated crash estimates to the datastore.
    This function is a placeholder for the upload logic.
    """
    with datastore.config.db_engine.connect() as conn:
        datastore.bayesian_crash_estimates.to_postgis(
            name=datastore.config.bayesian_estimates_table,
            con=conn,
            if_exists='replace',
            index=False,
            schema=datastore.config.output_schema,
            chunksize=5000,
        )
    with datastore.config.db_engine.connect() as conn:
        datastore.local_crash_counts.to_postgis(
            name=datastore.config.output_observed_crash_counts_table,
            con=conn,
            if_exists='replace',
            index=False,
            schema=datastore.config.debug_schema,
            chunksize=5000,
        )

    return datastore
