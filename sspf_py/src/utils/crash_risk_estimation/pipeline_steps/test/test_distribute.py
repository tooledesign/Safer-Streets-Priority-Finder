"""
Tests for crash count distribution: kernel blur, nearest-per-road filtering,
proportional aggregation, and full sjoin-based workflow.

Run with: pytest test_distribute.py -v
"""
import numpy as np
import pandas as pd
import geopandas as gpd
from shapely.geometry import LineString, Point

from utils.crash_risk_estimation.pipeline_steps.bayesian_updating import (
    _build_kernel,
    _distribute_crash_counts,
    _filter_nearest_per_road,
    _proportional_aggregate,
)


# ──────────────────────────────────────────────────────────
# Helpers
# ──────────────────────────────────────────────────────────

def _make_line_segments(dissolved_pkey, begs, ends, y=0):
    """Create a GeoDataFrame of line segments on a single dissolved road."""
    n = len(begs)
    return gpd.GeoDataFrame(
        {
            "dissolved_pkey": [dissolved_pkey] * n,
            "segment_sequence": list(range(n)),
            "start_distance_m": begs,
            "end_distance_m": ends,
            "short_0p05mi_window_id": list(range(n)),
        },
        geometry=[LineString([(b, y), (e, y)]) for b, e in zip(begs, ends)],
        crs="EPSG:32617",
    )


def _make_short_windows(dissolved_pkey, begs, ends, y=0, id_offset=0):
    """Create a GeoDataFrame of short_window segments."""
    n = len(begs)
    return gpd.GeoDataFrame(
        {
            "dissolved_pkey": [dissolved_pkey] * n,
            "start_distance_m": begs,
            "end_distance_m": ends,
            "short_window_id": list(range(id_offset, id_offset + n)),
        },
        geometry=[LineString([(b, y), (e, y)]) for b, e in zip(begs, ends)],
        crs="EPSG:32617",
    )


# ──────────────────────────────────────────────────────────
# Kernel tests (unchanged from original)
# ──────────────────────────────────────────────────────────

def test_kernel_linear():
    k = _build_kernel(2, "linear")
    assert len(k) == 5
    assert k[2] == max(k)
    assert np.isclose(k[0], k[4])
    assert k[1] > k[0]


def test_kernel_uniform():
    k = _build_kernel(3, "uniform")
    assert np.allclose(k, 1.0)


# ──────────────────────────────────────────────────────────
# _distribute_crash_counts tests (blur on 0.05mi segments)
# ──────────────────────────────────────────────────────────

def test_no_blur():
    df = pd.DataFrame({
        'dissolved_pkey': [1, 1, 1],
        'segment_sequence': [0, 1, 2],
        'count': [10.0, 0.0, 0.0],
    })
    result = _distribute_crash_counts(df, ['count'], 'dissolved_pkey', 'segment_sequence', 0, 'linear')
    assert np.allclose(result['count'].values, [10.0, 0.0, 0.0])


def test_blur_sum_preservation():
    df = pd.DataFrame({
        'dissolved_pkey': [1] * 10,
        'segment_sequence': list(range(10)),
        'count': [0, 0, 0, 0, 10.0, 0, 0, 0, 0, 0],
    })
    result = _distribute_crash_counts(df, ['count'], 'dissolved_pkey', 'segment_sequence', 2, 'linear')
    assert np.isclose(result['count'].sum(), 10.0)


def test_blur_edge_preservation():
    df = pd.DataFrame({
        'dissolved_pkey': [1] * 5,
        'segment_sequence': list(range(5)),
        'count': [10.0, 10.0, 0, 0, 0],
    })
    result = _distribute_crash_counts(df, ['count'], 'dissolved_pkey', 'segment_sequence', 2, 'linear')
    assert np.isclose(result['count'].sum(), 20.0)


def test_blur_multiple_roads():
    df = pd.DataFrame({
        'dissolved_pkey': [1, 1, 1, 2, 2, 2],
        'segment_sequence': [0, 1, 2, 0, 1, 2],
        'count': [10.0, 0, 0, 0, 0, 20.0],
    })
    result = _distribute_crash_counts(df, ['count'], 'dissolved_pkey', 'segment_sequence', 1, 'linear')
    road1 = result[result['dissolved_pkey'] == 1]['count']
    road2 = result[result['dissolved_pkey'] == 2]['count']
    assert np.isclose(road1.sum(), 10.0)
    assert np.isclose(road2.sum(), 20.0)


# ──────────────────────────────────────────────────────────
# _filter_nearest_per_road tests
# ──────────────────────────────────────────────────────────

def test_filter_nearest_single_road():
    """Crash near multiple segments on one road → only nearest kept."""
    segments = gpd.GeoDataFrame(
        {
            "dissolved_pkey": [1, 1, 1],
            "seg_id": [0, 1, 2],
        },
        geometry=[
            LineString([(0, 0), (50, 0)]),
            LineString([(50, 0), (100, 0)]),
            LineString([(100, 0), (150, 0)]),
        ],
        crs="EPSG:32617",
    )
    crashes = gpd.GeoDataFrame(
        {"crash_id": [1]},
        geometry=[Point(55, 3)],
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(segments, crashes, how="inner", predicate="dwithin", distance=50)
    result = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    # Should keep only the nearest segment (seg_id=1, which contains x=55)
    assert len(result) == 1
    assert result.iloc[0]["seg_id"] == 1


def test_filter_nearest_non_unique_index():
    """Regression: sjoin produces non-unique index when one segment matches
    multiple crashes. _filter_nearest_per_road must not over-count."""
    segments = gpd.GeoDataFrame(
        {"dissolved_pkey": [1, 1], "seg_id": ["A", "B"]},
        geometry=[
            LineString([(0, 0), (50, 0)]),
            LineString([(50, 0), (100, 0)]),
        ],
        crs="EPSG:32617",
    )
    # Three crashes: two near segment A, one near segment B
    crashes = gpd.GeoDataFrame(
        {"crash_id": [1, 2, 3]},
        geometry=[Point(10, 5), Point(20, 5), Point(80, 5)],
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(segments, crashes, how="inner", predicate="dwithin", distance=10)
    assert not joined.index.is_unique, "Test requires non-unique index"

    result = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    # Exactly 3 rows: one per (crash, dissolved_road) pair
    assert len(result) == 3


def test_filter_nearest_multiple_roads():
    """Crash near segments on two dissolved roads → one per road."""
    road1_seg = gpd.GeoDataFrame(
        {"dissolved_pkey": [1], "seg_id": [0]},
        geometry=[LineString([(0, 0), (100, 0)])],
        crs="EPSG:32617",
    )
    road2_seg = gpd.GeoDataFrame(
        {"dissolved_pkey": [2], "seg_id": [10]},
        geometry=[LineString([(0, 20), (100, 20)])],
        crs="EPSG:32617",
    )
    segments = pd.concat([road1_seg, road2_seg], ignore_index=True)
    segments = gpd.GeoDataFrame(segments, geometry="geometry", crs="EPSG:32617")

    crashes = gpd.GeoDataFrame(
        {"crash_id": [1]},
        geometry=[Point(50, 10)],  # equidistant from both roads
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(segments, crashes, how="inner", predicate="dwithin", distance=25)
    result = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    # One match per dissolved road
    assert len(result) == 2
    assert set(result["dissolved_pkey"]) == {1, 2}


def test_filter_nearest_passthrough_single_match():
    """Single match per road passes through unchanged."""
    segments = gpd.GeoDataFrame(
        {"dissolved_pkey": [1], "seg_id": [0]},
        geometry=[LineString([(0, 0), (100, 0)])],
        crs="EPSG:32617",
    )
    crashes = gpd.GeoDataFrame(
        {"crash_id": [1]},
        geometry=[Point(50, 5)],
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(segments, crashes, how="inner", predicate="dwithin", distance=10)
    result = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    assert len(result) == 1


# ──────────────────────────────────────────────────────────
# _proportional_aggregate tests
# ──────────────────────────────────────────────────────────

def test_proportional_aggregate_aligned_boundaries():
    """When 0.05mi and short_window boundaries align, no splitting occurs."""
    scored = pd.DataFrame({
        "dissolved_pkey": [1, 1],
        "start_distance_m": [0.0, 50.0],
        "end_distance_m": [50.0, 100.0],
        "score": [4.0, 6.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0, 1],
        "dissolved_pkey": [1, 1],
        "start_distance_m": [0.0, 50.0],
        "end_distance_m": [50.0, 100.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["score"], "short_window_id",
    )
    np.testing.assert_allclose(
        result.sort_values("short_window_id")["score"].values, [4.0, 6.0]
    )


def test_proportional_aggregate_straddling():
    """A 0.05mi segment straddling two short_windows splits proportionally."""
    # One segment from 40 to 60, straddling window [0,50] and [50,100]
    scored = pd.DataFrame({
        "dissolved_pkey": [1],
        "start_distance_m": [40.0],
        "end_distance_m": [60.0],
        "score": [10.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0, 1],
        "dissolved_pkey": [1, 1],
        "start_distance_m": [0.0, 50.0],
        "end_distance_m": [50.0, 100.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["score"], "short_window_id",
    )
    result = result.sort_values("short_window_id")
    # overlap with sw0: min(60,50)-max(40,0) = 10, proportion = 10/20 = 0.5 → 5.0
    # overlap with sw1: min(60,100)-max(40,50) = 10, proportion = 10/20 = 0.5 → 5.0
    np.testing.assert_allclose(result["score"].values, [5.0, 5.0])


def test_proportional_aggregate_total_preservation():
    """Total score is preserved across aggregation."""
    # Four fine segments, two coarse windows
    scored = pd.DataFrame({
        "dissolved_pkey": [1, 1, 1, 1],
        "start_distance_m": [0.0, 25.0, 50.0, 75.0],
        "end_distance_m": [25.0, 50.0, 75.0, 100.0],
        "score": [1.0, 2.0, 3.0, 4.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0, 1],
        "dissolved_pkey": [1, 1],
        "start_distance_m": [0.0, 60.0],
        "end_distance_m": [60.0, 100.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["score"], "short_window_id",
    )
    np.testing.assert_allclose(result["score"].sum(), 10.0)


def test_proportional_aggregate_multiple_columns():
    """Multiple score columns are aggregated independently."""
    scored = pd.DataFrame({
        "dissolved_pkey": [1, 1],
        "start_distance_m": [0.0, 50.0],
        "end_distance_m": [50.0, 100.0],
        "ped_K": [3.0, 0.0],
        "bike_A": [0.0, 5.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0],
        "dissolved_pkey": [1],
        "start_distance_m": [0.0],
        "end_distance_m": [100.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["ped_K", "bike_A"], "short_window_id",
    )
    np.testing.assert_allclose(result["ped_K"].values, [3.0])
    np.testing.assert_allclose(result["bike_A"].values, [5.0])


def test_proportional_aggregate_multiple_roads():
    """Scores from different dissolved roads stay independent."""
    scored = pd.DataFrame({
        "dissolved_pkey": [1, 1, 2, 2],
        "start_distance_m": [0.0, 50.0, 0.0, 50.0],
        "end_distance_m": [50.0, 100.0, 50.0, 100.0],
        "score": [10.0, 0.0, 0.0, 20.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0, 1, 2, 3],
        "dissolved_pkey": [1, 1, 2, 2],
        "start_distance_m": [0.0, 50.0, 0.0, 50.0],
        "end_distance_m": [50.0, 100.0, 50.0, 100.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["score"], "short_window_id",
    )
    result = result.sort_values("short_window_id")
    np.testing.assert_allclose(result["score"].values, [10.0, 0.0, 0.0, 20.0])


def test_proportional_aggregate_stub_segment():
    """A short stub segment at end of road aggregates correctly."""
    scored = pd.DataFrame({
        "dissolved_pkey": [1, 1, 1],
        "start_distance_m": [0.0, 50.0, 100.0],
        "end_distance_m": [50.0, 100.0, 120.0],  # stub: only 20m
        "score": [1.0, 1.0, 1.0],
    })
    sw = pd.DataFrame({
        "short_window_id": [0],
        "dissolved_pkey": [1],
        "start_distance_m": [0.0],
        "end_distance_m": [120.0],
    })

    result = _proportional_aggregate(
        scored, sw, "dissolved_pkey",
        "start_distance_m", "end_distance_m", ["score"], "short_window_id",
    )
    np.testing.assert_allclose(result["score"].values, [3.0])


# ──────────────────────────────────────────────────────────
# Full workflow integration tests
# ──────────────────────────────────────────────────────────

def test_full_workflow_count_preservation():
    """sjoin → filter → count → blur → aggregate preserves total crash count."""
    # One dissolved road, 200m long
    fine_segs = _make_line_segments(1, [0, 50, 100, 150], [50, 100, 150, 200])
    coarse_windows = _make_short_windows(1, [0, 100], [100, 200])

    # Two crashes: ped_K at x=30, bike_A at x=120
    crashes = gpd.GeoDataFrame(
        {
            "crash_mode": ["ped", "bike"],
            "crash_severity": ["K", "A"],
        },
        geometry=[Point(30, 2), Point(120, 3)],
        crs="EPSG:32617",
    )

    count_columns = ["ped_K", "bike_A"]

    # Step 1: sjoin
    joined = gpd.sjoin(fine_segs, crashes, how="inner", predicate="dwithin", distance=20)

    # Step 2: nearest per road
    joined = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    # Step 3: count
    for col in count_columns:
        mode, sev = col.split("_")
        joined[col] = (
            (joined["crash_mode"] == mode) & (joined["crash_severity"] == sev)
        ).astype(float)

    seg_counts = joined.groupby("short_0p05mi_window_id")[count_columns].sum().reset_index()
    seg_counts = fine_segs[["short_0p05mi_window_id", "dissolved_pkey", "segment_sequence",
                            "start_distance_m", "end_distance_m"]].merge(
        seg_counts, on="short_0p05mi_window_id", how="left",
    )
    seg_counts[count_columns] = seg_counts[count_columns].fillna(0)

    # Step 4: blur
    seg_counts = _distribute_crash_counts(
        seg_counts, count_columns, "dissolved_pkey", "segment_sequence", 1, "linear",
    )

    # Step 5: aggregate
    agg = _proportional_aggregate(
        seg_counts, coarse_windows, "dissolved_pkey",
        "start_distance_m", "end_distance_m", count_columns, "short_window_id",
    )

    # Total crash count preserved
    np.testing.assert_allclose(agg["ped_K"].sum(), 1.0, atol=1e-9)
    np.testing.assert_allclose(agg["bike_A"].sum(), 1.0, atol=1e-9)


def test_full_workflow_no_crashes():
    """When sjoin returns no matches, all counts should be zero."""
    fine_segs = _make_line_segments(1, [0, 50], [50, 100])

    # Crash far away from any segment
    crashes = gpd.GeoDataFrame(
        {"crash_mode": ["ped"], "crash_severity": ["K"]},
        geometry=[Point(50, 1000)],
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(fine_segs, crashes, how="inner", predicate="dwithin", distance=10)
    assert len(joined) == 0


def test_full_workflow_one_to_many_roads():
    """Crash near two dissolved roads → counted on both."""
    road1 = _make_line_segments(1, [0, 50], [50, 100], y=0)
    road2 = _make_line_segments(2, [0, 50], [50, 100], y=20)
    # Adjust PKs for road2
    road2["short_0p05mi_window_id"] = [10, 11]
    segments = pd.concat([road1, road2], ignore_index=True)
    segments = gpd.GeoDataFrame(segments, geometry="geometry", crs="EPSG:32617")

    # Crash at y=10 (equidistant)
    crashes = gpd.GeoDataFrame(
        {"crash_mode": ["ped"], "crash_severity": ["K"]},
        geometry=[Point(30, 10)],
        crs="EPSG:32617",
    )

    joined = gpd.sjoin(segments, crashes, how="inner", predicate="dwithin", distance=15)
    joined = _filter_nearest_per_road(joined, crashes, "dissolved_pkey")

    # Should have one match per dissolved road
    assert len(joined) == 2
    assert set(joined["dissolved_pkey"]) == {1, 2}
