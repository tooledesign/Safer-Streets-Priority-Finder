"""Utility helpers for building summary statistics data frames."""

import json
from typing import Dict, List, Tuple

import pandas as pd
import sqlalchemy

from src.utils import db

try:
    from src.global_vars import SEVERITY_ORDER
except ImportError:  # pragma: no cover
    SEVERITY_ORDER = ["K", "A", "B", "C", "O"]


def _deserialize_json(value) -> Dict:
    if value is None:
        return {}
    if isinstance(value, dict):
        return value
    value = str(value).strip()
    return json.loads(value) if value else {}


def _fetch_study_context(study_id: int) -> Dict:
    eng = db.get_database_engine()
    stmt = sqlalchemy.text(
        """
        SELECT username,
               study_name,
               crash_mode_map,
               crash_sev_map,
               crash_costs,
               roads_fclass_map
        FROM gen_management.studies
        WHERE study_id = :sid
        """
    )
    with eng.begin() as conn:
        row = conn.execute(stmt, {"sid": study_id}).mappings().first()
    if not row:
        raise ValueError(f"Study id {study_id} was not found.")
    username = row["username"]
    study_name = row["study_name"]
    context = {
        "username": username,
        "study_name": study_name,
        "crash_table": f"inputs.crashes_{username}_{study_name}",
        "roads_table": f"inputs.roads_{username}_{study_name}",
        "crash_mode_map": _deserialize_json(row["crash_mode_map"]),
        "crash_sev_map": _deserialize_json(row["crash_sev_map"]),
        "crash_costs": _deserialize_json(row["crash_costs"]),
        "roads_fclass_map": _deserialize_json(row["roads_fclass_map"]),
    }
    return context


def get_study_identifiers(study_id: int) -> Tuple[str, str]:
    """Return the username/study name tuple used in table names."""
    ctx = _fetch_study_context(study_id)
    return ctx["username"], ctx["study_name"]


def _table_exists(table_name: str) -> bool:
    return db.table_exists(table_name)


def _get_crash_counts(table_name: str, column: str) -> Dict[str, int]:
    if not _table_exists(table_name):
        raise ValueError(f"Required table {table_name} does not exist.")
    eng = db.get_database_engine()
    stmt = sqlalchemy.text(
        f"""
        SELECT {column} AS category, COUNT(*)::BIGINT AS total_count
        FROM {table_name}
        GROUP BY {column}
        """
    )
    df = pd.read_sql(stmt, eng)
    return df.set_index("category")["total_count"].to_dict()


def _build_mapping_rows(
    mapping: Dict[str, str],
    metric_by_standard: Dict[str, float],
    total_metric: float,
    dataset_label: str,
    standard_label: str,
    order: List[str] | None = None,
) -> pd.DataFrame:
    total_metric = float(total_metric) if total_metric else 0.0
    rows = []
    grouped_mapping: Dict[str, list[str]] = {}

    for dataset_value, standard_value in mapping.items():
        grouped_mapping.setdefault(standard_value, []).append(str(dataset_value))

    handled = set()
    for standard_value, count in metric_by_standard.items():
        dataset_values = grouped_mapping.get(standard_value, [])
        rows.append(
            {
                dataset_label: "\n".join(dataset_values) if dataset_values else str(standard_value),
                standard_label: standard_value,
                "Total Count": float(count),
                "Proportion": (float(count) / total_metric) if total_metric else 0.0,
            }
        )
        handled.add(standard_value)

    for standard_value, dataset_values in grouped_mapping.items():
        if standard_value in handled:
            continue
        rows.append(
            {
                dataset_label: "\n".join(dataset_values),
                standard_label: standard_value,
                "Total Count": 0.0,
                "Proportion": 0.0,
            }
        )

    df = pd.DataFrame(rows)
    if order and not df.empty and standard_label in df.columns:
        order_index = {val: idx for idx, val in enumerate(order)}
        df["__order__"] = df[standard_label].map(order_index).fillna(len(order_index))
        df = df.sort_values("__order__").drop(columns="__order__").reset_index(drop=True)
    return df


def get_crash_severity_dataframe(study_id: int) -> pd.DataFrame:
    """Return counts and proportions by severity for the study."""
    ctx = _fetch_study_context(study_id)
    severity_counts = _get_crash_counts(ctx["crash_table"], "crash_severity")
    total = sum(severity_counts.values())
    mapping = ctx["crash_sev_map"] or {"Fatal": "K"} # mapping would be null if FARS is used, thus fall back to K only mapping
    return _build_mapping_rows(
        mapping=mapping,
        metric_by_standard=severity_counts,
        total_metric=total,
        dataset_label="Your Dataset's Severity",
        standard_label="Standard Severity",
        order=SEVERITY_ORDER,
    )


def get_crash_mode_dataframe(study_id: int) -> pd.DataFrame:
    """Return counts and proportions by crash mode."""
    ctx = _fetch_study_context(study_id)
    mode_counts = _get_crash_counts(ctx["crash_table"], "crash_mode")
    total = sum(mode_counts.values())
    df = _build_mapping_rows(
        mapping=ctx["crash_mode_map"],
        metric_by_standard=mode_counts,
        total_metric=total,
        dataset_label="Your Dataset's Mode",
        standard_label="Standard Mode",
    )
    mode_names = {
        "mv": "Motor Vehicle",
        "ped": "Pedestrian",
        "bike": "Bicycle",
        "other": "Other",
    }
    df["Standard Mode"] = df["Standard Mode"].map(mode_names)
    return df


def get_crash_cost_dataframe(study_id: int) -> pd.DataFrame:
    """Attach crash costs to observed crash counts by severity."""
    ctx = _fetch_study_context(study_id)
    severity_counts = _get_crash_counts(ctx["crash_table"], "crash_severity")
    total = sum(severity_counts.values())
    rows = []
    for severity in SEVERITY_ORDER:
        cost = ctx["crash_costs"].get(severity)
        if cost is None:
            continue
        count = float(severity_counts.get(severity, 0))
        rows.append(
            {
                "Severity": severity,
                "Crash Cost": float(cost),
                "Total Count": count,
                "Proportion": (count / total) if total else 0.0,
            }
        )
    return pd.DataFrame(rows)


def _get_road_miles(table_name: str) -> Dict[str, float]:
    if not _table_exists(table_name):
        raise ValueError(f"Required table {table_name} does not exist.")
    eng = db.get_database_engine()
    stmt = sqlalchemy.text(
        f"""
        SELECT functional_class AS category,
               COALESCE(SUM(ST_Length(geom)) / 1609.34, 0) AS total_miles
        FROM {table_name}
        GROUP BY functional_class
        """
    )
    df = pd.read_sql(stmt, eng)
    return df.set_index("category")["total_miles"].to_dict()


def get_functional_class_dataframe(study_id: int) -> pd.DataFrame:
    """Return roadway miles and proportions by functional class."""
    ctx = _fetch_study_context(study_id)
    miles_map = _get_road_miles(ctx["roads_table"])
    total = sum(miles_map.values())
    df = _build_mapping_rows(
        mapping=ctx["roads_fclass_map"],
        metric_by_standard=miles_map,
        total_metric=total,
        dataset_label="Your Dataset's Functional Class",
        standard_label="Standard Functional Class",
    )
    if not df.empty:
        df = df.rename(columns={"Total Count": "Total Miles"})
    return df


if __name__ == "__main__":
    # simple test
    study_id = 23
    print(get_crash_severity_dataframe(study_id))
    print(get_crash_mode_dataframe(study_id))
    print(get_crash_cost_dataframe(study_id))
    print(get_functional_class_dataframe(study_id))
