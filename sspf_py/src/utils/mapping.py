"""Shared mapping helpers for classification and styling."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Optional

import numpy as np
import pandas as pd
import plotly.express as px
import jenkspy


@dataclass
class ChoroplethScheme:
    breaks: list[float]
    bins: pd.Series
    colors: list[str]
    legend: list[dict]


def _deduplicate_breaks(breaks: list[float]) -> list[float]:
    cleaned: list[float] = []
    for value in breaks:
        value = float(value)
        if not cleaned or abs(value - cleaned[-1]) > 1e-9:
            cleaned.append(value)
    if len(cleaned) < 2:
        base = cleaned[0] if cleaned else 0.0
        cleaned = [base, base + 1e-6]
    return cleaned


_JENKS_SAMPLE_SIZE = 10_000


def _compute_breaks(values: np.ndarray, n_classes: int) -> list[float]:
    if values.size > _JENKS_SAMPLE_SIZE:
        rng = np.random.default_rng(42)
        idx = rng.choice(values.size, size=_JENKS_SAMPLE_SIZE - 2, replace=False)
        sample = np.empty(_JENKS_SAMPLE_SIZE, dtype=values.dtype)
        sample[0] = values.min()
        sample[1] = values.max()
        sample[2:] = values[idx]
        values = sample

    unique_values = np.unique(values)
    n_classes = max(1, min(n_classes, unique_values.size))

    while n_classes > 1:
        try:
            breaks = jenkspy.jenks_breaks(values, n_classes=n_classes)
            return _deduplicate_breaks(breaks)
        except ValueError:
            n_classes -= 1

    min_val = float(values.min())
    max_val = float(values.max())
    if min_val == max_val:
        max_val = min_val + 1e-6
    return [min_val, max_val]


def build_color_scheme(
    values: pd.Series | np.ndarray | list,
    *,
    max_bins: int,
    colorscale: str,
    label_precision: int = 1,
    label_formatter: Optional[Callable[[float, float], str]] = None,
) -> Optional[ChoroplethScheme]:
    """Return Jenks classification, sampled colors, and legend metadata."""

    series = values if isinstance(values, pd.Series) else pd.Series(values)
    valid = series.dropna()
    if valid.empty:
        return None

    breaks = _compute_breaks(valid.to_numpy(dtype=float), max_bins)
    try:
        bin_assignments = pd.cut(series, bins=breaks, labels=False, include_lowest=True)
    except ValueError:
        return None

    bin_count = len(breaks) - 1
    if bin_count <= 0:
        return None

    if bin_count == 1:
        scale = px.colors.get_colorscale(colorscale)
        colors = [scale[-1][1] if scale else "#000000"]
    else:
        colors = px.colors.sample_colorscale(colorscale, bin_count, low=0, high=1)

    fmt = f"{{:.{label_precision}f}} - {{:.{label_precision}f}}"
    legend = []
    for idx in range(bin_count):
        start = breaks[idx]
        end = breaks[idx + 1]
        label = label_formatter(start, end) if label_formatter else fmt.format(start, end)
        legend.append({"label": label, "color": colors[idx]})

    return ChoroplethScheme(breaks=breaks, bins=bin_assignments, colors=colors, legend=legend)
