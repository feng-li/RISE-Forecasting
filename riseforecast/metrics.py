"""Forecast accuracy and interval metrics."""

from __future__ import annotations

import numpy as np
import pandas as pd

ArrayLike = np.ndarray | pd.Series | pd.DataFrame | list[float]


def _to_numpy(values: ArrayLike) -> np.ndarray:
    return np.asarray(values, dtype=float)


def mae(actual: ArrayLike, forecast: ArrayLike) -> float:
    """Mean absolute error."""

    actual_arr = _to_numpy(actual)
    forecast_arr = _to_numpy(forecast)
    return float(np.nanmean(np.abs(actual_arr - forecast_arr)))


def mape(actual: ArrayLike, forecast: ArrayLike) -> float:
    """Mean absolute percentage error, ignoring zero actual values."""

    actual_arr = _to_numpy(actual)
    forecast_arr = _to_numpy(forecast)
    mask = actual_arr != 0
    if not np.any(mask):
        raise ValueError("MAPE is undefined when all actual values are zero.")
    percentage_error = np.abs(
        (actual_arr[mask] - forecast_arr[mask]) / actual_arr[mask]
    )
    return float(np.nanmean(percentage_error))


def mase(
    actual: ArrayLike,
    forecast: ArrayLike,
    insample: ArrayLike,
    seasonal_period: int = 1,
) -> float:
    """Mean absolute scaled error."""

    actual_arr = _to_numpy(actual)
    forecast_arr = _to_numpy(forecast)
    insample_arr = _to_numpy(insample)
    if seasonal_period < 1:
        raise ValueError("seasonal_period must be at least 1.")
    if insample_arr.size <= seasonal_period:
        raise ValueError("insample must be longer than seasonal_period.")
    naive_errors = np.abs(
        insample_arr[seasonal_period:] - insample_arr[:-seasonal_period]
    )
    scale = np.nanmean(naive_errors)
    if scale == 0:
        raise ValueError("MASE scale is zero.")
    return float(np.nanmean(np.abs(actual_arr - forecast_arr)) / scale)


def winkler_score(
    actual: ArrayLike,
    lower: ArrayLike,
    upper: ArrayLike,
    alpha: float = 0.2,
) -> float:
    """Average Winkler interval score."""

    if not 0 < alpha < 1:
        raise ValueError("alpha must be between 0 and 1.")
    actual_arr = _to_numpy(actual)
    lower_arr = _to_numpy(lower)
    upper_arr = _to_numpy(upper)
    width = upper_arr - lower_arr
    below = actual_arr < lower_arr
    above = actual_arr > upper_arr
    score = width.copy()
    score[below] += (2 / alpha) * (lower_arr[below] - actual_arr[below])
    score[above] += (2 / alpha) * (actual_arr[above] - upper_arr[above])
    return float(np.nanmean(score))
