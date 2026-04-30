"""Preprocessing helpers for recovery forecasting."""

from __future__ import annotations

import numpy as np
import pandas as pd


def impute_time_series(series: pd.Series, method: str = "interpolate") -> pd.Series:
    """Fill missing values in a single time series."""

    if method == "interpolate":
        return series.astype(float).interpolate(limit_direction="both")
    if method == "ffill_bfill":
        return series.astype(float).ffill().bfill()
    raise ValueError(f"Unknown imputation method: {method}")


def monthly_seasonal_factors(
    series: pd.Series,
    period: int = 12,
    multiplicative: bool = True,
) -> pd.Series:
    """Estimate stable month-of-year seasonal factors from historical data."""

    values = series.dropna().astype(float)
    if len(values) < period * 2:
        raise ValueError("At least two full seasonal periods are recommended.")
    if not isinstance(values.index, pd.DatetimeIndex):
        raise ValueError("series must use a DatetimeIndex.")

    if multiplicative:
        if (values <= 0).any():
            raise ValueError("Multiplicative seasonality requires positive values.")
        transformed = np.log(values)
    else:
        transformed = values

    from statsmodels.tsa.seasonal import STL

    decomposition = STL(transformed, period=period, seasonal=period + 1).fit()
    seasonal = pd.Series(decomposition.seasonal, index=values.index)
    factors = seasonal.groupby(seasonal.index.month).mean()
    factors.index.name = "month"
    return factors


def apply_monthly_seasonality(
    trend: pd.Series,
    seasonal_factors: pd.Series,
    multiplicative: bool = True,
) -> pd.Series:
    """Apply month-of-year seasonal factors to a trend forecast."""

    if not isinstance(trend.index, pd.DatetimeIndex):
        raise ValueError("trend must use a DatetimeIndex.")
    factors = trend.index.month.map(seasonal_factors.to_dict()).to_numpy(dtype=float)
    if multiplicative:
        return trend * np.exp(factors)
    return trend + factors


def stl_monthly_seasonal_multipliers(
    matrix: pd.DataFrame,
    period: int = 12,
) -> pd.DataFrame:
    """Estimate multiplicative month-of-year factors via STL on log series.

    This follows the paper's seasonal-trend factorization: estimate stable seasonal
    effects with STL on the logarithmic scale, then use exp(seasonal) as the
    multiplier for de-seasonalizing and re-seasonalizing recovery curves.
    """

    if period != 12:
        raise ValueError(
            "stl_monthly_seasonal_multipliers currently requires period=12."
        )
    if not isinstance(matrix.index, pd.DatetimeIndex):
        raise ValueError("matrix must use a DatetimeIndex.")

    values = matrix.copy()
    values.index = pd.to_datetime(values.index)
    values = values.sort_index()
    if len(values) < period * 2:
        raise ValueError("At least two full seasonal periods are required.")

    factors = {}
    for column in values.columns:
        factors[column] = _stl_monthly_multipliers_for_series(
            values[column],
            period=period,
        )
    result = pd.DataFrame(factors).reindex(range(1, period + 1))
    result.index.name = "month"
    if result.isna().any().any():
        raise ValueError("STL seasonal decomposition did not cover all months.")
    return result


def _stl_monthly_multipliers_for_series(
    series: pd.Series,
    period: int,
) -> pd.Series:
    values = series.dropna().astype(float)
    if len(values) < period * 2:
        raise ValueError("At least two full seasonal periods are required.")
    if (values <= 0).any():
        raise ValueError("Multiplicative STL seasonality requires positive values.")

    from statsmodels.tsa.seasonal import STL

    transformed = np.log(values)
    decomposition = STL(
        transformed,
        period=period,
        seasonal=_stl_seasonal_window(period),
    ).fit()
    seasonal = pd.Series(decomposition.seasonal, index=values.index)
    return np.exp(seasonal.groupby(seasonal.index.month).mean())


def _stl_seasonal_window(period: int) -> int:
    window = period + 1 if period % 2 == 0 else period + 2
    return max(window, 7)
