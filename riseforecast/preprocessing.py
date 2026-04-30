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
