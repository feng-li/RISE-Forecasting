"""Preprocessing helpers for recovery forecasting."""

from __future__ import annotations

import warnings
from typing import Literal

import numpy as np
import pandas as pd

ImputationMethod = Literal["kalman", "kalman_smoothing", "interpolate", "ffill_bfill"]


def impute_time_series(
    series: pd.Series,
    method: ImputationMethod = "kalman",
    seasonal_period: int | None = None,
    min_observations: int = 3,
) -> pd.Series:
    """Fill missing values in a single time series."""

    if method in {"kalman", "kalman_smoothing"}:
        return kalman_smooth_impute(
            series,
            seasonal_period=seasonal_period,
            min_observations=min_observations,
        )
    if method in {"interpolate", "ffill_bfill"}:
        return _fallback_impute(series, method=method)
    raise ValueError(f"Unknown imputation method: {method}")


def kalman_smooth_impute(
    series: pd.Series,
    seasonal_period: int | None = None,
    level: str = "local linear trend",
    min_observations: int = 3,
    fallback_method: Literal["interpolate", "ffill_bfill"] = "interpolate",
) -> pd.Series:
    """Impute missing values with a structural state-space Kalman smoother.

    Observed values are preserved exactly. Only missing positions are filled with
    model-implied smoothed estimates. If the state-space model cannot be estimated
    reliably for a short or degenerate series, the deterministic fallback is used.
    """

    values = series.astype(float).copy()
    if not values.isna().any():
        return values
    if values.notna().sum() == 0:
        raise ValueError("Cannot impute a series with no observed values.")
    if min_observations < 1:
        raise ValueError("min_observations must be at least 1.")
    if values.notna().sum() < min_observations:
        return _fallback_impute(values, method=fallback_method)

    model_kwargs: dict[str, object] = {"level": level}
    if seasonal_period is not None:
        if seasonal_period < 2:
            raise ValueError("seasonal_period must be at least 2 when supplied.")
        enough_seasons = (
            values.notna().sum() >= 2 * seasonal_period
            and len(values) >= 2 * seasonal_period
        )
        if enough_seasons:
            model_kwargs["seasonal"] = seasonal_period

    try:
        from statsmodels.tsa.statespace.structural import UnobservedComponents

        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            model = UnobservedComponents(
                values.to_numpy(dtype=float),
                **model_kwargs,
            )
            result = model.fit(disp=False, maxiter=200)
            predicted = np.asarray(
                result.predict(start=0, end=len(values) - 1),
                dtype=float,
            )
    except Exception:
        return _fallback_impute(values, method=fallback_method)

    imputed = values.copy()
    fill_values = pd.Series(predicted, index=values.index)
    missing = imputed.isna()
    usable = missing & np.isfinite(fill_values)
    imputed.loc[usable] = fill_values.loc[usable]
    if imputed.isna().any():
        fallback = _fallback_impute(values, method=fallback_method)
        imputed = imputed.combine_first(fallback)
    return imputed.astype(float)


def _fallback_impute(
    series: pd.Series,
    method: Literal["interpolate", "ffill_bfill"],
) -> pd.Series:
    values = series.astype(float)
    if method == "interpolate":
        return values.interpolate(limit_direction="both")
    if method == "ffill_bfill":
        return values.ffill().bfill()
    raise ValueError(f"Unknown fallback imputation method: {method}")


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
