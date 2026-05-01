"""Prediction interval helpers."""

from __future__ import annotations

import numpy as np
import pandas as pd


def residual_quantile_bounds(
    actual: pd.DataFrame,
    validation_forecast: pd.DataFrame,
    future_forecast: pd.DataFrame,
    alpha: float = 0.2,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Calibrate future interval bounds from validation residual quantiles."""

    if not 0 < alpha < 1:
        raise ValueError("alpha must be between 0 and 1.")

    actual_aligned, forecast_aligned = _align_matrices(actual, validation_forecast)
    residuals = actual_aligned - forecast_aligned
    global_residuals = _finite_values(residuals)
    if global_residuals.size == 0:
        raise ValueError("No finite validation residuals are available.")

    future = _prepare_matrix(future_forecast)
    lower = pd.DataFrame(index=future.index, columns=future.columns, dtype=float)
    upper = pd.DataFrame(index=future.index, columns=future.columns, dtype=float)
    for column in future.columns:
        if column in residuals.columns:
            column_residuals = _finite_values(residuals.loc[:, [column]])
        else:
            column_residuals = np.array([], dtype=float)
        calibration_residuals = (
            column_residuals if column_residuals.size else global_residuals
        )
        lower_offset = float(np.quantile(calibration_residuals, alpha / 2))
        upper_offset = float(np.quantile(calibration_residuals, 1 - alpha / 2))
        lower.loc[:, column] = future.loc[:, column] + lower_offset
        upper.loc[:, column] = future.loc[:, column] + upper_offset

    return order_interval_bounds(lower, upper, values=future)


def order_interval_bounds(
    lower: pd.DataFrame,
    upper: pd.DataFrame,
    values: pd.DataFrame | None = None,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Return ordered lower/upper bounds, optionally forcing point inclusion."""

    lower_matrix = _prepare_matrix(lower)
    upper_matrix = _prepare_matrix(upper)
    if values is not None:
        values_matrix = _prepare_matrix(values)
        lower_matrix = lower_matrix.reindex_like(values_matrix)
        upper_matrix = upper_matrix.reindex_like(values_matrix)
    else:
        lower_matrix, upper_matrix = lower_matrix.align(
            upper_matrix,
            join="inner",
            axis=0,
        )
        lower_matrix, upper_matrix = lower_matrix.align(
            upper_matrix,
            join="inner",
            axis=1,
        )
        values_matrix = None

    lower_values = lower_matrix.to_numpy(dtype=float)
    upper_values = upper_matrix.to_numpy(dtype=float)
    ordered_lower = np.fmin(lower_values, upper_values)
    ordered_upper = np.fmax(lower_values, upper_values)
    if values_matrix is not None:
        point_values = values_matrix.to_numpy(dtype=float)
        ordered_lower = np.fmin(ordered_lower, point_values)
        ordered_upper = np.fmax(ordered_upper, point_values)

    return (
        pd.DataFrame(
            ordered_lower,
            index=lower_matrix.index,
            columns=lower_matrix.columns,
        ),
        pd.DataFrame(
            ordered_upper,
            index=upper_matrix.index,
            columns=upper_matrix.columns,
        ),
    )


def _align_matrices(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    actual_matrix = _prepare_matrix(actual)
    forecast_matrix = _prepare_matrix(forecast)
    actual_aligned, forecast_aligned = actual_matrix.align(
        forecast_matrix,
        join="inner",
        axis=0,
    )
    actual_aligned, forecast_aligned = actual_aligned.align(
        forecast_aligned,
        join="inner",
        axis=1,
    )
    if actual_aligned.empty or actual_aligned.shape[1] == 0:
        raise ValueError("Actual and forecast matrices have no overlapping data.")
    return actual_aligned, forecast_aligned


def _prepare_matrix(matrix: pd.DataFrame) -> pd.DataFrame:
    result = matrix.copy()
    result.index = pd.to_datetime(result.index)
    return result.sort_index().astype(float)


def _finite_values(matrix: pd.DataFrame) -> np.ndarray:
    values = matrix.to_numpy(dtype=float).ravel()
    return values[np.isfinite(values)]
