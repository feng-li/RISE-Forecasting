"""Evaluation helpers backed by utilsforecast."""

from __future__ import annotations

from collections.abc import Callable
from functools import partial

import pandas as pd
from utilsforecast.evaluation import evaluate
from utilsforecast.losses import bias, mae, mape, mase, mse, rmse, rmsse, smape

DEFAULT_METRIC_NAMES = ("mae", "mse", "rmse", "mape", "smape", "bias", "mase", "rmsse")


def evaluate_forecast_matrix(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
    train: pd.DataFrame,
    model_name: str,
    seasonality: int = 12,
    metrics: tuple[str, ...] = DEFAULT_METRIC_NAMES,
    include_overall: bool = True,
) -> pd.DataFrame:
    """Evaluate one date-by-series forecast matrix with utilsforecast metrics."""

    evaluation_frame = forecast_matrix_to_long(actual, forecast, model_name)
    train_frame = train_matrix_to_long(train, evaluation_frame["unique_id"].unique())
    metric_functions = _metric_functions(metrics, seasonality=seasonality)
    by_series = evaluate(
        evaluation_frame,
        metrics=metric_functions,
        models=[model_name],
        train_df=train_frame,
        id_col="unique_id",
        time_col="ds",
        target_col="y",
    )
    frames = [by_series.assign(level="series")]
    if include_overall:
        overall = evaluate(
            evaluation_frame,
            metrics=metric_functions,
            models=[model_name],
            train_df=train_frame,
            id_col="unique_id",
            time_col="ds",
            target_col="y",
            agg_fn="mean",
        )
        frames.append(overall.assign(unique_id="__overall__", level="overall"))

    result = pd.concat(frames, ignore_index=True)
    return result.rename(columns={model_name: "value"}).loc[
        :,
        ["level", "unique_id", "metric", "value"],
    ]


def forecast_matrix_to_long(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
    model_name: str,
) -> pd.DataFrame:
    """Convert aligned matrix forecasts to utilsforecast's long format."""

    actual_aligned, forecast_aligned = _align_matrices(actual, forecast)
    actual_series = _stack_matrix(actual_aligned).rename("y")
    forecast_series = _stack_matrix(forecast_aligned).rename(model_name)
    frame = pd.concat([actual_series, forecast_series], axis=1)
    frame = frame.dropna(subset=["y", model_name]).reset_index()
    if frame.empty:
        raise ValueError("No overlapping non-missing observations to evaluate.")
    return frame


def train_matrix_to_long(
    train: pd.DataFrame,
    series_ids: pd.Index | list[object],
) -> pd.DataFrame:
    """Convert an insample training matrix to utilsforecast's long format."""

    if train.empty:
        raise ValueError("Training matrix is empty.")
    matrix = train.copy()
    matrix.index = pd.to_datetime(matrix.index)
    matrix = matrix.sort_index().reindex(columns=list(series_ids))
    frame = _stack_matrix(matrix).rename("y").dropna().reset_index()
    if frame.empty:
        raise ValueError("Training matrix has no non-missing values.")
    return frame


def _align_matrices(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    actual_matrix = actual.copy()
    forecast_matrix = forecast.copy()
    actual_matrix.index = pd.to_datetime(actual_matrix.index)
    forecast_matrix.index = pd.to_datetime(forecast_matrix.index)
    actual_matrix = actual_matrix.sort_index()
    forecast_matrix = forecast_matrix.sort_index()
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


def _stack_matrix(matrix: pd.DataFrame) -> pd.Series:
    result = matrix.copy()
    result.index.name = "ds"
    result.columns.name = "unique_id"
    return result.stack(future_stack=True)


def _metric_functions(
    metrics: tuple[str, ...],
    seasonality: int,
) -> list[Callable]:
    registry: dict[str, Callable] = {
        "mae": mae,
        "mse": mse,
        "rmse": rmse,
        "mape": mape,
        "smape": smape,
        "bias": bias,
        "mase": partial(mase, seasonality=seasonality),
        "rmsse": partial(rmsse, seasonality=seasonality),
    }
    missing = [name for name in metrics if name not in registry]
    if missing:
        raise ValueError(f"Unknown utilsforecast metrics: {', '.join(missing)}")
    return [registry[name] for name in metrics]
