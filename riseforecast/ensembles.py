"""Forecast combination utilities."""

from __future__ import annotations

import warnings

import numpy as np
import pandas as pd

ForecastErrorMetric = str


def simple_average(forecasts: dict[str, pd.DataFrame]) -> pd.DataFrame:
    """Average forecast matrices with aligned indexes and columns."""

    if not forecasts:
        raise ValueError("At least one forecast matrix is required.")
    return sum(forecasts.values()) / len(forecasts)


def error_weighted_average(
    forecasts: dict[str, pd.DataFrame],
    errors: pd.Series,
) -> pd.DataFrame:
    """Combine forecasts with inverse-error weights."""

    if not forecasts:
        raise ValueError("At least one forecast matrix is required.")
    missing = [name for name in forecasts if name not in errors.index]
    if missing:
        raise ValueError(f"Missing errors for forecasts: {', '.join(missing)}")
    selected_errors = errors.loc[list(forecasts)].astype(float)
    if (selected_errors < 0).any() or not np.isfinite(selected_errors).all():
        raise ValueError("All errors must be finite and nonnegative.")
    zero_error = selected_errors == 0
    if zero_error.any():
        weights = pd.Series(0.0, index=selected_errors.index)
        weights.loc[zero_error] = 1 / zero_error.sum()
    else:
        weights = (1 / selected_errors) / (1 / selected_errors).sum()
    result = None
    for name, forecast in forecasts.items():
        weighted = forecast * weights.loc[name]
        result = weighted if result is None else result + weighted
    return result


def positive_stacking(
    train_forecasts: pd.DataFrame,
    train_actual: pd.Series,
    future_forecasts: pd.DataFrame,
    method: str = "ridge",
    alpha: float = 1.0,
) -> np.ndarray:
    """Fit positive ridge/lasso stacking and predict future values."""

    if method not in {"ridge", "lasso"}:
        raise ValueError("method must be 'ridge' or 'lasso'.")
    if method == "ridge":
        from sklearn.linear_model import Ridge

        model = Ridge(alpha=alpha, positive=True, fit_intercept=False)
    else:
        from sklearn.linear_model import Lasso

        model = Lasso(alpha=alpha, positive=True, fit_intercept=False)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        model.fit(
            train_forecasts.to_numpy(dtype=float),
            train_actual.to_numpy(dtype=float),
        )
    return model.predict(future_forecasts.to_numpy(dtype=float))


def forecast_errors(
    forecasts: dict[str, pd.DataFrame],
    actual: pd.DataFrame,
    metric: ForecastErrorMetric = "mape",
) -> pd.Series:
    """Compute one validation error per model over an aligned panel."""

    if not forecasts:
        raise ValueError("At least one forecast matrix is required.")
    errors = {
        name: _forecast_error(actual=actual, forecast=forecast, metric=metric)
        for name, forecast in forecasts.items()
    }
    result = pd.Series(errors, dtype=float, name=metric)
    result = result.loc[np.isfinite(result)]
    if result.empty:
        raise ValueError("No finite validation errors were produced.")
    return result


def select_top_models(
    errors: pd.Series,
    fraction: float = 0.8,
) -> tuple[str, ...]:
    """Select the best-performing fraction of models by validation error."""

    if not 0 < fraction <= 1:
        raise ValueError("selection fraction must be in (0, 1].")
    valid_errors = errors.astype(float).dropna()
    valid_errors = valid_errors.loc[np.isfinite(valid_errors)]
    if valid_errors.empty:
        raise ValueError("At least one finite validation error is required.")
    keep = max(1, int(np.floor(len(valid_errors) * fraction)))
    return tuple(valid_errors.sort_values(kind="mergesort").index[:keep])


def positive_stacking_panel(
    validation_forecasts: dict[str, pd.DataFrame],
    validation_actual: pd.DataFrame,
    future_forecasts: dict[str, pd.DataFrame],
    method: str = "ridge",
    alpha: float = 1.0,
) -> pd.DataFrame:
    """Fit positive stacking per series and predict a future forecast matrix."""

    if method not in {"ridge", "lasso"}:
        raise ValueError("method must be 'ridge' or 'lasso'.")
    _require_same_models(validation_forecasts, future_forecasts)
    if len(future_forecasts) == 1:
        return next(iter(future_forecasts.values()))

    models = tuple(future_forecasts)
    future_index = next(iter(future_forecasts.values())).index
    columns = next(iter(future_forecasts.values())).columns
    result = pd.DataFrame(index=future_index, columns=columns, dtype=float)
    actual = _prepare_matrix(validation_actual)

    for entity in columns:
        train_features = pd.DataFrame(
            {
                name: _prepare_matrix(validation_forecasts[name]).loc[:, entity]
                for name in models
            }
        )
        train = train_features.join(actual.loc[:, entity].rename("actual")).dropna()
        future_features = pd.DataFrame(
            {
                name: _prepare_matrix(future_forecasts[name]).loc[:, entity]
                for name in models
            },
            index=future_index,
        )
        if train.empty or future_features.isna().any().any():
            result.loc[:, entity] = future_features.mean(axis=1)
            continue
        result.loc[:, entity] = positive_stacking(
            train_forecasts=train.loc[:, list(models)],
            train_actual=train["actual"],
            future_forecasts=future_features.loc[:, list(models)],
            method=method,
            alpha=alpha,
        )
    return result


def combine_panel_forecasts(
    future_forecasts: dict[str, pd.DataFrame],
    ensemble: str,
    validation_errors: pd.Series | None = None,
    validation_forecasts: dict[str, pd.DataFrame] | None = None,
    validation_actual: pd.DataFrame | None = None,
    stacking_alpha: float = 1.0,
) -> pd.DataFrame:
    """Combine model forecast matrices using a configured ensemble method."""

    if ensemble == "mean":
        return simple_average(future_forecasts)
    if ensemble == "error_weighted":
        if validation_errors is None:
            raise ValueError("error_weighted ensemble requires validation errors.")
        return error_weighted_average(future_forecasts, validation_errors)
    if ensemble in {"ridge", "lasso"}:
        if validation_forecasts is None or validation_actual is None:
            raise ValueError(f"{ensemble} ensemble requires validation forecasts.")
        return positive_stacking_panel(
            validation_forecasts=validation_forecasts,
            validation_actual=validation_actual,
            future_forecasts=future_forecasts,
            method=ensemble,
            alpha=stacking_alpha,
        )
    raise ValueError(f"Unknown ensemble method: {ensemble}")


def _forecast_error(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
    metric: ForecastErrorMetric,
) -> float:
    actual_aligned, forecast_aligned = _align_matrices(actual, forecast)
    y = actual_aligned.to_numpy(dtype=float).ravel()
    y_hat = forecast_aligned.to_numpy(dtype=float).ravel()
    mask = np.isfinite(y) & np.isfinite(y_hat)
    y = y[mask]
    y_hat = y_hat[mask]
    if y.size == 0:
        raise ValueError("No overlapping non-missing validation observations.")

    error = y - y_hat
    if metric == "mae":
        return float(np.mean(np.abs(error)))
    if metric == "rmse":
        return float(np.sqrt(np.mean(error**2)))
    if metric == "mape":
        nonzero = y != 0
        if not np.any(nonzero):
            raise ValueError("MAPE validation error is undefined for all-zero actuals.")
        return float(np.mean(np.abs(error[nonzero] / y[nonzero])))
    if metric == "smape":
        denominator = np.abs(y) + np.abs(y_hat)
        nonzero = denominator != 0
        if not np.any(nonzero):
            raise ValueError("SMAPE validation error is undefined for all-zero pairs.")
        return float(np.mean(2 * np.abs(error[nonzero]) / denominator[nonzero]))
    raise ValueError("validation metric must be one of mae, rmse, mape, or smape.")


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


def _prepare_matrix(frame: pd.DataFrame) -> pd.DataFrame:
    matrix = frame.copy()
    matrix.index = pd.to_datetime(matrix.index)
    return matrix.sort_index().astype(float)


def _require_same_models(
    left: dict[str, pd.DataFrame],
    right: dict[str, pd.DataFrame],
) -> None:
    if tuple(left) != tuple(right):
        raise ValueError("Validation and future forecasts must use the same models.")
