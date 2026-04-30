"""Forecast combination utilities."""

from __future__ import annotations

import numpy as np
import pandas as pd


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
    if (selected_errors <= 0).any():
        raise ValueError("All errors must be positive.")
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
    model.fit(train_forecasts.to_numpy(dtype=float), train_actual.to_numpy(dtype=float))
    return model.predict(future_forecasts.to_numpy(dtype=float))
