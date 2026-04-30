"""External-signal reference forecast methods."""

from __future__ import annotations

from dataclasses import dataclass

import pandas as pd


@dataclass(frozen=True)
class ReferenceForecast:
    """Reference forecast values for the early recovery stage."""

    values: pd.DataFrame
    method: str


def growth_rate_reference_forecast(
    latest_observed: pd.Series,
    signal: pd.DataFrame,
    baseline_signal: pd.Series,
) -> pd.DataFrame:
    """Forecast target values from signal growth relative to a baseline signal."""

    missing = [
        entity for entity in latest_observed.index if entity not in signal.columns
    ]
    if missing:
        raise ValueError(f"Missing signal columns for entities: {', '.join(missing)}")
    signal = signal.loc[:, latest_observed.index]
    baseline = baseline_signal.loc[latest_observed.index].replace(0, pd.NA)
    growth = signal.divide(baseline, axis="columns")
    return growth.multiply(latest_observed, axis="columns")


def ratio_signal_forecast(
    signal: pd.DataFrame,
    ratio_forecast: pd.DataFrame | pd.Series,
) -> pd.DataFrame:
    """Forecast target values as external signal multiplied by target/signal ratio."""

    if isinstance(ratio_forecast, pd.Series):
        return signal.multiply(ratio_forecast, axis="columns")
    return signal * ratio_forecast


def combine_reference_forecasts(
    forecasts: list[ReferenceForecast],
) -> ReferenceForecast:
    """Average multiple reference forecasts."""

    if not forecasts:
        raise ValueError("At least one reference forecast is required.")
    values = sum(item.values for item in forecasts) / len(forecasts)
    method = "+".join(item.method for item in forecasts)
    return ReferenceForecast(values=values, method=f"mean({method})")
