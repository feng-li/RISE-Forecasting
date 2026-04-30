"""Model-based initial recovery forecasts.

The initial forecast is the near-term anchor used by the recovery curve stage. This
module provides a fallback path when no external signal model is available: fit the
standard base-model pool to the latest observed target data and forecast forward to
the configured initial date.
"""

from __future__ import annotations

from dataclasses import dataclass

import pandas as pd

from riseforecast.base_models import (
    BaseForecast,
    ModelRegistry,
    default_model_registry,
    forecast_panel,
)
from riseforecast.data import ForecastFrame
from riseforecast.ensembles import simple_average
from riseforecast.intervention import DateLike, select_forecast_date


@dataclass(frozen=True)
class InitialForecast:
    """Forecast path ending at the initial recovery anchor."""

    values: pd.DataFrame
    initial_date: pd.Timestamp
    train_end: pd.Timestamp
    model_name: str
    components: dict[str, BaseForecast]

    @property
    def initial(self) -> pd.Series:
        """Return the forecast row used as the recovery curve's initial anchor."""

        return select_forecast_date(self.values, self.initial_date).rename(
            "initial_forecast"
        )

    def as_forecast_frame(self) -> ForecastFrame:
        """Return the combined forecast path as a `ForecastFrame`."""

        return ForecastFrame(values=self.values)

    def to_frame(self) -> pd.DataFrame:
        """Return a tidy long-form initial forecast path."""

        frame = self.values.copy()
        frame.index.name = "date"
        return (
            frame.reset_index()
            .melt(id_vars="date", var_name="entity", value_name="forecast")
            .sort_values(["entity", "date"])
            .reset_index(drop=True)
        )


@dataclass(frozen=True)
class InitialForecaster:
    """Generate the initial recovery anchor using registered base models."""

    initial_date: DateLike
    train_end: DateLike | None = None
    models: tuple[str, ...] = (
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
    )
    ensemble: str = "mean"
    frequency: str = "MS"
    seasonal_period: int = 12
    registry: ModelRegistry | None = None

    def forecast(self, observed: pd.DataFrame) -> InitialForecast:
        """Forecast observed targets forward to `initial_date`."""

        if not self.models:
            raise ValueError("At least one base model is required.")
        if self.ensemble != "mean":
            raise ValueError("Only mean initial forecast ensembles are implemented.")

        panel = _prepare_observed_panel(observed)
        train_end = _resolve_train_end(panel, self.train_end)
        forecast_dates = _forecast_dates(
            train_end=train_end,
            initial_date=pd.Timestamp(self.initial_date),
            frequency=self.frequency,
        )
        registry = (
            default_model_registry(period=self.seasonal_period)
            if self.registry is None
            else self.registry
        )
        components = forecast_panel(
            observed=panel,
            models=self.models,
            horizon=len(forecast_dates),
            train_end=train_end,
            frequency=self.frequency,
            registry=registry,
        )
        values = simple_average(
            {name: forecast.values for name, forecast in components.items()}
        )
        values = values.reindex(index=forecast_dates, columns=panel.columns)
        model_name = f"mean({'+'.join(components)})"
        return InitialForecast(
            values=values,
            initial_date=pd.Timestamp(self.initial_date),
            train_end=train_end,
            model_name=model_name,
            components=components,
        )


def initial_forecast(
    observed: pd.DataFrame,
    initial_date: DateLike,
    train_end: DateLike | None = None,
    models: tuple[str, ...] = (
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
    ),
    ensemble: str = "mean",
    frequency: str = "MS",
    seasonal_period: int = 12,
    registry: ModelRegistry | None = None,
) -> InitialForecast:
    """Convenience wrapper for model-based initial recovery forecasts."""

    forecaster = InitialForecaster(
        initial_date=initial_date,
        train_end=train_end,
        models=models,
        ensemble=ensemble,
        frequency=frequency,
        seasonal_period=seasonal_period,
        registry=registry,
    )
    return forecaster.forecast(observed)


def _prepare_observed_panel(observed: pd.DataFrame) -> pd.DataFrame:
    if observed.empty:
        raise ValueError("observed panel is empty.")
    panel = observed.copy()
    panel.index = pd.to_datetime(panel.index)
    return panel.sort_index()


def _resolve_train_end(
    observed: pd.DataFrame,
    train_end: DateLike | None,
) -> pd.Timestamp:
    cutoff = observed.index.max() if train_end is None else pd.Timestamp(train_end)
    available = observed.loc[:cutoff]
    available = available.loc[available.notna().any(axis=1)]
    if available.empty:
        raise ValueError("No observed rows are available at or before train_end.")
    return pd.Timestamp(available.index[-1])


def _forecast_dates(
    train_end: pd.Timestamp,
    initial_date: pd.Timestamp,
    frequency: str,
) -> pd.DatetimeIndex:
    offset = pd.tseries.frequencies.to_offset(frequency)
    start = pd.Timestamp(train_end) + offset
    dates = pd.date_range(start=start, end=initial_date, freq=frequency)
    if len(dates) == 0 or dates[-1] != initial_date:
        raise ValueError(
            "initial_date must be after train_end and aligned to the forecast "
            "frequency."
        )
    return dates
