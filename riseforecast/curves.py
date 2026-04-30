"""Recovery curve construction."""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from riseforecast.intervention import (
    DateLike,
    InterventionTerminalForecast,
    select_forecast_date,
)


@dataclass(frozen=True)
class CurveAnchors:
    """Initial and terminal anchors for a recovery path."""

    initial: float
    terminal: float
    periods: int

    def __post_init__(self) -> None:
        if self.periods < 2:
            raise ValueError("periods must be at least 2.")


def linear_curve(anchors: CurveAnchors) -> np.ndarray:
    """Straight-line recovery path including both anchors."""

    return np.linspace(anchors.initial, anchors.terminal, anchors.periods)


def quadratic_curve(
    anchors: CurveAnchors,
    history: pd.Series | None = None,
    terminal_weight: float = 18.0,
) -> np.ndarray:
    """Quadratic recovery path.

    If history is supplied, fit the curve to historical trend values and a weighted
    terminal anchor. Otherwise return a convex quadratic path between anchors.
    """

    if history is None or history.dropna().empty:
        x = np.linspace(0.0, 1.0, anchors.periods)
        return anchors.initial + (anchors.terminal - anchors.initial) * x**2

    y = history.dropna().to_numpy(dtype=float)
    x = np.arange(len(y), dtype=float)
    terminal_x = float(len(y) + anchors.periods - 1)
    x_fit = np.concatenate([x, np.repeat(terminal_x, int(terminal_weight))])
    y_fit = np.concatenate([y, np.repeat(anchors.terminal, int(terminal_weight))])
    coefficients = np.polyfit(x_fit, y_fit, deg=2)
    forecast_x = np.arange(len(y), len(y) + anchors.periods, dtype=float)
    curve = np.polyval(coefficients, forecast_x)
    curve[0] = anchors.initial
    return curve


def logistic_curve(
    anchors: CurveAnchors,
    midpoint: float | None = None,
    growth_rate: float = 0.8,
) -> np.ndarray:
    """Smooth S-shaped recovery path including both anchors."""

    if growth_rate <= 0:
        raise ValueError("growth_rate must be positive.")
    x = np.arange(anchors.periods, dtype=float)
    midpoint = (anchors.periods - 1) / 2 if midpoint is None else midpoint
    raw = 1 / (1 + np.exp(-growth_rate * (x - midpoint)))
    scaled = (raw - raw[0]) / (raw[-1] - raw[0])
    return anchors.initial + (anchors.terminal - anchors.initial) * scaled


def average_curves(curves: list[np.ndarray]) -> np.ndarray:
    """Average multiple recovery paths."""

    if not curves:
        raise ValueError("At least one curve is required.")
    lengths = {len(curve) for curve in curves}
    if len(lengths) != 1:
        raise ValueError("All curves must have the same length.")
    return np.vstack(curves).mean(axis=0)


def build_recovery_curve(
    initial: float,
    terminal: float,
    periods: int,
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic"),
) -> np.ndarray:
    """Build and average named recovery curves."""

    anchors = CurveAnchors(initial=initial, terminal=terminal, periods=periods)
    built: list[np.ndarray] = []
    for name in curve_names:
        if name == "linear":
            built.append(linear_curve(anchors))
        elif name == "quadratic":
            built.append(quadratic_curve(anchors))
        elif name == "logistic":
            built.append(logistic_curve(anchors))
        else:
            raise ValueError(f"Unknown curve name: {name}")
    return average_curves(built)


@dataclass(frozen=True)
class RecoveryCurveForecast:
    """Date-indexed recovery curve forecast for multiple entities.

    The named recovery curves are estimated on the trend component. The full
    forecasts in `values` are recovered from `recovery_curve` and
    `seasonal_components`.
    """

    values: pd.DataFrame
    components: dict[str, pd.DataFrame]
    initial_date: pd.Timestamp
    terminal_date: pd.Timestamp
    initial: pd.Series
    terminal: pd.Series
    recovery_curve: pd.DataFrame | None = None
    seasonal_components: pd.DataFrame | None = None
    trend_values: pd.DataFrame | None = None
    trend_components: dict[str, pd.DataFrame] = field(default_factory=dict)
    seasonal_multipliers: pd.DataFrame | None = None
    initial_trend: pd.Series | None = None
    terminal_trend: pd.Series | None = None

    def to_frame(self) -> pd.DataFrame:
        """Return a tidy long-form recovery forecast table."""

        frame = self.values.copy()
        frame.index.name = "date"
        return (
            frame.reset_index()
            .melt(id_vars="date", var_name="entity", value_name="forecast")
            .sort_values(["entity", "date"])
            .reset_index(drop=True)
        )


@dataclass(frozen=True)
class RecoveryCurveForecaster:
    """Build recovery paths between initial and terminal forecasts."""

    initial_date: DateLike
    forecast_start: DateLike
    forecast_end: DateLike | None = None
    frequency: str = "MS"
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic")

    def forecast(
        self,
        initial_forecast: pd.Series | pd.DataFrame,
        terminal_forecast: InterventionTerminalForecast | pd.Series,
        seasonal_multipliers: pd.Series | pd.DataFrame | None = None,
    ) -> RecoveryCurveForecast:
        """Generate a recovery curve forecast.

        `terminal_forecast` can be the object returned by
        `intervention_terminal_forecast`, which wires the recovery curve stage
        directly to the intervention-adjusted terminal stage.
        """

        initial_date = pd.Timestamp(self.initial_date)
        initial = _coerce_anchor_series(initial_forecast, initial_date)
        terminal, terminal_date = _coerce_terminal_series(
            terminal_forecast,
            forecast_end=self.forecast_end,
        )
        forecast_end = (
            pd.Timestamp(self.forecast_end)
            if self.forecast_end is not None
            else terminal_date
        )
        forecast_start = pd.Timestamp(self.forecast_start)
        _validate_dates(initial_date, forecast_start, forecast_end, terminal_date)

        entities = _shared_entities(initial, terminal)
        initial = initial.loc[entities].astype(float)
        terminal = terminal.loc[entities].astype(float)

        full_dates = pd.date_range(initial_date, terminal_date, freq=self.frequency)
        forecast_dates = pd.date_range(
            forecast_start,
            forecast_end,
            freq=self.frequency,
        )
        if not set(forecast_dates).issubset(set(full_dates)):
            raise ValueError(
                "Forecast dates must be within initial and terminal dates."
            )

        multiplier_frame = _seasonal_multiplier_frame(
            seasonal_multipliers=seasonal_multipliers,
            entities=entities,
            dates=full_dates,
        )
        initial_trend = initial / multiplier_frame.loc[initial_date]
        terminal_trend = terminal / multiplier_frame.loc[terminal_date]

        trend_component_frames = self._build_trend_component_frames(
            initial_trend=initial_trend,
            terminal_trend=terminal_trend,
            dates=full_dates,
        )
        trend_values = sum(trend_component_frames.values()) / len(
            trend_component_frames
        )
        component_frames = {
            name: recover_full_forecast(frame, multiplier_frame)
            for name, frame in trend_component_frames.items()
        }
        values = recover_full_forecast(trend_values, multiplier_frame).loc[
            forecast_dates
        ]
        trend_values = trend_values.loc[forecast_dates]
        seasonal_components = multiplier_frame.loc[forecast_dates]
        components = {
            name: frame.loc[forecast_dates] for name, frame in component_frames.items()
        }
        trend_components = {
            name: frame.loc[forecast_dates]
            for name, frame in trend_component_frames.items()
        }
        return RecoveryCurveForecast(
            values=values,
            components=components,
            initial_date=initial_date,
            terminal_date=terminal_date,
            initial=initial,
            terminal=terminal,
            recovery_curve=trend_values,
            seasonal_components=seasonal_components,
            trend_values=trend_values,
            trend_components=trend_components,
            seasonal_multipliers=seasonal_components,
            initial_trend=initial_trend.rename("initial_trend"),
            terminal_trend=terminal_trend.rename("terminal_trend"),
        )

    def _build_trend_component_frames(
        self,
        initial_trend: pd.Series,
        terminal_trend: pd.Series,
        dates: pd.DatetimeIndex,
    ) -> dict[str, pd.DataFrame]:
        components: dict[str, pd.DataFrame] = {}
        for curve_name in self.curve_names:
            columns = {}
            for entity in initial_trend.index:
                path = _build_entity_trend_path(
                    curve_name=curve_name,
                    initial=float(initial_trend.loc[entity]),
                    terminal=float(terminal_trend.loc[entity]),
                    periods=len(dates),
                )
                columns[entity] = path
            components[curve_name] = pd.DataFrame(columns, index=dates)
        if not components:
            raise ValueError("At least one curve name is required.")
        return components


def recovery_curve_forecast(
    initial_forecast: pd.Series | pd.DataFrame,
    terminal_forecast: InterventionTerminalForecast | pd.Series,
    initial_date: DateLike,
    forecast_start: DateLike,
    forecast_end: DateLike | None = None,
    frequency: str = "MS",
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic"),
    seasonal_multipliers: pd.Series | pd.DataFrame | None = None,
) -> RecoveryCurveForecast:
    """Convenience wrapper for recovery curve forecasts."""

    forecaster = RecoveryCurveForecaster(
        initial_date=initial_date,
        forecast_start=forecast_start,
        forecast_end=forecast_end,
        frequency=frequency,
        curve_names=curve_names,
    )
    return forecaster.forecast(
        initial_forecast=initial_forecast,
        terminal_forecast=terminal_forecast,
        seasonal_multipliers=seasonal_multipliers,
    )


def recover_full_forecast(
    recovery_curve: pd.DataFrame,
    seasonal_components: pd.Series | pd.DataFrame | None = None,
) -> pd.DataFrame:
    """Recover full forecasts from a trend recovery curve and seasonality."""

    curve = _prepare_matrix(recovery_curve)
    components = _coerce_seasonal_components(
        seasonal_components=seasonal_components,
        entities=pd.Index(curve.columns),
        dates=pd.DatetimeIndex(curve.index),
    )
    return curve * components


def _build_entity_trend_path(
    curve_name: str,
    initial: float,
    terminal: float,
    periods: int,
) -> np.ndarray:
    anchors = CurveAnchors(initial=initial, terminal=terminal, periods=periods)

    if curve_name == "linear":
        return linear_curve(anchors)
    if curve_name == "quadratic":
        return quadratic_curve(anchors)
    if curve_name == "logistic":
        return logistic_curve(anchors)
    raise ValueError(f"Unknown curve name: {curve_name}")


def _seasonal_multiplier_frame(
    seasonal_multipliers: pd.Series | pd.DataFrame | None,
    entities: pd.Index,
    dates: pd.DatetimeIndex,
) -> pd.DataFrame:
    if seasonal_multipliers is None:
        return pd.DataFrame(1.0, index=dates, columns=entities)
    columns = {}
    for entity in entities:
        columns[entity] = [
            _seasonal_multiplier(
                seasonal_multipliers,
                entity=entity,
                month=int(date.month),
            )
            for date in dates
        ]
    return pd.DataFrame(columns, index=dates, dtype=float)


def _coerce_seasonal_components(
    seasonal_components: pd.Series | pd.DataFrame | None,
    entities: pd.Index,
    dates: pd.DatetimeIndex,
) -> pd.DataFrame:
    if seasonal_components is None:
        return pd.DataFrame(1.0, index=dates, columns=entities)
    if isinstance(seasonal_components, pd.Series):
        return _seasonal_multiplier_frame(seasonal_components, entities, dates)

    frame = seasonal_components.copy()
    if _is_month_index(frame.index):
        return _seasonal_multiplier_frame(frame, entities, dates)

    frame.index = pd.to_datetime(frame.index)
    frame = frame.sort_index().reindex(index=dates, columns=entities)
    if frame.isna().any().any():
        raise ValueError(
            "Seasonal components must cover all recovery curve dates and entities."
        )
    return frame.astype(float)


def _prepare_matrix(matrix: pd.DataFrame) -> pd.DataFrame:
    result = matrix.copy()
    result.index = pd.to_datetime(result.index)
    return result.sort_index().astype(float)


def _is_month_index(index: pd.Index) -> bool:
    if isinstance(index, pd.DatetimeIndex):
        return False
    try:
        months = pd.Index(index).astype(int)
    except (TypeError, ValueError):
        return False
    return set(months).issubset(set(range(1, 13)))


def _coerce_anchor_series(
    forecast: pd.Series | pd.DataFrame,
    date: pd.Timestamp,
) -> pd.Series:
    if isinstance(forecast, pd.Series):
        return forecast.astype(float)
    return select_forecast_date(forecast, date)


def _coerce_terminal_series(
    terminal_forecast: InterventionTerminalForecast | pd.Series,
    forecast_end: DateLike | None,
) -> tuple[pd.Series, pd.Timestamp]:
    if isinstance(terminal_forecast, InterventionTerminalForecast):
        return terminal_forecast.values.astype(float), terminal_forecast.terminal_date
    if forecast_end is None:
        raise ValueError("forecast_end is required when terminal_forecast is a Series.")
    return terminal_forecast.astype(float), pd.Timestamp(forecast_end)


def _shared_entities(initial: pd.Series, terminal: pd.Series) -> pd.Index:
    missing = [entity for entity in initial.index if entity not in terminal.index]
    if missing:
        joined = ", ".join(str(entity) for entity in missing)
        raise ValueError(f"Missing terminal forecasts for entities: {joined}")
    return pd.Index(initial.index)


def _validate_dates(
    initial_date: pd.Timestamp,
    forecast_start: pd.Timestamp,
    forecast_end: pd.Timestamp,
    terminal_date: pd.Timestamp,
) -> None:
    if not initial_date < forecast_start:
        raise ValueError("initial_date must be earlier than forecast_start.")
    if forecast_start > forecast_end:
        raise ValueError(
            "forecast_start must be earlier than or equal to forecast_end."
        )
    if forecast_end > terminal_date:
        raise ValueError("forecast_end cannot be later than terminal_date.")


def _seasonal_multiplier(
    seasonal_multipliers: pd.Series | pd.DataFrame | None,
    entity: object,
    month: int,
) -> float:
    if seasonal_multipliers is None:
        return 1.0
    if isinstance(seasonal_multipliers, pd.Series):
        multiplier = seasonal_multipliers.loc[month]
    else:
        multiplier = seasonal_multipliers.loc[month, entity]
    multiplier = float(multiplier)
    if multiplier == 0:
        raise ValueError("Seasonal multipliers cannot contain zero values.")
    return multiplier
