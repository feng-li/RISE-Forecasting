"""Recovery curve construction."""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from riseforecast.data import ForecastFrame
from riseforecast.intervals import order_interval_bounds
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
    trend_history: pd.DataFrame | None = None
    lower: pd.DataFrame | None = None
    upper: pd.DataFrame | None = None
    lower_recovery_curve: pd.DataFrame | None = None
    upper_recovery_curve: pd.DataFrame | None = None

    def to_frame(self) -> pd.DataFrame:
        """Return a tidy long-form recovery forecast table."""

        result = _melt_forecast_matrix(self.values, value_name="forecast")
        if self.lower is not None:
            result = result.merge(
                _melt_forecast_matrix(self.lower, value_name="lower"),
                on=["date", "entity"],
                how="left",
            )
        if self.upper is not None:
            result = result.merge(
                _melt_forecast_matrix(self.upper, value_name="upper"),
                on=["date", "entity"],
                how="left",
            )
        return result.sort_values(["entity", "date"]).reset_index(drop=True)


@dataclass(frozen=True)
class RecoveryCurveForecaster:
    """Build recovery paths between initial and terminal forecasts."""

    initial_date: DateLike
    forecast_start: DateLike
    forecast_end: DateLike | None = None
    frequency: str = "MS"
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic")
    quadratic_terminal_weight: float = 18.0
    logistic_anchor_dates: tuple[DateLike, ...] = ()

    def forecast(
        self,
        initial_forecast: ForecastFrame | pd.Series | pd.DataFrame,
        terminal_forecast: InterventionTerminalForecast | pd.Series,
        seasonal_multipliers: pd.Series | pd.DataFrame | None = None,
        trend_history: pd.DataFrame | None = None,
        base_forecast: pd.DataFrame | None = None,
    ) -> RecoveryCurveForecast:
        """Generate a recovery curve forecast.

        `terminal_forecast` can be the object returned by
        `intervention_terminal_forecast`, which wires the recovery curve stage
        directly to the intervention-adjusted terminal stage.
        """

        initial_date = pd.Timestamp(self.initial_date)
        initial = _coerce_anchor_series(initial_forecast, initial_date)
        initial_lower = _coerce_anchor_bound(initial_forecast, initial_date, "lower")
        initial_upper = _coerce_anchor_bound(initial_forecast, initial_date, "upper")
        terminal, terminal_date = _coerce_terminal_series(
            terminal_forecast,
            forecast_end=self.forecast_end,
        )
        terminal_lower = _coerce_terminal_bound(terminal_forecast, "lower")
        terminal_upper = _coerce_terminal_bound(terminal_forecast, "upper")
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
        interval_anchors = _coerce_interval_anchors(
            initial=initial,
            terminal=terminal,
            initial_lower=initial_lower,
            initial_upper=initial_upper,
            terminal_lower=terminal_lower,
            terminal_upper=terminal_upper,
            entities=entities,
        )

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
        trend_history = _prepare_optional_trend_history(
            trend_history=trend_history,
            entities=entities,
        )
        base_trend_forecast = _base_trend_forecast(
            base_forecast=base_forecast,
            seasonal_multipliers=seasonal_multipliers,
            entities=entities,
        )

        trend_component_frames = self._build_trend_component_frames(
            initial_trend=initial_trend,
            terminal_trend=terminal_trend,
            dates=full_dates,
            trend_history=trend_history,
            base_trend_forecast=base_trend_forecast,
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

        lower = None
        upper = None
        lower_recovery_curve = None
        upper_recovery_curve = None
        if interval_anchors is not None:
            lower_initial, upper_initial, lower_terminal, upper_terminal = (
                interval_anchors
            )
            lower_trend_components = self._build_trend_component_frames(
                initial_trend=lower_initial / multiplier_frame.loc[initial_date],
                terminal_trend=lower_terminal / multiplier_frame.loc[terminal_date],
                dates=full_dates,
                trend_history=trend_history,
                base_trend_forecast=base_trend_forecast,
            )
            upper_trend_components = self._build_trend_component_frames(
                initial_trend=upper_initial / multiplier_frame.loc[initial_date],
                terminal_trend=upper_terminal / multiplier_frame.loc[terminal_date],
                dates=full_dates,
                trend_history=trend_history,
                base_trend_forecast=base_trend_forecast,
            )
            lower_recovery_curve = (
                sum(lower_trend_components.values()) / len(lower_trend_components)
            ).loc[forecast_dates]
            upper_recovery_curve = (
                sum(upper_trend_components.values()) / len(upper_trend_components)
            ).loc[forecast_dates]
            lower_recovery_curve, upper_recovery_curve = order_interval_bounds(
                lower_recovery_curve,
                upper_recovery_curve,
                values=trend_values,
            )
            lower_full = recover_full_forecast(
                lower_recovery_curve,
                seasonal_components,
            )
            upper_full = recover_full_forecast(
                upper_recovery_curve,
                seasonal_components,
            )
            lower, upper = order_interval_bounds(
                lower_full,
                upper_full,
                values=values,
            )

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
            trend_history=trend_history,
            lower=lower,
            upper=upper,
            lower_recovery_curve=lower_recovery_curve,
            upper_recovery_curve=upper_recovery_curve,
        )

    def _build_trend_component_frames(
        self,
        initial_trend: pd.Series,
        terminal_trend: pd.Series,
        dates: pd.DatetimeIndex,
        trend_history: pd.DataFrame | None,
        base_trend_forecast: pd.DataFrame | None,
    ) -> dict[str, pd.DataFrame]:
        components: dict[str, pd.DataFrame] = {}
        for curve_name in self.curve_names:
            columns = {}
            for entity in initial_trend.index:
                path = _build_entity_trend_path(
                    curve_name=curve_name,
                    initial=float(initial_trend.loc[entity]),
                    terminal=float(terminal_trend.loc[entity]),
                    dates=dates,
                    history=(
                        None
                        if trend_history is None
                        else trend_history.loc[:, entity].dropna()
                    ),
                    base_trend_forecast=(
                        None
                        if base_trend_forecast is None
                        else base_trend_forecast.loc[:, entity].dropna()
                    ),
                    quadratic_terminal_weight=self.quadratic_terminal_weight,
                    logistic_anchor_dates=self.logistic_anchor_dates,
                )
                columns[entity] = path
            components[curve_name] = pd.DataFrame(columns, index=dates)
        if not components:
            raise ValueError("At least one curve name is required.")
        return components


def recovery_curve_forecast(
    initial_forecast: ForecastFrame | pd.Series | pd.DataFrame,
    terminal_forecast: InterventionTerminalForecast | pd.Series,
    initial_date: DateLike,
    forecast_start: DateLike,
    forecast_end: DateLike | None = None,
    frequency: str = "MS",
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic"),
    seasonal_multipliers: pd.Series | pd.DataFrame | None = None,
    trend_history: pd.DataFrame | None = None,
    base_forecast: pd.DataFrame | None = None,
    quadratic_terminal_weight: float = 18.0,
    logistic_anchor_dates: tuple[DateLike, ...] = (),
) -> RecoveryCurveForecast:
    """Convenience wrapper for recovery curve forecasts."""

    forecaster = RecoveryCurveForecaster(
        initial_date=initial_date,
        forecast_start=forecast_start,
        forecast_end=forecast_end,
        frequency=frequency,
        curve_names=curve_names,
        quadratic_terminal_weight=quadratic_terminal_weight,
        logistic_anchor_dates=logistic_anchor_dates,
    )
    return forecaster.forecast(
        initial_forecast=initial_forecast,
        terminal_forecast=terminal_forecast,
        seasonal_multipliers=seasonal_multipliers,
        trend_history=trend_history,
        base_forecast=base_forecast,
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


def extract_trend_component(
    values: pd.DataFrame,
    seasonal_components: pd.Series | pd.DataFrame | None = None,
) -> pd.DataFrame:
    """Remove seasonality from full values to obtain trend components."""

    matrix = _prepare_matrix(values)
    components = _coerce_seasonal_components(
        seasonal_components=seasonal_components,
        entities=pd.Index(matrix.columns),
        dates=pd.DatetimeIndex(matrix.index),
    )
    return matrix / components


def _build_entity_trend_path(
    curve_name: str,
    initial: float,
    terminal: float,
    dates: pd.DatetimeIndex,
    history: pd.Series | None,
    base_trend_forecast: pd.Series | None,
    quadratic_terminal_weight: float,
    logistic_anchor_dates: tuple[DateLike, ...],
) -> np.ndarray:
    anchors = CurveAnchors(initial=initial, terminal=terminal, periods=len(dates))

    if curve_name == "linear":
        return linear_curve(anchors)
    if curve_name == "quadratic":
        return quadratic_curve(
            anchors,
            history=history,
            terminal_weight=quadratic_terminal_weight,
        )
    if curve_name == "logistic":
        return paper_logistic_curve(
            anchors=anchors,
            dates=dates,
            history=history,
            base_trend_forecast=base_trend_forecast,
            anchor_dates=logistic_anchor_dates,
        )
    raise ValueError(f"Unknown curve name: {curve_name}")


def paper_logistic_curve(
    anchors: CurveAnchors,
    dates: pd.DatetimeIndex,
    history: pd.Series | None = None,
    base_trend_forecast: pd.Series | None = None,
    anchor_dates: tuple[DateLike, ...] = (),
) -> np.ndarray:
    """Fit a logistic trend curve to paper-style critical trend points."""

    points = _logistic_fit_points(
        initial=anchors.initial,
        dates=dates,
        history=history,
        base_trend_forecast=base_trend_forecast,
        anchor_dates=anchor_dates,
    )
    if points is None:
        return logistic_curve(anchors)

    x, y, forecast_x = points
    fitted = _fit_logistic_to_points(x=x, y=y, forecast_x=forecast_x)
    if fitted is None:
        return logistic_curve(anchors)
    return fitted


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


def _prepare_optional_trend_history(
    trend_history: pd.DataFrame | None,
    entities: pd.Index,
) -> pd.DataFrame | None:
    if trend_history is None:
        return None
    history = _prepare_matrix(trend_history).reindex(columns=entities)
    if history.dropna(how="all").empty:
        return None
    return history


def _base_trend_forecast(
    base_forecast: pd.DataFrame | None,
    seasonal_multipliers: pd.Series | pd.DataFrame | None,
    entities: pd.Index,
) -> pd.DataFrame | None:
    if base_forecast is None:
        return None
    base = _prepare_matrix(base_forecast).reindex(columns=entities)
    if base.dropna(how="all").empty:
        return None
    return extract_trend_component(base, seasonal_multipliers)


def _logistic_fit_points(
    initial: float,
    dates: pd.DatetimeIndex,
    history: pd.Series | None,
    base_trend_forecast: pd.Series | None,
    anchor_dates: tuple[DateLike, ...],
) -> tuple[np.ndarray, np.ndarray, np.ndarray] | None:
    points: list[tuple[pd.Timestamp, float]] = []
    if history is not None and not history.dropna().empty:
        history_values = history.dropna().astype(float)
        points.append(
            (pd.Timestamp(history_values.index[0]), float(history_values.iloc[0]))
        )

    points.append((pd.Timestamp(dates[0]), initial))

    if base_trend_forecast is not None:
        base = base_trend_forecast.dropna().astype(float)
        for date in anchor_dates:
            timestamp = pd.Timestamp(date)
            if timestamp in base.index:
                points.append((timestamp, float(base.loc[timestamp])))

    points = _unique_positive_fit_points(points)
    if len(points) < 3:
        return None

    origin = min(date for date, _ in points)
    x = np.array([_month_distance(origin, date) for date, _ in points], dtype=float)
    y = np.array([value for _, value in points], dtype=float)
    forecast_x = np.array(
        [_month_distance(origin, pd.Timestamp(date)) for date in dates],
        dtype=float,
    )
    return x, y, forecast_x


def _unique_positive_fit_points(
    points: list[tuple[pd.Timestamp, float]],
) -> list[tuple[pd.Timestamp, float]]:
    unique: dict[pd.Timestamp, float] = {}
    for date, value in points:
        if np.isfinite(value) and value > 0:
            unique[pd.Timestamp(date)] = float(value)
    return sorted(unique.items(), key=lambda item: item[0])


def _fit_logistic_to_points(
    x: np.ndarray,
    y: np.ndarray,
    forecast_x: np.ndarray,
) -> np.ndarray | None:
    if len(x) < 3 or len(np.unique(x)) < 3 or (y <= 0).any():
        return None

    from scipy.optimize import curve_fit

    def logistic(x_values: np.ndarray, limit: float, growth: float, midpoint: float):
        return limit / (1 + np.exp(-growth * (x_values - midpoint)))

    max_y = float(np.max(y))
    span = max(float(np.max(x) - np.min(x)), 1.0)
    initial = [max_y * 1.2, 0.2, float(np.median(x))]
    bounds = (
        [max_y, 1e-5, float(np.min(x) - 2 * span)],
        [max_y * 100.0, 5.0, float(np.max(x) + 2 * span)],
    )
    try:
        params, _ = curve_fit(
            logistic,
            x,
            y,
            p0=initial,
            bounds=bounds,
            maxfev=20000,
        )
    except (RuntimeError, ValueError, FloatingPointError):
        return None
    fitted = logistic(forecast_x, *params)
    if not np.isfinite(fitted).all():
        return None
    return fitted


def _month_distance(origin: pd.Timestamp, date: pd.Timestamp) -> int:
    return (date.year - origin.year) * 12 + (date.month - origin.month)


def _coerce_anchor_series(
    forecast: ForecastFrame | pd.Series | pd.DataFrame,
    date: pd.Timestamp,
) -> pd.Series:
    if isinstance(forecast, ForecastFrame):
        forecast = forecast.values
    if isinstance(forecast, pd.Series):
        return forecast.astype(float)
    return select_forecast_date(forecast, date)


def _coerce_anchor_bound(
    forecast: ForecastFrame | pd.Series | pd.DataFrame,
    date: pd.Timestamp,
    bound_name: str,
) -> pd.Series | None:
    if not isinstance(forecast, ForecastFrame):
        return None
    bound = getattr(forecast, bound_name)
    if bound is None:
        return None
    return select_forecast_date(bound, date)


def _coerce_terminal_series(
    terminal_forecast: InterventionTerminalForecast | pd.Series,
    forecast_end: DateLike | None,
) -> tuple[pd.Series, pd.Timestamp]:
    if isinstance(terminal_forecast, InterventionTerminalForecast):
        return terminal_forecast.values.astype(float), terminal_forecast.terminal_date
    if forecast_end is None:
        raise ValueError(
            "forecast_end is required when terminal_forecast is a Series."
        )
    return terminal_forecast.astype(float), pd.Timestamp(forecast_end)


def _coerce_terminal_bound(
    terminal_forecast: InterventionTerminalForecast | pd.Series,
    bound_name: str,
) -> pd.Series | None:
    if not isinstance(terminal_forecast, InterventionTerminalForecast):
        return None
    bound = getattr(terminal_forecast, bound_name)
    if bound is None:
        return None
    return bound.astype(float)


def _coerce_interval_anchors(
    initial: pd.Series,
    terminal: pd.Series,
    initial_lower: pd.Series | None,
    initial_upper: pd.Series | None,
    terminal_lower: pd.Series | None,
    terminal_upper: pd.Series | None,
    entities: pd.Index,
) -> tuple[pd.Series, pd.Series, pd.Series, pd.Series] | None:
    if all(
        bound is None
        for bound in (initial_lower, initial_upper, terminal_lower, terminal_upper)
    ):
        return None

    lower_initial = _align_bound_or_point(
        initial_lower,
        fallback=initial,
        entities=entities,
        label="initial lower",
    )
    upper_initial = _align_bound_or_point(
        initial_upper,
        fallback=initial,
        entities=entities,
        label="initial upper",
    )
    lower_terminal = _align_bound_or_point(
        terminal_lower,
        fallback=terminal,
        entities=entities,
        label="terminal lower",
    )
    upper_terminal = _align_bound_or_point(
        terminal_upper,
        fallback=terminal,
        entities=entities,
        label="terminal upper",
    )
    lower_initial, upper_initial = _order_interval_series(
        lower_initial,
        upper_initial,
        values=initial,
    )
    lower_terminal, upper_terminal = _order_interval_series(
        lower_terminal,
        upper_terminal,
        values=terminal,
    )
    return lower_initial, upper_initial, lower_terminal, upper_terminal


def _align_bound_or_point(
    bound: pd.Series | None,
    fallback: pd.Series,
    entities: pd.Index,
    label: str,
) -> pd.Series:
    if bound is None:
        return fallback.loc[entities].astype(float)
    missing = [entity for entity in entities if entity not in bound.index]
    if missing:
        joined = ", ".join(str(entity) for entity in missing)
        raise ValueError(f"Missing {label} interval bounds for entities: {joined}")
    return bound.loc[entities].astype(float)


def _order_interval_series(
    lower: pd.Series,
    upper: pd.Series,
    values: pd.Series,
) -> tuple[pd.Series, pd.Series]:
    frame = pd.concat(
        [
            lower.rename("lower"),
            upper.rename("upper"),
            values.rename("value"),
        ],
        axis=1,
    )
    ordered_lower = frame.min(axis=1).rename(lower.name)
    ordered_upper = frame.max(axis=1).rename(upper.name)
    return ordered_lower, ordered_upper


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


def _melt_forecast_matrix(matrix: pd.DataFrame, value_name: str) -> pd.DataFrame:
    frame = matrix.copy()
    frame.index.name = "date"
    return frame.reset_index().melt(
        id_vars="date",
        var_name="entity",
        value_name=value_name,
    )
