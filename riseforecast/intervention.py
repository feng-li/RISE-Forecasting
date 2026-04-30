"""Intervention-adjusted terminal forecasts.

The terminal forecast stage takes a no-shock counterfactual forecast and applies
entity-specific intervention coefficients. In the RISE paper these coefficients
represent incomplete post-COVID recovery, but the implementation is domain-neutral:
any multiplicative intervention factor can be supplied.
"""

from __future__ import annotations

from dataclasses import dataclass

import pandas as pd

from riseforecast.data import ForecastFrame

DateLike = str | pd.Timestamp
CoefficientBounds = tuple[float | None, float | None] | None


@dataclass(frozen=True)
class InterventionTerminalForecast:
    """Terminal endpoint after applying intervention coefficients."""

    terminal_date: pd.Timestamp
    values: pd.Series
    baseline: pd.Series
    coefficients: pd.Series
    lower: pd.Series | None = None
    upper: pd.Series | None = None

    def to_frame(self) -> pd.DataFrame:
        """Return a tidy terminal forecast table."""

        result = pd.DataFrame(
            {
                "entity": self.values.index,
                "terminal_date": self.terminal_date,
                "baseline": self.baseline.to_numpy(dtype=float),
                "coefficient": self.coefficients.to_numpy(dtype=float),
                "forecast": self.values.to_numpy(dtype=float),
            }
        )
        if self.lower is not None:
            result["lower"] = self.lower.to_numpy(dtype=float)
        if self.upper is not None:
            result["upper"] = self.upper.to_numpy(dtype=float)
        return result


@dataclass(frozen=True)
class InterventionTerminalForecaster:
    """Compute terminal forecasts from counterfactual forecasts and coefficients."""

    terminal_date: DateLike
    coefficient_bounds: CoefficientBounds = (0.0, 1.0)

    def forecast(
        self,
        base_forecast: ForecastFrame | pd.DataFrame,
        coefficients: pd.Series,
    ) -> InterventionTerminalForecast:
        """Apply intervention coefficients to the terminal baseline row."""

        forecast_frame = _coerce_forecast_frame(base_forecast)
        terminal_date = pd.Timestamp(self.terminal_date)
        baseline = select_forecast_date(forecast_frame.values, terminal_date)
        aligned_coefficients = align_coefficients(
            coefficients,
            baseline.index,
            bounds=self.coefficient_bounds,
        )
        values = baseline * aligned_coefficients

        lower = None
        upper = None
        if forecast_frame.lower is not None:
            lower = select_forecast_date(forecast_frame.lower, terminal_date)
        if forecast_frame.upper is not None:
            upper = select_forecast_date(forecast_frame.upper, terminal_date)
        if lower is not None and upper is not None:
            lower, upper = adjust_interval(lower, upper, aligned_coefficients)
        elif lower is not None:
            lower = lower * aligned_coefficients
        elif upper is not None:
            upper = upper * aligned_coefficients

        return InterventionTerminalForecast(
            terminal_date=terminal_date,
            values=values.rename("terminal_forecast"),
            baseline=baseline.rename("baseline"),
            coefficients=aligned_coefficients.rename("coefficient"),
            lower=None if lower is None else lower.rename("lower"),
            upper=None if upper is None else upper.rename("upper"),
        )


def intervention_terminal_forecast(
    base_forecast: ForecastFrame | pd.DataFrame,
    coefficients: pd.Series,
    terminal_date: DateLike,
    coefficient_bounds: CoefficientBounds = (0.0, 1.0),
) -> InterventionTerminalForecast:
    """Convenience wrapper for intervention-adjusted terminal forecasts."""

    forecaster = InterventionTerminalForecaster(
        terminal_date=terminal_date,
        coefficient_bounds=coefficient_bounds,
    )
    return forecaster.forecast(base_forecast, coefficients)


def select_forecast_date(forecast: pd.DataFrame, date: DateLike) -> pd.Series:
    """Select one forecast row by date with a clear error on mismatch."""

    matrix = forecast.copy()
    matrix.index = pd.to_datetime(matrix.index)
    matrix = matrix.sort_index()
    timestamp = pd.Timestamp(date)
    if timestamp not in matrix.index:
        start = matrix.index.min()
        end = matrix.index.max()
        raise ValueError(
            f"Forecast date {timestamp.date()} is not available. "
            f"Available range is {start.date()} to {end.date()}."
        )
    row = matrix.loc[timestamp]
    if isinstance(row, pd.DataFrame):
        raise ValueError(f"Forecast date {timestamp.date()} is not unique.")
    return row.astype(float)


def align_coefficients(
    coefficients: pd.Series,
    entities: pd.Index,
    bounds: CoefficientBounds = (0.0, 1.0),
) -> pd.Series:
    """Align intervention coefficients to entities and optionally clip them."""

    missing = [entity for entity in entities if entity not in coefficients.index]
    if missing:
        raise ValueError(f"Missing coefficients for entities: {', '.join(missing)}")
    aligned = coefficients.loc[entities].astype(float)
    if bounds is not None:
        lower, upper = bounds
        aligned = aligned.clip(lower=lower, upper=upper)
    return aligned


def adjust_interval(
    lower: pd.Series,
    upper: pd.Series,
    coefficients: pd.Series,
) -> tuple[pd.Series, pd.Series]:
    """Apply nonnegative multiplicative coefficients to forecast intervals."""

    aligned_lower = lower.loc[coefficients.index].astype(float)
    aligned_upper = upper.loc[coefficients.index].astype(float)
    adjusted_lower = aligned_lower * coefficients
    adjusted_upper = aligned_upper * coefficients
    return (
        pd.concat([adjusted_lower, adjusted_upper], axis=1).min(axis=1),
        pd.concat([adjusted_lower, adjusted_upper], axis=1).max(axis=1),
    )


def _coerce_forecast_frame(
    forecast: ForecastFrame | pd.DataFrame,
) -> ForecastFrame:
    if isinstance(forecast, ForecastFrame):
        return forecast
    return ForecastFrame(values=forecast)
