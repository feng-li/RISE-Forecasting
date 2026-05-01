"""External-signal reference forecast methods."""

from __future__ import annotations

import os
import warnings
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import Literal

import numpy as np
import pandas as pd

from riseforecast.base_models import RandomWalkDriftForecaster
from riseforecast.data import ForecastFrame
from riseforecast.intervention import DateLike, select_forecast_date

ReferenceMethod = Literal["ratio", "growth_rate", "arimax", "prophet"]
RatioStatistic = Literal["mean", "median"]


@dataclass(frozen=True)
class ReferenceForecast:
    """Reference forecast values for the early recovery stage."""

    values: pd.DataFrame
    method: str
    components: dict[str, pd.DataFrame] = field(default_factory=dict)

    def anchor(self, date: DateLike | None = None) -> pd.Series:
        """Return one reference forecast row, defaulting to the last date."""

        anchor_date = self.values.index[-1] if date is None else pd.Timestamp(date)
        return select_forecast_date(self.values, anchor_date).rename(
            "reference_forecast"
        )

    def as_forecast_frame(self) -> ForecastFrame:
        """Return the reference path as a `ForecastFrame`."""

        return ForecastFrame(values=self.values)

    def to_frame(self) -> pd.DataFrame:
        """Return a tidy long-form reference forecast table."""

        frame = self.values.copy()
        frame.index.name = "date"
        return (
            frame.reset_index()
            .melt(id_vars="date", var_name="entity", value_name="forecast")
            .sort_values(["entity", "date"])
            .reset_index(drop=True)
        )


@dataclass(frozen=True)
class ReferenceSignalSpec:
    """One exogenous-variable case used in the reference forecast stage.

    `signal_name` is kept for backward compatibility. Conceptually it is the X
    variable name, or a tuple of X variable names for models such as ARIMAX.
    """

    signal_name: str | tuple[str, ...]
    method: ReferenceMethod
    name: str | None = None
    signal_lag: int = 0
    ratio_window: int | None = 24
    ratio_statistic: RatioStatistic = "mean"
    base_date: DateLike | None = None
    seasonal_period: int = 12
    min_train_size: int = 8

    @property
    def variables(self) -> tuple[str, ...]:
        """Return the exogenous variable names used by this spec."""

        return _signal_names(self.signal_name)


ReferenceXSpec = ReferenceSignalSpec


@dataclass(frozen=True)
class ReferenceForecaster:
    """Estimate near-term recovery forecasts from external signal matrices."""

    start: DateLike
    end: DateLike
    specs: tuple[ReferenceSignalSpec, ...]
    train_end: DateLike | None = None
    frequency: str = "MS"
    combine: str = "mean"

    def forecast(
        self,
        observed: pd.DataFrame,
        signals: Mapping[str, pd.DataFrame] | pd.DataFrame,
    ) -> ReferenceForecast:
        """Transform external signals into a date-by-series reference forecast."""

        if not self.specs:
            raise ValueError("At least one reference signal spec is required.")
        if self.combine != "mean":
            raise ValueError("Only mean reference forecast combinations are supported.")

        observed_matrix = _prepare_matrix(observed)
        signal_matrices = coerce_signal_matrices(signals)
        forecast_dates = pd.date_range(self.start, self.end, freq=self.frequency)
        if forecast_dates.empty:
            raise ValueError("Reference forecast date range is empty.")
        train_end = _resolve_train_end(
            observed_matrix,
            train_end=self.train_end,
            start=forecast_dates[0],
            frequency=self.frequency,
        )

        components: dict[str, pd.DataFrame] = {}
        for spec in self.specs:
            signal_names = _signal_names(spec.signal_name)
            missing_signals = [
                signal_name
                for signal_name in signal_names
                if signal_name not in signal_matrices
            ]
            if missing_signals:
                raise ValueError(
                    f"Missing signal matrix: {', '.join(missing_signals)}"
                )
            component_name = spec.name or f"{spec.method}:{'+'.join(signal_names)}"
            components[component_name] = self._forecast_component(
                observed=observed_matrix,
                signal_matrices=signal_matrices,
                forecast_dates=forecast_dates,
                train_end=train_end,
                spec=spec,
            )

        combined = _mean_frames(components)
        method = f"mean({'+'.join(components)})"
        return ReferenceForecast(
            values=combined.reindex(
                index=forecast_dates,
                columns=observed_matrix.columns,
            ),
            method=method,
            components=components,
        )

    def _forecast_component(
        self,
        observed: pd.DataFrame,
        signal_matrices: Mapping[str, pd.DataFrame],
        forecast_dates: pd.DatetimeIndex,
        train_end: pd.Timestamp,
        spec: ReferenceSignalSpec,
    ) -> pd.DataFrame:
        lagged_signals = {
            signal_name: align_signal_to_target_dates(
                _prepare_matrix(signal_matrices[signal_name]),
                signal_lag=spec.signal_lag,
                frequency=self.frequency,
            ).reindex(columns=observed.columns)
            for signal_name in _signal_names(spec.signal_name)
        }
        if spec.method == "ratio":
            signal = _single_signal(lagged_signals, spec)
            return ratio_reference_forecast(
                observed=observed,
                signal=signal,
                forecast_dates=forecast_dates,
                train_end=train_end,
                ratio_window=spec.ratio_window,
                ratio_statistic=spec.ratio_statistic,
            )
        if spec.method == "growth_rate":
            signal = _single_signal(lagged_signals, spec)
            base_date = (
                train_end
                if spec.base_date is None
                else _resolve_available_date(observed, pd.Timestamp(spec.base_date))
            )
            return growth_rate_reference_forecast(
                latest_observed=select_forecast_date(observed, base_date),
                signal=_select_dates(signal, forecast_dates),
                baseline_signal=select_forecast_date(signal, base_date),
            )
        if spec.method == "arimax":
            return arimax_reference_forecast(
                observed=observed,
                signals=lagged_signals,
                forecast_dates=forecast_dates,
                train_end=train_end,
                frequency=self.frequency,
                seasonal_period=spec.seasonal_period,
                min_train_size=spec.min_train_size,
            )
        if spec.method == "prophet":
            return prophet_reference_forecast(
                observed=observed,
                signals=lagged_signals,
                forecast_dates=forecast_dates,
                train_end=train_end,
                frequency=self.frequency,
                seasonal_period=spec.seasonal_period,
                min_train_size=spec.min_train_size,
            )
        raise ValueError(f"Unknown reference method: {spec.method}")


def reference_forecast(
    observed: pd.DataFrame,
    signals: Mapping[str, pd.DataFrame] | pd.DataFrame,
    start: DateLike,
    end: DateLike,
    specs: tuple[ReferenceSignalSpec, ...],
    train_end: DateLike | None = None,
    frequency: str = "MS",
    combine: str = "mean",
) -> ReferenceForecast:
    """Convenience wrapper for external-signal reference forecasts."""

    forecaster = ReferenceForecaster(
        start=start,
        end=end,
        specs=specs,
        train_end=train_end,
        frequency=frequency,
        combine=combine,
    )
    return forecaster.forecast(observed=observed, signals=signals)


def reference_specs_from_config(
    config: Mapping[str, object],
) -> tuple[ReferenceXSpec, ...]:
    """Build reference X specs from a dataset or reference config dictionary."""

    reference_config = config.get("reference", config)
    if not isinstance(reference_config, Mapping):
        raise ValueError("reference config must be a mapping.")
    x_items = reference_config.get("x", ())
    specs = []
    for item in x_items:
        if not isinstance(item, Mapping):
            raise ValueError("Each reference x item must be a mapping.")
        raw_variables = item.get(
            "variables",
            item.get("signals", item.get("signal_name")),
        )
        variables = _coerce_variable_names(raw_variables)
        if not variables:
            raise ValueError("Each reference x item must define variables.")
        method = item.get("method")
        if method is None:
            raise ValueError("Each reference x item must define method.")
        specs.append(
            ReferenceXSpec(
                signal_name=variables[0] if len(variables) == 1 else variables,
                method=method,
                name=item.get("name"),
                signal_lag=int(item.get("lag", item.get("signal_lag", 0))),
                ratio_window=item.get("ratio_window", 24),
                ratio_statistic=item.get("ratio_statistic", "mean"),
                base_date=item.get("base_date"),
                seasonal_period=int(item.get("seasonal_period", 12)),
                min_train_size=int(item.get("min_train_size", 8)),
            )
        )
    return tuple(specs)


def align_signal_to_target_dates(
    signal: pd.DataFrame,
    signal_lag: int = 0,
    frequency: str = "MS",
) -> pd.DataFrame:
    """Index signal values by the target date they are intended to predict."""

    matrix = _prepare_matrix(signal)
    if signal_lag == 0:
        return matrix
    offset = pd.tseries.frequencies.to_offset(frequency) * signal_lag
    result = matrix.copy()
    result.index = result.index + offset
    return result.sort_index()


def ratio_reference_forecast(
    observed: pd.DataFrame,
    signal: pd.DataFrame,
    forecast_dates: pd.DatetimeIndex,
    train_end: DateLike,
    ratio_window: int | None = 24,
    ratio_statistic: RatioStatistic = "mean",
) -> pd.DataFrame:
    """Forecast targets as future signal values times historical target/signal ratio."""

    observed_matrix = _prepare_matrix(observed)
    signal_matrix = _prepare_matrix(signal).reindex(columns=observed_matrix.columns)
    train_dates = observed_matrix.index.intersection(signal_matrix.index)
    train_dates = train_dates[train_dates <= pd.Timestamp(train_end)]
    if train_dates.empty:
        raise ValueError("No overlapping observed and signal rows for ratio fitting.")

    ratios = observed_matrix.loc[train_dates].divide(
        signal_matrix.loc[train_dates].replace(0, pd.NA)
    )
    if ratio_window is not None:
        if ratio_window < 1:
            raise ValueError("ratio_window must be positive or None.")
        ratios = ratios.tail(ratio_window)
    if ratio_statistic == "mean":
        ratio = ratios.mean(axis=0, skipna=True)
    elif ratio_statistic == "median":
        ratio = ratios.median(axis=0, skipna=True)
    else:
        raise ValueError("ratio_statistic must be 'mean' or 'median'.")
    future_signal = _select_dates(signal_matrix, forecast_dates)
    return ratio_signal_forecast(future_signal, ratio)


def arimax_reference_forecast(
    observed: pd.DataFrame,
    signals: Mapping[str, pd.DataFrame],
    forecast_dates: pd.DatetimeIndex,
    train_end: DateLike,
    frequency: str = "MS",
    seasonal_period: int = 12,
    min_train_size: int = 8,
) -> pd.DataFrame:
    """Forecast targets with AutoARIMA and exogenous signal regressors."""

    if not signals:
        raise ValueError("At least one exogenous signal is required for ARIMAX.")
    observed_matrix = _prepare_matrix(observed)
    signal_matrices = {
        name: _prepare_matrix(signal).reindex(columns=observed_matrix.columns)
        for name, signal in signals.items()
    }
    for signal_name, signal in signal_matrices.items():
        _select_dates(signal, forecast_dates)
        if signal.loc[: pd.Timestamp(train_end)].empty:
            raise ValueError(f"Signal has no training rows: {signal_name}")

    columns = {}
    for entity in observed_matrix.columns:
        y_train, x_train, x_future, prediction_dates = _arimax_design_matrices(
            observed=observed_matrix,
            signals=signal_matrices,
            entity=entity,
            forecast_dates=forecast_dates,
            train_end=pd.Timestamp(train_end),
            frequency=frequency,
        )
        columns[entity] = _fit_predict_arimax_series(
            y_train=y_train,
            x_train=x_train,
            x_future=x_future,
            prediction_dates=prediction_dates,
            forecast_dates=forecast_dates,
            frequency=frequency,
            seasonal_period=seasonal_period,
            min_train_size=min_train_size,
        )
    return pd.DataFrame(columns, index=forecast_dates)


def prophet_reference_forecast(
    observed: pd.DataFrame,
    signals: Mapping[str, pd.DataFrame],
    forecast_dates: pd.DatetimeIndex,
    train_end: DateLike,
    frequency: str = "MS",
    seasonal_period: int = 12,
    min_train_size: int = 8,
) -> pd.DataFrame:
    """Forecast targets with Prophet and exogenous signal regressors."""

    if not signals:
        raise ValueError("At least one exogenous signal is required for Prophet.")
    observed_matrix = _prepare_matrix(observed)
    signal_matrices = {
        name: _prepare_matrix(signal).reindex(columns=observed_matrix.columns)
        for name, signal in signals.items()
    }
    for signal_name, signal in signal_matrices.items():
        _select_dates(signal, forecast_dates)
        if signal.loc[: pd.Timestamp(train_end)].empty:
            raise ValueError(f"Signal has no training rows: {signal_name}")

    columns = {}
    for entity in observed_matrix.columns:
        y_train, x_train, x_future = _prophet_design_matrices(
            observed=observed_matrix,
            signals=signal_matrices,
            entity=entity,
            forecast_dates=forecast_dates,
            train_end=pd.Timestamp(train_end),
        )
        columns[entity] = _fit_predict_prophet_series(
            y_train=y_train,
            x_train=x_train,
            x_future=x_future,
            forecast_dates=forecast_dates,
            frequency=frequency,
            seasonal_period=seasonal_period,
            min_train_size=min_train_size,
        )
    return pd.DataFrame(columns, index=forecast_dates)


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
    values = _mean_frames({item.method: item.values for item in forecasts})
    method = "+".join(item.method for item in forecasts)
    components = {item.method: item.values for item in forecasts}
    return ReferenceForecast(
        values=values,
        method=f"mean({method})",
        components=components,
    )


def coerce_signal_matrices(
    signals: Mapping[str, pd.DataFrame] | pd.DataFrame,
) -> dict[str, pd.DataFrame]:
    """Coerce mapping or long-form exogenous data to a dictionary of matrices."""

    if isinstance(signals, Mapping):
        return {name: _prepare_matrix(frame) for name, frame in signals.items()}
    frame = signals.copy()
    if "kind" in frame.columns:
        frame = frame.loc[frame["kind"].isin(["signal", "exogenous"])].copy()
    if {"date", "entity", "signal_name", "signal_value"} <= set(frame.columns):
        date_col = "date"
        entity_col = "entity"
        name_col = "signal_name"
        value_col = "signal_value"
    elif {"date", "series_id", "name", "value"} <= set(frame.columns):
        date_col = "date"
        entity_col = "series_id"
        name_col = "name"
        value_col = "value"
    else:
        raise ValueError(
            "signals must be a mapping of matrices or a long frame with signal "
            "name/value columns."
        )
    frame[date_col] = pd.to_datetime(frame[date_col])
    matrices = {}
    for signal_name, group in frame.groupby(name_col):
        matrix = group.pivot(
            index=date_col,
            columns=entity_col,
            values=value_col,
        )
        matrices[str(signal_name)] = _prepare_matrix(matrix)
    return matrices


def _signal_names(signal_name: str | tuple[str, ...]) -> tuple[str, ...]:
    if isinstance(signal_name, str):
        return (signal_name,)
    if not signal_name:
        raise ValueError("signal_name tuple cannot be empty.")
    return tuple(signal_name)


def _coerce_variable_names(value: object) -> tuple[str, ...]:
    if value is None:
        return ()
    if isinstance(value, str):
        return (value,)
    return tuple(str(item) for item in value)


def _single_signal(
    signals: Mapping[str, pd.DataFrame],
    spec: ReferenceSignalSpec,
) -> pd.DataFrame:
    signal_names = tuple(signals)
    if len(signal_names) != 1:
        raise ValueError(f"{spec.method} reference specs require exactly one signal.")
    return signals[signal_names[0]]


def _prepare_matrix(frame: pd.DataFrame) -> pd.DataFrame:
    matrix = frame.copy()
    matrix.index = pd.to_datetime(matrix.index)
    matrix = matrix.sort_index()
    return matrix.astype(float)


def _resolve_train_end(
    observed: pd.DataFrame,
    train_end: DateLike | None,
    start: pd.Timestamp,
    frequency: str,
) -> pd.Timestamp:
    if train_end is not None:
        return _resolve_available_date(observed, pd.Timestamp(train_end))
    offset = pd.tseries.frequencies.to_offset(frequency)
    before_start = observed.loc[: start - offset]
    before_start = before_start.loc[before_start.notna().any(axis=1)]
    if not before_start.empty:
        return pd.Timestamp(before_start.index[-1])
    return _resolve_available_date(observed, start)


def _resolve_available_date(observed: pd.DataFrame, date: pd.Timestamp) -> pd.Timestamp:
    available = observed.loc[:date]
    available = available.loc[available.notna().any(axis=1)]
    if available.empty:
        raise ValueError(f"No observed rows are available at or before {date.date()}.")
    return pd.Timestamp(available.index[-1])


def _select_dates(matrix: pd.DataFrame, dates: pd.DatetimeIndex) -> pd.DataFrame:
    missing = [date for date in dates if date not in matrix.index]
    if missing:
        joined = ", ".join(date.strftime("%Y-%m-%d") for date in missing[:5])
        raise ValueError(f"Signal is missing required forecast dates: {joined}")
    return matrix.loc[dates]


def _arimax_design_matrices(
    observed: pd.DataFrame,
    signals: Mapping[str, pd.DataFrame],
    entity: object,
    forecast_dates: pd.DatetimeIndex,
    train_end: pd.Timestamp,
    frequency: str,
) -> tuple[pd.Series, pd.DataFrame, pd.DataFrame, pd.DatetimeIndex]:
    train_parts = [observed.loc[:train_end, entity].rename("y")]
    for signal_name, signal in signals.items():
        signal_series = signal.loc[:, entity].rename(signal_name)
        train_parts.append(signal_series.loc[:train_end])

    train = pd.concat(train_parts, axis=1, join="inner").dropna()
    if train.empty:
        raise ValueError(f"No complete ARIMAX training rows for entity {entity}.")
    offset = pd.tseries.frequencies.to_offset(frequency)
    prediction_dates = pd.date_range(
        start=pd.Timestamp(train.index[-1]) + offset,
        end=forecast_dates[-1],
        freq=frequency,
    )
    future_parts = []
    for signal_name, signal in signals.items():
        signal_series = signal.loc[:, entity].rename(signal_name)
        future_parts.append(signal_series.loc[prediction_dates])
    x_future = pd.concat(future_parts, axis=1)
    if x_future.isna().any().any():
        raise ValueError(f"Future exogenous signals contain NaNs for entity {entity}.")
    return (
        train["y"].astype(float),
        train.drop(columns="y").astype(float),
        x_future,
        prediction_dates,
    )


def _fit_predict_arimax_series(
    y_train: pd.Series,
    x_train: pd.DataFrame,
    x_future: pd.DataFrame,
    prediction_dates: pd.DatetimeIndex,
    forecast_dates: pd.DatetimeIndex,
    frequency: str,
    seasonal_period: int,
    min_train_size: int,
) -> pd.Series:
    if len(y_train) < min_train_size:
        return _fallback_series_forecast(
            y_train,
            horizon=len(prediction_dates),
            frequency=frequency,
            prediction_index=prediction_dates,
            output_index=forecast_dates,
        )

    from statsforecast.models import AutoARIMA

    seasonal = len(y_train) >= 2 * seasonal_period
    model = AutoARIMA(
        max_p=2,
        max_q=2,
        max_d=2,
        max_P=1,
        max_Q=1,
        max_order=5,
        seasonal=seasonal,
        season_length=seasonal_period if seasonal else 1,
        stepwise=True,
        alias="arimax",
    )
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            fitted = model.fit(
                y_train.to_numpy(dtype=float),
                X=x_train.to_numpy(dtype=float),
            )
            prediction = fitted.predict(
                len(prediction_dates),
                X=x_future.to_numpy(dtype=float),
            )
            values = np.asarray(prediction["mean"], dtype=float)
        except Exception:
            return _fallback_series_forecast(
                y_train,
                horizon=len(prediction_dates),
                frequency=frequency,
                prediction_index=prediction_dates,
                output_index=forecast_dates,
            )
    return pd.Series(values, index=prediction_dates, name=y_train.name).reindex(
        forecast_dates
    )


def _prophet_design_matrices(
    observed: pd.DataFrame,
    signals: Mapping[str, pd.DataFrame],
    entity: object,
    forecast_dates: pd.DatetimeIndex,
    train_end: pd.Timestamp,
) -> tuple[pd.Series, pd.DataFrame, pd.DataFrame]:
    train_parts = [observed.loc[:train_end, entity].rename("y")]
    for signal_name, signal in signals.items():
        signal_series = signal.loc[:, entity].rename(signal_name)
        train_parts.append(signal_series.loc[:train_end])

    train = pd.concat(train_parts, axis=1, join="inner").dropna()
    if train.empty:
        raise ValueError(f"No complete Prophet training rows for entity {entity}.")

    future_parts = []
    for signal_name, signal in signals.items():
        signal_series = signal.loc[:, entity].rename(signal_name)
        future_parts.append(signal_series.loc[forecast_dates])
    x_future = pd.concat(future_parts, axis=1)
    if x_future.isna().any().any():
        raise ValueError(f"Future exogenous signals contain NaNs for entity {entity}.")
    return (
        train["y"].astype(float),
        train.drop(columns="y").astype(float),
        x_future.astype(float),
    )


def _fit_predict_prophet_series(
    y_train: pd.Series,
    x_train: pd.DataFrame,
    x_future: pd.DataFrame,
    forecast_dates: pd.DatetimeIndex,
    frequency: str,
    seasonal_period: int,
    min_train_size: int,
) -> pd.Series:
    offset = pd.tseries.frequencies.to_offset(frequency)
    prediction_dates = pd.date_range(
        start=pd.Timestamp(y_train.index[-1]) + offset,
        end=forecast_dates[-1],
        freq=frequency,
    )
    if len(y_train) < min_train_size:
        return _fallback_series_forecast(
            y_train,
            horizon=len(prediction_dates),
            frequency=frequency,
            prediction_index=prediction_dates,
            output_index=forecast_dates,
        )

    try:
        Prophet = _prophet_class()
    except ImportError:
        raise
    except Exception:
        return _fallback_series_forecast(
            y_train,
            horizon=len(prediction_dates),
            frequency=frequency,
            prediction_index=prediction_dates,
            output_index=forecast_dates,
        )

    regressor_names = _prophet_regressor_names(x_train.columns)
    train = pd.DataFrame(
        {
            "ds": pd.to_datetime(y_train.index),
            "y": y_train.to_numpy(dtype=float),
        }
    )
    future = pd.DataFrame({"ds": pd.to_datetime(forecast_dates)})
    for source_name, regressor_name in regressor_names.items():
        train[regressor_name] = x_train.loc[:, source_name].to_numpy(dtype=float)
        future[regressor_name] = x_future.loc[
            forecast_dates,
            source_name,
        ].to_numpy(dtype=float)

    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            model = Prophet(
                yearly_seasonality=_prophet_yearly_seasonality(
                    y_train,
                    seasonal_period=seasonal_period,
                ),
                weekly_seasonality=False,
                daily_seasonality=False,
            )
            for regressor_name in regressor_names.values():
                model.add_regressor(regressor_name)
            model.fit(train)
            prediction = model.predict(future)
            values = prediction["yhat"].to_numpy(dtype=float)
        except Exception:
            return _fallback_series_forecast(
                y_train,
                horizon=len(prediction_dates),
                frequency=frequency,
                prediction_index=prediction_dates,
                output_index=forecast_dates,
            )

    return pd.Series(values, index=forecast_dates, name=y_train.name)


def _prophet_class():
    os.environ.setdefault("MPLCONFIGDIR", "/tmp")
    try:
        from prophet import Prophet
    except ImportError as exc:
        raise ImportError(
            "Prophet reference forecasts require the core dependency `prophet`. "
            "Reinstall the package dependencies or run `pip install prophet`."
        ) from exc
    return Prophet


def _prophet_regressor_names(columns: pd.Index) -> dict[object, str]:
    return {column: f"x_{index}" for index, column in enumerate(columns)}


def _prophet_yearly_seasonality(
    y_train: pd.Series,
    seasonal_period: int,
) -> bool | str:
    if seasonal_period == 12 and len(y_train) >= 2 * seasonal_period:
        return True
    return False


def _fallback_series_forecast(
    y_train: pd.Series,
    horizon: int,
    frequency: str,
    prediction_index: pd.DatetimeIndex,
    output_index: pd.DatetimeIndex,
) -> pd.Series:
    forecast = RandomWalkDriftForecaster().fit(y_train).predict(
        horizon=horizon,
        frequency=frequency,
    )
    forecast.index = prediction_index
    return forecast.reindex(output_index)


def _mean_frames(frames: Mapping[str, pd.DataFrame]) -> pd.DataFrame:
    if not frames:
        raise ValueError("At least one forecast matrix is required.")
    aligned = pd.concat(frames, names=["component", "date"])
    return aligned.groupby(level="date").mean()
