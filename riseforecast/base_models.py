"""Counterfactual base forecasting models.

These models implement the first RISE stage: forecasting the no-shock baseline
trajectory from pre-shock data. The wrappers are intentionally small and
standardized so they can be combined, validated, and later reconciled.
"""

from __future__ import annotations

import warnings
from abc import ABC, abstractmethod
from collections.abc import Callable, Iterable
from dataclasses import dataclass

import numpy as np
import pandas as pd


@dataclass(frozen=True)
class BaseForecast:
    """Counterfactual no-shock forecast values."""

    values: pd.DataFrame
    model_name: str
    lower: pd.DataFrame | None = None
    upper: pd.DataFrame | None = None


class BaseForecaster(ABC):
    """Minimal interface for one-series baseline model wrappers."""

    name: str

    @abstractmethod
    def fit(self, y: pd.Series) -> BaseForecaster:
        """Fit a forecaster to one entity series."""

    @abstractmethod
    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        """Forecast a fitted entity series."""


class SeasonalNaiveForecaster(BaseForecaster):
    """Seasonal naive forecast using StatsForecast's `SeasonalNaive`."""

    name = "seasonal_naive"

    def __init__(self, period: int = 12) -> None:
        self.period = period

    def fit(self, y: pd.Series) -> SeasonalNaiveForecaster:
        self._y = _prepare_series(y)
        self._fallback = None
        self._sf_model = None
        if len(self._y) < self.period:
            return self

        from statsforecast.models import SeasonalNaive

        self._sf_model = _fit_statsforecast_model(
            SeasonalNaive(season_length=self.period, alias=self.name),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is not None:
            return _predict_statsforecast_model(
                self._sf_model,
                self._y,
                horizon=horizon,
                frequency=frequency,
                name=self.name,
            )
        if len(self._y) >= self.period:
            pattern = self._y.iloc[-self.period :]
        else:
            pattern = self._y.iloc[-1:]
        values = np.resize(pattern.to_numpy(dtype=float), horizon)
        return pd.Series(
            values,
            index=_future_index(self._y.index[-1], horizon, frequency),
            name=self.name,
        )


class RandomWalkDriftForecaster(BaseForecaster):
    """Random walk with drift using StatsForecast's `RandomWalkWithDrift`."""

    name = "random_walk_drift"

    def fit(self, y: pd.Series) -> RandomWalkDriftForecaster:
        self._y = _prepare_series(y)
        self._sf_model = None
        if len(self._y) > 1:
            from statsforecast.models import RandomWalkWithDrift

            self._sf_model = _fit_statsforecast_model(
                RandomWalkWithDrift(alias=self.name),
                self._y,
            )
        if len(self._y) > 1:
            total_change = self._y.iloc[-1] - self._y.iloc[0]
            self._drift = float(total_change / (len(self._y) - 1))
        else:
            self._drift = 0.0
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is not None:
            return _predict_statsforecast_model(
                self._sf_model,
                self._y,
                horizon=horizon,
                frequency=frequency,
                name=self.name,
            )
        steps = np.arange(1, horizon + 1, dtype=float)
        values = float(self._y.iloc[-1]) + self._drift * steps
        return pd.Series(
            values,
            index=_future_index(self._y.index[-1], horizon, frequency),
            name=self.name,
        )


class AutoARIMAForecaster(BaseForecaster):
    """Automatic ARIMA using StatsForecast's `AutoARIMA`."""

    name = "arima"

    def __init__(
        self,
        max_p: int = 2,
        max_q: int = 2,
        max_d: int = 2,
        period: int = 12,
    ) -> None:
        self.max_p = max_p
        self.max_q = max_q
        self.max_d = max_d
        self.period = period

    def fit(self, y: pd.Series) -> AutoARIMAForecaster:
        self._y = _prepare_series(y)
        self._fallback = RandomWalkDriftForecaster().fit(self._y)
        self._sf_model = None
        if len(self._y) < 8:
            return self

        from statsforecast.models import AutoARIMA

        seasonal = len(self._y) >= 2 * self.period
        self._sf_model = _fit_statsforecast_model(
            AutoARIMA(
                max_p=self.max_p,
                max_q=self.max_q,
                max_d=self.max_d,
                max_P=1,
                max_Q=1,
                max_order=max(5, self.max_p + self.max_q + 2),
                seasonal=seasonal,
                season_length=self.period if seasonal else 1,
                stepwise=True,
                alias=self.name,
            ),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class ETSForecaster(BaseForecaster):
    """Automatic ETS using StatsForecast's `AutoETS`."""

    name = "ets"

    def __init__(self, period: int = 12) -> None:
        self.period = period

    def fit(self, y: pd.Series) -> ETSForecaster:
        self._y = _prepare_series(y)
        self._fallback = RandomWalkDriftForecaster().fit(self._y)
        self._sf_model = None
        if len(self._y) < 4:
            return self

        from statsforecast.models import AutoETS

        season_length = self.period if len(self._y) >= 2 * self.period else 1
        self._sf_model = _fit_statsforecast_model(
            AutoETS(season_length=season_length, model="ZZZ", alias=self.name),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class HoltTrendForecaster(BaseForecaster):
    """Holt's linear trend model using StatsForecast's `Holt`."""

    name = "holt"

    def fit(self, y: pd.Series) -> HoltTrendForecaster:
        self._y = _prepare_series(y)
        self._fallback = RandomWalkDriftForecaster().fit(self._y)
        self._sf_model = None
        if len(self._y) < 4:
            return self

        from statsforecast.models import Holt

        self._sf_model = _fit_statsforecast_model(
            Holt(alias=self.name),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class HoltWintersForecaster(BaseForecaster):
    """Holt-Winters model using StatsForecast's `HoltWinters`."""

    name = "holt_winters"

    def __init__(self, period: int = 12) -> None:
        self.period = period

    def fit(self, y: pd.Series) -> HoltWintersForecaster:
        self._y = _prepare_series(y)
        self._fallback = HoltTrendForecaster().fit(self._y)
        self._sf_model = None
        if len(self._y) < 2 * self.period:
            return self

        from statsforecast.models import HoltWinters

        self._sf_model = _fit_statsforecast_model(
            HoltWinters(season_length=self.period, alias=self.name),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class STLForecaster(BaseForecaster):
    """MSTL decomposition using StatsForecast's `MSTL` model."""

    def __init__(
        self,
        trend_model: str = "arima",
        period: int = 12,
    ) -> None:
        if trend_model not in {"arima", "ets"}:
            raise ValueError("trend_model must be 'arima' or 'ets'.")
        self.trend_model = trend_model
        self.period = period
        self.name = f"stl_{trend_model}"

    def fit(self, y: pd.Series) -> STLForecaster:
        self._y = _prepare_series(y)
        self._sf_model = None
        if len(self._y) < 2 * self.period:
            self._fallback = (
                default_model_registry().create(self.trend_model).fit(self._y)
            )
            return self

        from statsforecast.models import MSTL, AutoARIMA, AutoETS

        if self.trend_model == "arima":
            trend_forecaster = AutoARIMA(season_length=1, seasonal=False)
        else:
            trend_forecaster = AutoETS(season_length=1, model="ZZN")
        self._sf_model = _fit_statsforecast_model(
            MSTL(
                season_length=self.period,
                trend_forecaster=trend_forecaster,
                alias=self.name,
            ),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class TBATSForecaster(BaseForecaster):
    """TBATS model using StatsForecast's `AutoTBATS`."""

    name = "tbats"

    def __init__(self, period: int = 12) -> None:
        self.period = period

    def fit(self, y: pd.Series) -> TBATSForecaster:
        self._y = _prepare_series(y)
        self._fallback = HoltWintersForecaster(period=self.period).fit(self._y)
        self._sf_model = None
        if len(self._y) < 2 * self.period:
            return self

        from statsforecast.models import AutoTBATS

        self._sf_model = _fit_statsforecast_model(
            AutoTBATS(season_length=self.period, alias=self.name),
            self._y,
        )
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._sf_model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)
        return _predict_statsforecast_model(
            self._sf_model,
            self._y,
            horizon=horizon,
            frequency=frequency,
            name=self.name,
        )


class NeuralNetworkAutoregressionForecaster(BaseForecaster):
    """Feed-forward neural-network autoregression using lagged observations."""

    name = "nnetar"

    def __init__(
        self,
        lags: tuple[int, ...] = tuple(range(1, 13)),
        hidden_layer_size: int = 7,
        random_state: int = 42,
    ) -> None:
        self.lags = lags
        self.hidden_layer_size = hidden_layer_size
        self.random_state = random_state

    def fit(self, y: pd.Series) -> NeuralNetworkAutoregressionForecaster:
        self._y = _prepare_series(y)
        self._fallback = RandomWalkDriftForecaster().fit(self._y)
        max_lag = max(self.lags)
        if len(self._y) <= max_lag + 4:
            self._model = None
            return self

        from sklearn.neural_network import MLPRegressor
        from sklearn.pipeline import make_pipeline
        from sklearn.preprocessing import StandardScaler

        x, target = _lag_matrix(self._y.to_numpy(dtype=float), self.lags)
        self._model = make_pipeline(
            StandardScaler(),
            MLPRegressor(
                hidden_layer_sizes=(self.hidden_layer_size,),
                random_state=self.random_state,
                max_iter=1000,
            ),
        )
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            self._model.fit(x, target)
        return self

    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        _require_fitted(self, "_y")
        if self._model is None:
            return self._fallback.predict(horizon, frequency).rename(self.name)

        history = list(self._y.to_numpy(dtype=float))
        values = []
        for _ in range(horizon):
            features = np.array([[history[-lag] for lag in self.lags]], dtype=float)
            prediction = float(self._model.predict(features)[0])
            values.append(prediction)
            history.append(prediction)
        return pd.Series(
            values,
            index=_future_index(self._y.index[-1], horizon, frequency),
            name=self.name,
        )


class ModelRegistry:
    """Registry for base forecasting model factories."""

    def __init__(self) -> None:
        self._factories: dict[str, Callable[[], BaseForecaster]] = {}

    def register(self, name: str, factory: Callable[[], BaseForecaster]) -> None:
        self._factories[name] = factory

    def create(self, name: str) -> BaseForecaster:
        if name not in self._factories:
            raise KeyError(f"Unknown base forecaster: {name}")
        return self._factories[name]()

    def names(self) -> tuple[str, ...]:
        return tuple(sorted(self._factories))


def default_model_registry(period: int = 12) -> ModelRegistry:
    """Return the default model registry for the paper's base model pool."""

    registry = ModelRegistry()
    registry.register("seasonal_naive", lambda: SeasonalNaiveForecaster(period=period))
    registry.register("random_walk_drift", RandomWalkDriftForecaster)
    registry.register("rwf", RandomWalkDriftForecaster)
    registry.register("arima", AutoARIMAForecaster)
    registry.register("ets", lambda: ETSForecaster(period=period))
    registry.register("holt", HoltTrendForecaster)
    registry.register("holt_winters", lambda: HoltWintersForecaster(period=period))
    registry.register("stl_arima", lambda: STLForecaster("arima", period=period))
    registry.register("stl_ets", lambda: STLForecaster("ets", period=period))
    registry.register("tbats", lambda: TBATSForecaster(period=period))
    registry.register("nnetar", NeuralNetworkAutoregressionForecaster)
    registry.register("nnar", NeuralNetworkAutoregressionForecaster)
    return registry


def forecast_series(
    y: pd.Series,
    model_name: str,
    horizon: int,
    frequency: str = "MS",
    registry: ModelRegistry | None = None,
) -> pd.Series:
    """Fit one registered base model and forecast one series."""

    registry = default_model_registry() if registry is None else registry
    model = registry.create(model_name)
    return model.fit(y).predict(horizon=horizon, frequency=frequency)


def forecast_panel(
    observed: pd.DataFrame,
    models: Iterable[str],
    horizon: int,
    train_end: str | pd.Timestamp | None = None,
    frequency: str = "MS",
    registry: ModelRegistry | None = None,
) -> dict[str, BaseForecast]:
    """Forecast every column in a panel for each requested base model."""

    if horizon < 1:
        raise ValueError("horizon must be at least 1.")
    registry = default_model_registry() if registry is None else registry
    panel = observed.copy()
    panel.index = pd.to_datetime(panel.index)
    panel = panel.sort_index()
    if train_end is not None:
        panel = panel.loc[: pd.Timestamp(train_end)]
    valid_panel = panel.loc[panel.notna().any(axis=1)]
    if valid_panel.empty:
        raise ValueError("observed panel is empty after applying train_end.")
    panel = panel.loc[: valid_panel.index[-1]]

    forecast_index = _future_index(panel.index[-1], horizon, frequency)
    forecast_end = forecast_index[-1]
    forecasts: dict[str, BaseForecast] = {}
    for model_name in models:
        columns = {}
        for series_id in panel.columns:
            series = panel[series_id]
            prepared = _prepare_series(series)
            series_horizon = _horizon_to_date(
                last_observed_date=prepared.index[-1],
                forecast_end=forecast_end,
                frequency=frequency,
            )
            columns[series_id] = forecast_series(
                series,
                model_name=model_name,
                horizon=series_horizon,
                frequency=frequency,
                registry=registry,
            ).reindex(forecast_index)
        values = pd.DataFrame(columns, index=forecast_index)
        forecasts[model_name] = BaseForecast(values=values, model_name=model_name)
    return forecasts


def _prepare_series(y: pd.Series) -> pd.Series:
    result = y.copy()
    result.index = pd.to_datetime(result.index)
    result = result.sort_index().astype(float)
    first_valid = result.first_valid_index()
    last_valid = result.last_valid_index()
    if first_valid is None or last_valid is None:
        raise ValueError("Cannot fit a base forecaster to an empty series.")
    result = result.loc[first_valid:last_valid]
    result = result.interpolate(limit_direction="both").dropna()
    if result.empty:
        raise ValueError("Cannot fit a base forecaster to an empty series.")
    return result


def _future_index(
    last_observed_date: pd.Timestamp,
    horizon: int,
    frequency: str,
) -> pd.DatetimeIndex:
    offset = pd.tseries.frequencies.to_offset(frequency)
    start = pd.Timestamp(last_observed_date) + offset
    return pd.date_range(start=start, periods=horizon, freq=frequency)


def _horizon_to_date(
    last_observed_date: pd.Timestamp,
    forecast_end: pd.Timestamp,
    frequency: str,
) -> int:
    offset = pd.tseries.frequencies.to_offset(frequency)
    start = pd.Timestamp(last_observed_date) + offset
    dates = pd.date_range(start=start, end=forecast_end, freq=frequency)
    if len(dates) == 0:
        raise ValueError("forecast_end must be after each series' last observation.")
    return len(dates)


def _fit_statsforecast_model(model: object, y: pd.Series) -> object | None:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            return model.fit(y.to_numpy(dtype=float))
        except Exception:
            return None


def _predict_statsforecast_model(
    model: object,
    y: pd.Series,
    horizon: int,
    frequency: str,
    name: str,
) -> pd.Series:
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        prediction = model.predict(horizon)
    values = np.asarray(prediction["mean"], dtype=float)
    return pd.Series(
        values,
        index=_future_index(y.index[-1], horizon, frequency),
        name=name,
    )


def _lag_matrix(
    values: np.ndarray,
    lags: tuple[int, ...],
) -> tuple[np.ndarray, np.ndarray]:
    max_lag = max(lags)
    x = []
    y = []
    for index in range(max_lag, len(values)):
        x.append([values[index - lag] for lag in lags])
        y.append(values[index])
    return np.asarray(x, dtype=float), np.asarray(y, dtype=float)


def _require_fitted(model: object, attribute: str) -> None:
    if not hasattr(model, attribute):
        raise RuntimeError(f"{model.__class__.__name__} must be fitted before predict.")
