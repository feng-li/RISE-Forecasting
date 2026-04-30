import numpy as np
import pandas as pd

from riseforecast.base_models import (
    AutoARIMAForecaster,
    HoltTrendForecaster,
    RandomWalkDriftForecaster,
    SeasonalNaiveForecaster,
    default_model_registry,
    forecast_panel,
    forecast_series,
)


def monthly_series(periods: int = 48) -> pd.Series:
    index = pd.date_range("2016-01-01", periods=periods, freq="MS")
    trend = np.linspace(100.0, 160.0, periods)
    seasonal = np.tile([0, 4, 8, 12, 8, 4, 0, -4, -8, -12, -8, -4], periods // 12)
    return pd.Series(trend + seasonal, index=index)


def test_default_model_registry_contains_paper_models() -> None:
    registry = default_model_registry()

    assert {
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
        "holt",
        "holt_winters",
        "stl_arima",
        "stl_ets",
        "tbats",
        "nnetar",
    } <= set(registry.names())


def test_seasonal_naive_repeats_latest_seasonal_cycle() -> None:
    series = monthly_series()

    forecast = SeasonalNaiveForecaster(period=12).fit(series).predict(14)

    assert len(forecast) == 14
    assert forecast.index[0] == pd.Timestamp("2020-01-01")
    assert forecast.iloc[0] == series.iloc[-12]
    assert forecast.iloc[12] == series.iloc[-12]


def test_random_walk_drift_extrapolates_drift() -> None:
    series = pd.Series(
        [10.0, 12.0, 14.0],
        index=pd.date_range("2020-01-01", periods=3, freq="MS"),
    )

    forecast = RandomWalkDriftForecaster().fit(series).predict(2)

    assert np.allclose(forecast, [16.0, 18.0])


def test_arima_and_holt_forecasters_return_requested_horizon() -> None:
    series = monthly_series()

    arima = AutoARIMAForecaster(max_p=1, max_q=1).fit(series).predict(6)
    holt = HoltTrendForecaster().fit(series).predict(6)

    assert len(arima) == 6
    assert len(holt) == 6
    assert arima.index[0] == pd.Timestamp("2020-01-01")
    assert holt.index[-1] == pd.Timestamp("2020-06-01")


def test_forecast_panel_returns_forecast_per_model_and_series() -> None:
    observed = pd.DataFrame(
        {
            "series_a": monthly_series(),
            "series_b": monthly_series() + 10,
        }
    )

    forecasts = forecast_panel(
        observed,
        models=("seasonal_naive", "random_walk_drift"),
        horizon=3,
        train_end="2019-12",
    )

    assert set(forecasts) == {"seasonal_naive", "random_walk_drift"}
    assert forecasts["seasonal_naive"].values.shape == (3, 2)
    assert forecasts["random_walk_drift"].values.index[0] == pd.Timestamp("2020-01-01")


def test_forecast_series_uses_registry() -> None:
    forecast = forecast_series(monthly_series(), "seasonal_naive", horizon=2)

    assert len(forecast) == 2


def test_base_forecaster_does_not_backfill_before_series_start() -> None:
    index = pd.date_range("2020-01-01", periods=5, freq="MS")
    series = pd.Series([np.nan, np.nan, 10.0, np.nan, 14.0], index=index)

    forecast = RandomWalkDriftForecaster().fit(series).predict(1)

    assert forecast.index[0] == pd.Timestamp("2020-06-01")
    assert forecast.iloc[0] == 16.0
