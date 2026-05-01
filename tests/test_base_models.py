import numpy as np
import pandas as pd

from riseforecast.base_models import (
    AutoARIMAForecaster,
    HoltTrendForecaster,
    RandomWalkDriftForecaster,
    SeasonalNaiveForecaster,
    default_model_registry,
    forecast_hierarchical_panel,
    forecast_panel,
    forecast_series,
    is_hierarchical_base_model,
    parse_hierarchical_base_model,
)
from riseforecast.hierarchy import HierarchySpec


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


def test_forecast_panel_extends_ragged_series_to_common_horizon() -> None:
    index = pd.date_range("2020-01-01", periods=6, freq="MS")
    observed = pd.DataFrame(
        {
            "complete": [10.0, 12.0, 14.0, 16.0, 18.0, 20.0],
            "ragged": [10.0, 12.0, 14.0, 16.0, np.nan, np.nan],
        },
        index=index,
    )

    forecasts = forecast_panel(
        observed,
        models=("random_walk_drift",),
        horizon=2,
        train_end="2020-06",
    )

    values = forecasts["random_walk_drift"].values
    assert values.index.tolist() == list(
        pd.date_range("2020-07-01", periods=2, freq="MS")
    )
    assert not values["ragged"].isna().any()


def test_parse_hierarchical_base_model_names() -> None:
    assert parse_hierarchical_base_model("top_down_arima") == (
        "top_down_forecast_proportions",
        "arima",
    )
    assert parse_hierarchical_base_model("top_down_ets") == (
        "top_down_forecast_proportions",
        "ets",
    )
    assert parse_hierarchical_base_model("wls_struct") == ("wls_struct", "arima")
    assert parse_hierarchical_base_model("mint_shrink_random_walk_drift") == (
        "mint_shrink",
        "random_walk_drift",
    )
    assert is_hierarchical_base_model("mint_shrink")
    assert not is_hierarchical_base_model("arima")


def test_forecast_hierarchical_panel_returns_bottom_level_candidate() -> None:
    observed = hierarchical_observed_panel()
    hierarchy = simple_hierarchy()

    forecast = forecast_hierarchical_panel(
        observed=observed,
        model_name="top_down_arima",
        horizon=2,
        train_end="2021-12",
        hierarchy=hierarchy,
    )

    assert forecast.model_name == "top_down_arima"
    assert forecast.values.columns.tolist() == ["series_a", "series_b"]
    assert forecast.values.index.tolist() == list(
        pd.date_range("2022-01-01", periods=2, freq="MS")
    )
    assert np.isfinite(forecast.values.to_numpy()).all()


def test_forecast_panel_accepts_hierarchical_candidates() -> None:
    observed = hierarchical_observed_panel()
    hierarchy = simple_hierarchy()

    forecasts = forecast_panel(
        observed,
        models=("top_down_ets", "wls_struct", "mint_shrink_random_walk_drift"),
        horizon=2,
        train_end="2021-12",
        hierarchy=hierarchy,
    )

    assert set(forecasts) == {
        "top_down_ets",
        "wls_struct",
        "mint_shrink_random_walk_drift",
    }
    for forecast in forecasts.values():
        assert forecast.values.columns.tolist() == ["series_a", "series_b"]
        assert np.isfinite(forecast.values.to_numpy()).all()


def test_forecast_panel_requires_hierarchy_for_hierarchical_candidates() -> None:
    observed = hierarchical_observed_panel()

    try:
        forecast_panel(observed, models=("top_down_arima",), horizon=1)
    except ValueError as exc:
        assert "requires a hierarchy" in str(exc)
    else:  # pragma: no cover
        raise AssertionError("Expected hierarchical candidate to require hierarchy.")


def simple_hierarchy() -> HierarchySpec:
    return HierarchySpec.from_series(
        pd.DataFrame(
            {
                "series_id": ["total", "region", "series_a", "series_b"],
                "parent_id": [None, "total", "region", "region"],
            }
        )
    )


def hierarchical_observed_panel() -> pd.DataFrame:
    index = pd.date_range("2021-01-01", periods=12, freq="MS")
    return pd.DataFrame(
        {
            "series_a": np.linspace(10.0, 21.0, len(index)),
            "series_b": np.linspace(30.0, 52.0, len(index)),
        },
        index=index,
    )
