import numpy as np
import pandas as pd
import pytest

from riseforecast import InitialForecaster, PipelineConfig
from riseforecast.config import InitialForecastConfig
from riseforecast.initial import initial_forecast
from riseforecast.pipeline import RecoveryForecastingPipeline


def monthly_panel() -> pd.DataFrame:
    index = pd.date_range("2019-01-01", periods=16, freq="MS")
    return pd.DataFrame(
        {
            "series_a": np.arange(100.0, 116.0),
            "series_b": np.arange(200.0, 216.0),
        },
        index=index,
    )


def test_initial_forecaster_returns_path_and_anchor() -> None:
    result = InitialForecaster(
        initial_date="2020-06",
        train_end="2020-04",
        models=("random_walk_drift",),
    ).forecast(monthly_panel())

    assert result.values.shape == (2, 2)
    assert result.values.index.tolist() == list(
        pd.to_datetime(["2020-05-01", "2020-06-01"])
    )
    assert result.initial_date == pd.Timestamp("2020-06-01")
    assert result.initial.loc["series_a"] == result.values.loc[
        "2020-06-01",
        "series_a",
    ]


def test_initial_forecaster_averages_base_model_components() -> None:
    result = InitialForecaster(
        initial_date="2020-06",
        train_end="2020-04",
        models=("seasonal_naive", "random_walk_drift"),
    ).forecast(monthly_panel())

    expected = (
        result.components["seasonal_naive"].values
        + result.components["random_walk_drift"].values
    ) / 2

    assert set(result.components) == {"seasonal_naive", "random_walk_drift"}
    assert result.model_name == "mean(seasonal_naive+random_walk_drift)"
    assert np.allclose(result.values, expected)


def test_initial_forecast_convenience_wrapper() -> None:
    result = initial_forecast(
        monthly_panel(),
        initial_date="2020-05",
        train_end="2020-04",
        models=("random_walk_drift",),
    )

    assert result.values.index.tolist() == [pd.Timestamp("2020-05-01")]


def test_initial_forecaster_requires_future_initial_date() -> None:
    with pytest.raises(ValueError, match="initial_date must be after train_end"):
        InitialForecaster(
            initial_date="2020-04",
            train_end="2020-04",
            models=("random_walk_drift",),
        ).forecast(monthly_panel())


def test_pipeline_fits_configured_initial_stage() -> None:
    config = PipelineConfig(
        shock_start="2020-01",
        initial_date="2020-06",
        terminal_date="2021-07",
        forecast_start="2020-08",
        forecast_end="2021-07",
        initial=InitialForecastConfig(
            train_end="2020-04",
            models=("random_walk_drift",),
        ),
    )

    pipeline = RecoveryForecastingPipeline(config).fit(monthly_panel())

    assert pipeline.state.initial_forecast is not None
    assert pipeline.state.reference_forecast is not None
    assert pipeline.state.reference_forecast.values.index[-1] == pd.Timestamp(
        "2020-06-01"
    )
