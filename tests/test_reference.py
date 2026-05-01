import numpy as np
import pandas as pd
import pytest

from riseforecast.config import ReferenceForecastConfig, ReferenceXConfig
from riseforecast.pipeline import RecoveryForecastingPipeline
from riseforecast.reference import (
    ReferenceForecaster,
    ReferenceSignalSpec,
    align_signal_to_target_dates,
    arimax_reference_forecast,
    coerce_signal_matrices,
    prophet_reference_forecast,
    reference_forecast,
    reference_specs_from_config,
)


def observed_matrix() -> pd.DataFrame:
    return pd.DataFrame(
        {"series_a": [100.0, 200.0], "series_b": [50.0, 100.0]},
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )


def signal_matrix() -> pd.DataFrame:
    return pd.DataFrame(
        {"series_a": [10.0, 20.0, 30.0, 40.0], "series_b": [5.0, 10.0, 15.0, 20.0]},
        index=pd.date_range("2024-01-01", periods=4, freq="MS"),
    )


def longer_observed_matrix() -> pd.DataFrame:
    index = pd.date_range("2021-01-01", periods=36, freq="MS")
    signal = np.arange(10.0, 46.0)
    seasonal = np.tile([0.0, 1.0, -1.0], 12)
    return pd.DataFrame(
        {
            "series_a": 5.0 + 2.0 * signal + seasonal,
            "series_b": 10.0 + 3.0 * signal - seasonal,
        },
        index=index,
    )


def longer_signal_matrix() -> pd.DataFrame:
    index = pd.date_range("2021-01-01", periods=42, freq="MS")
    signal = np.arange(10.0, 52.0)
    return pd.DataFrame(
        {"series_a": signal, "series_b": signal},
        index=index,
    )


def test_reference_forecaster_ratio_method() -> None:
    result = ReferenceForecaster(
        start="2024-03",
        end="2024-04",
        train_end="2024-02",
        specs=(ReferenceSignalSpec("signal", method="ratio", ratio_window=None),),
    ).forecast(observed_matrix(), {"signal": signal_matrix()})

    assert result.values.index.tolist() == list(
        pd.date_range("2024-03-01", periods=2, freq="MS")
    )
    assert np.allclose(result.values["series_a"], [300.0, 400.0])
    assert np.allclose(result.anchor("2024-04"), [400.0, 200.0])


def test_reference_forecaster_growth_rate_method() -> None:
    result = reference_forecast(
        observed=observed_matrix(),
        signals={"signal": signal_matrix()},
        start="2024-03",
        end="2024-04",
        train_end="2024-02",
        specs=(ReferenceSignalSpec("signal", method="growth_rate"),),
    )

    assert np.allclose(result.values["series_a"], [300.0, 400.0])
    assert np.allclose(result.values["series_b"], [150.0, 200.0])


def test_reference_forecaster_combines_components_with_mean() -> None:
    result = ReferenceForecaster(
        start="2024-03",
        end="2024-04",
        train_end="2024-02",
        specs=(
            ReferenceSignalSpec("signal", method="ratio", ratio_window=None),
            ReferenceSignalSpec("signal", method="growth_rate"),
        ),
    ).forecast(observed_matrix(), {"signal": signal_matrix()})

    assert set(result.components) == {"ratio:signal", "growth_rate:signal"}
    assert np.allclose(result.values["series_a"], [300.0, 400.0])


def test_reference_forecaster_arimax_method_returns_finite_path() -> None:
    result = ReferenceForecaster(
        start="2024-01",
        end="2024-03",
        train_end="2023-12",
        specs=(ReferenceSignalSpec("signal", method="arimax", name="x_arimax"),),
    ).forecast(longer_observed_matrix(), {"signal": longer_signal_matrix()})

    assert set(result.components) == {"x_arimax"}
    assert result.values.shape == (3, 2)
    assert np.isfinite(result.values.to_numpy()).all()


def test_arimax_reference_forecast_accepts_multiple_exogenous_signals() -> None:
    observed = longer_observed_matrix()
    signal = longer_signal_matrix()
    second_signal = signal * 2
    forecast_dates = pd.date_range("2024-01-01", periods=2, freq="MS")

    forecast = arimax_reference_forecast(
        observed=observed,
        signals={"signal": signal, "second_signal": second_signal},
        forecast_dates=forecast_dates,
        train_end="2023-12",
    )

    assert forecast.index.tolist() == list(forecast_dates)
    assert np.isfinite(forecast.to_numpy()).all()


def test_prophet_reference_forecast_returns_finite_path() -> None:
    pytest.importorskip("prophet")
    observed = longer_observed_matrix()
    signal = longer_signal_matrix()
    forecast_dates = pd.date_range("2024-01-01", periods=2, freq="MS")

    forecast = prophet_reference_forecast(
        observed=observed,
        signals={"signal": signal},
        forecast_dates=forecast_dates,
        train_end="2023-12",
    )

    assert forecast.index.tolist() == list(forecast_dates)
    assert np.isfinite(forecast.to_numpy()).all()


def test_reference_signal_lag_indexes_signal_by_target_date() -> None:
    lagged = align_signal_to_target_dates(signal_matrix(), signal_lag=1)

    assert lagged.loc["2024-02-01", "series_a"] == 10.0
    assert lagged.loc["2024-03-01", "series_a"] == 20.0


def test_coerce_signal_matrices_accepts_long_frame() -> None:
    long = pd.DataFrame(
        {
            "date": ["2024-01-01", "2024-02-01"],
            "series_id": ["series_a", "series_a"],
            "name": ["search", "search"],
            "value": [10.0, 20.0],
        }
    )

    signals = coerce_signal_matrices(long)

    assert list(signals) == ["search"]
    assert signals["search"].loc["2024-02-01", "series_a"] == 20.0


def test_coerce_signal_matrices_accepts_exogenous_kind() -> None:
    long = pd.DataFrame(
        {
            "date": ["2024-01-01", "2024-02-01"],
            "series_id": ["series_a", "series_a"],
            "kind": ["exogenous", "exogenous"],
            "name": ["search", "search"],
            "value": [10.0, 20.0],
        }
    )

    signals = coerce_signal_matrices(long)

    assert signals["search"].loc["2024-02-01", "series_a"] == 20.0


def test_reference_specs_from_config_builds_named_x_cases() -> None:
    specs = reference_specs_from_config(
        {
            "reference": {
                "x": [
                    {
                        "name": "search_arimax",
                        "variables": ["search"],
                        "method": "arimax",
                        "lag": 1,
                    },
                    {
                        "name": "multi_x",
                        "variables": ["search", "capacity"],
                        "method": "arimax",
                    },
                    {
                        "name": "search_prophet",
                        "variables": ["search"],
                        "method": "prophet",
                    },
                ]
            }
        }
    )

    assert specs[0].name == "search_arimax"
    assert specs[0].variables == ("search",)
    assert specs[0].signal_lag == 1
    assert specs[1].variables == ("search", "capacity")
    assert specs[2].name == "search_prophet"
    assert specs[2].method == "prophet"


def test_pipeline_fits_reference_stage_from_external_signals() -> None:
    from riseforecast import PipelineConfig

    config = PipelineConfig(
        shock_start="2024-01",
        initial_date="2024-04",
        terminal_date="2024-12",
        forecast_start="2024-05",
        forecast_end="2024-12",
        reference=ReferenceForecastConfig(
            start="2024-03",
            end="2024-04",
            train_end="2024-02",
            x=(
                ReferenceXConfig(
                    name="generic_x_arimax",
                    variables=("signal",),
                    method="arimax",
                ),
            ),
        ),
    )

    pipeline = RecoveryForecastingPipeline(config).fit(
        observed_matrix(),
        external_signals={"signal": signal_matrix()},
    )

    assert pipeline.state.reference_forecast is not None
    assert np.isfinite(pipeline.state.reference_forecast.values.to_numpy()).all()
