import numpy as np
import pandas as pd

from riseforecast import RecoveryDataset, RecoveryForecastingPipeline


def compact_recovery_dataset() -> RecoveryDataset:
    series = pd.DataFrame(
        {
            "series_id": ["series_a", "series_b"],
            "series_name": ["Series A", "Series B"],
            "target_name": ["target", "target"],
            "unit": ["count", "count"],
            "policy": [1, 5],
            "distance": [1, 5],
            "recovery": [1, 5],
            "coefficient": [0.5, 0.8],
        }
    )
    observed = pd.DataFrame(
        {
            "series_a": [100.0, 120.0],
            "series_b": [200.0, 220.0],
        },
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )
    search_index = pd.DataFrame(
        {
            "series_a": [10.0, 12.0, 14.0, 16.0, 18.0],
            "series_b": [20.0, 22.0, 24.0, 26.0, 28.0],
        },
        index=pd.date_range("2024-01-01", periods=5, freq="MS"),
    )
    base_forecast = pd.DataFrame(
        {
            "series_a": [130.0, 140.0, 150.0],
            "series_b": [230.0, 240.0, 250.0],
        },
        index=pd.date_range("2024-03-01", periods=3, freq="MS"),
    )

    rows = []
    rows.extend(_matrix_rows(observed, kind="observed", name="target"))
    rows.extend(_matrix_rows(search_index, kind="exogenous", name="search_index"))
    rows.extend(
        _matrix_rows(base_forecast, kind="base_forecast", name="legacy_ensemble")
    )
    config = {
        "frequency": "MS",
        "shock": {"start": "2024-01"},
        "dates": {
            "observed_until": "2024-02",
            "initial_date": "2024-03",
            "forecast_start": "2024-04",
            "terminal_date": "2024-05",
            "forecast_end": "2024-05",
        },
        "reference": {
            "start": "2024-03",
            "end": "2024-03",
            "train_end": "2024-02",
            "x": [
                {
                    "name": "search_growth",
                    "variables": ["search_index"],
                    "method": "growth_rate",
                }
            ],
        },
        "curve": {"curves": ["linear"]},
    }
    return RecoveryDataset(
        series=series,
        panel=pd.DataFrame(rows),
        config=config,
    ).validate()


def test_pipeline_fits_dataset_from_compact_config() -> None:
    dataset = compact_recovery_dataset()

    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    forecast = pipeline.predict()

    assert pipeline.state.reference_forecast is not None
    assert pipeline.state.terminal_forecast is not None
    assert forecast.values.index.tolist() == list(
        pd.to_datetime(["2024-04-01", "2024-05-01"])
    )
    assert list(forecast.values.columns) == ["series_a", "series_b"]
    assert np.allclose(forecast.values.loc["2024-05-01"], [75.0, 200.0])
    assert np.allclose(forecast.values.loc["2024-04-01"], [107.5, 220.0])
    assert pipeline.state.seasonal_multipliers is None


def test_pipeline_uses_configured_regression_recovery_coefficients() -> None:
    direct_dataset = compact_recovery_dataset()
    config = {
        **direct_dataset.config,
        "recovery": {
            "method": "regression",
            "score_columns": ["policy", "distance", "recovery"],
            "anchors": {"series_a": 0.5, "series_b": 1.0},
        },
    }
    dataset = RecoveryDataset(
        series=direct_dataset.series,
        panel=direct_dataset.panel,
        config=config,
    ).validate()

    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)

    assert pipeline.state.recovery_coefficients is not None
    assert pipeline.state.terminal_forecast is not None
    assert pipeline.state.recovery_coefficients.loc["series_b"] == 1.0
    assert pipeline.state.terminal_forecast.values.loc["series_b"] == 250.0


def test_pipeline_decomposes_base_forecast_seasonality_for_curve() -> None:
    dataset = seasonal_recovery_dataset()

    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    forecast = pipeline.predict()

    assert pipeline.state.seasonal_multipliers is not None
    assert (
        pipeline.state.seasonal_multipliers.loc[6, "series_a"]
        > pipeline.state.seasonal_multipliers.loc[3, "series_a"]
    )
    assert pipeline.state.trend_history is not None
    assert forecast.values.index[-1] == pd.Timestamp("2025-12-01")
    assert np.isclose(forecast.values.loc["2025-12-01", "series_a"], 110.0)


def seasonal_recovery_dataset() -> RecoveryDataset:
    series = pd.DataFrame(
        {
            "series_id": ["series_a"],
            "series_name": ["Series A"],
            "target_name": ["target"],
            "unit": ["count"],
            "coefficient": [1.0],
        }
    )
    observed = pd.DataFrame(
        {"series_a": [100.0]},
        index=pd.to_datetime(["2024-01-01"]),
    )
    reference = pd.DataFrame(
        {"series_a": [100.0]},
        index=pd.to_datetime(["2024-01-01"]),
    )
    month_factors = np.array(
        [1.0, 1.2, 0.8, 1.1, 0.9, 1.3, 1.0, 0.95, 1.05, 0.85, 1.15, 1.0]
    )
    dates = pd.date_range("2024-01-01", periods=24, freq="MS")
    base_forecast = pd.DataFrame(
        {"series_a": 110.0 * np.resize(month_factors, len(dates))},
        index=dates,
    )

    rows = []
    rows.extend(_matrix_rows(observed, kind="observed", name="target"))
    rows.extend(
        _matrix_rows(reference, kind="reference_forecast", name="legacy_average")
    )
    rows.extend(
        _matrix_rows(base_forecast, kind="base_forecast", name="legacy_ensemble")
    )
    config = {
        "frequency": "MS",
        "shock": {"start": "2024-01"},
        "dates": {
            "observed_until": "2024-01",
            "initial_date": "2024-01",
            "forecast_start": "2024-02",
            "terminal_date": "2025-12",
            "forecast_end": "2025-12",
        },
        "curve": {
            "curves": ["linear"],
            "seasonal_period": 12,
            "trend_history_start": "2024-01",
            "trend_history_end": "2024-01",
        },
    }
    return RecoveryDataset(
        series=series,
        panel=pd.DataFrame(rows),
        config=config,
    ).validate()


def _matrix_rows(
    matrix: pd.DataFrame,
    kind: str,
    name: str,
) -> list[dict[str, object]]:
    rows = []
    for date, values in matrix.iterrows():
        for series_id, value in values.items():
            rows.append(
                {
                    "date": date,
                    "series_id": series_id,
                    "kind": kind,
                    "name": name,
                    "value": value,
                    "lower": np.nan,
                    "upper": np.nan,
                }
            )
    return rows
