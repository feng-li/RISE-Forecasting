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
