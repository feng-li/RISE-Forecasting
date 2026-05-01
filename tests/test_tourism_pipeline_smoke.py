from __future__ import annotations

from copy import deepcopy
from pathlib import Path

import numpy as np
import pandas as pd

from riseforecast import RecoveryDataset, RecoveryForecastingPipeline

DATA_DIR = Path("examples/tourism_competition/data")
SMOKE_SERIES = ("canada", "mexico", "hong_kong")


def test_tourism_package_native_pipeline_smoke() -> None:
    dataset = tourism_smoke_dataset()

    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    forecast = pipeline.predict()

    config = pipeline.config
    expected_dates = pd.date_range(
        config.forecast_start,
        config.forecast_end,
        freq=config.frequency,
    )

    assert forecast.values.index.equals(expected_dates)
    assert forecast.values.columns.tolist() == list(SMOKE_SERIES)
    assert np.isfinite(forecast.values.to_numpy(dtype=float)).all()
    assert (forecast.values >= 0).all().all()

    assert pipeline.state.base_forecast is not None
    assert pipeline.state.base_validation_errors is not None
    assert pipeline.state.base_selected_models
    assert pipeline.state.reference_forecast is not None
    assert pipeline.state.recovery_coefficients is not None
    assert pipeline.state.recovery_coefficients.index.tolist() == list(SMOKE_SERIES)
    assert pipeline.state.terminal_forecast is not None
    assert pipeline.state.recovery_curve_forecast is not None
    assert pipeline.state.seasonal_multipliers is not None
    assert pipeline.state.trend_history is not None

    if config.interval.enabled:
        assert forecast.lower is not None
        assert forecast.upper is not None
        assert forecast.lower.index.equals(expected_dates)
        assert forecast.upper.index.equals(expected_dates)
        assert forecast.lower.columns.tolist() == list(SMOKE_SERIES)
        assert forecast.upper.columns.tolist() == list(SMOKE_SERIES)
        assert np.isfinite(forecast.lower.to_numpy(dtype=float)).all()
        assert np.isfinite(forecast.upper.to_numpy(dtype=float)).all()
        assert (forecast.lower <= forecast.values).all().all()
        assert (forecast.values <= forecast.upper).all().all()


def tourism_smoke_dataset() -> RecoveryDataset:
    dataset = RecoveryDataset.from_directory(DATA_DIR)
    series = dataset.series.loc[
        dataset.series["series_id"].isin(SMOKE_SERIES)
    ].copy()
    panel = dataset.panel.loc[
        dataset.panel[dataset.schema.series_id].isin(SMOKE_SERIES)
    ].copy()
    config = deepcopy(dataset.config)
    config["base"] = {
        **config["base"],
        "models": ["seasonal_naive", "random_walk_drift"],
        "selection_fraction": 1.0,
    }
    config["interval"] = {
        "enabled": True,
        "alpha": 0.2,
        "method": "residual_quantile",
    }
    return RecoveryDataset(series=series, panel=panel, config=config).validate()
