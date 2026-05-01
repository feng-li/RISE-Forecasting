from pathlib import Path

import pandas as pd
import yaml

from riseforecast import RecoveryDataset

DATA_DIR = Path("examples/tourism_competition/data")


def test_converted_tourism_data_files_exist() -> None:
    assert (DATA_DIR / "series.csv").exists()
    assert (DATA_DIR / "panel.csv").exists()
    assert (DATA_DIR / "config.yaml").exists()


def test_converted_tourism_data_uses_compact_schema() -> None:
    series = pd.read_csv(DATA_DIR / "series.csv")
    panel = pd.read_csv(DATA_DIR / "panel.csv")
    with (DATA_DIR / "config.yaml").open("r", encoding="utf-8") as handle:
        config = yaml.safe_load(handle)

    assert len(series) == 20
    assert series["series_id"].is_unique
    assert set(panel.columns) == {
        "date",
        "series_id",
        "kind",
        "name",
        "value",
        "lower",
        "upper",
    }
    assert set(series["series_id"]) == set(panel["series_id"])
    assert set(config["kinds"]) <= set(panel["kind"])
    assert config["base"]["train_end"] == "2019-12"
    assert config["base"]["horizon"] == 60
    assert "arima" in config["base"]["models"]
    assert config["recovery"]["method"] == "regression"
    assert config["recovery"]["score_columns"] == ["policy", "distance", "recovery"]
    assert set(config["recovery"]["anchors"]) == {"canada", "mexico", "hong_kong"}
    assert config["curve"]["trend_history_start"] == "2022-01"
    assert "2024-12" in config["curve"]["logistic_anchor_dates"]
    assert len(panel) == 10188


def test_recovery_dataset_loads_converted_tourism_data() -> None:
    dataset = RecoveryDataset.from_directory(DATA_DIR)

    observed = dataset.observed_target()
    baseline = dataset.base_forecast()
    reference = dataset.reference_forecast()
    coefficients = dataset.coefficients()

    assert observed.shape[1] == 20
    assert baseline.shape == (24, 20)
    assert reference.shape == (6, 20)
    assert coefficients.index.tolist() == baseline.columns.tolist()
    assert coefficients.loc["canada"] == 0.7


def test_recovery_dataset_filters_and_forecast_frame() -> None:
    dataset = RecoveryDataset.from_directory(DATA_DIR)

    search = dataset.filter_panel(kind="signal", name="search_index")
    frame = dataset.forecast_frame(kind="base_forecast", name="legacy_ensemble")
    signals = dataset.signals()
    exogenous = dataset.exogenous_variables()

    assert not search.empty
    assert set(signals) == {"flight_capacity", "search_index"}
    assert set(exogenous) == {"flight_capacity", "search_index"}
    assert frame.values.shape == (24, 20)
    assert frame.lower is None
    assert frame.upper is None
