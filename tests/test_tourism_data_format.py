from pathlib import Path

import pandas as pd
import yaml

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
    assert len(panel) == 10188
