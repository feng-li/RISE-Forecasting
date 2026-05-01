import pandas as pd
import yaml

from riseforecast import PipelineConfig, RecoveryDataset


def test_pipeline_config_from_dict_parses_dataset_config() -> None:
    config = PipelineConfig.from_dict(compact_config())

    assert config.frequency == "MS"
    assert config.shock_start == "2024-01"
    assert config.initial_date == "2024-03"
    assert config.reference is not None
    assert config.reference.x[0].name == "search_growth"
    assert config.reference.x[0].variables == ("search_index",)
    assert config.reference.x[0].method == "growth_rate"
    assert config.base is not None
    assert config.base.ensemble == "ridge"
    assert config.base.validation_start == "2024-01"
    assert config.base.validation_metric == "mae"
    assert config.base.selection_fraction == 1.0
    assert config.hierarchy.enabled is True
    assert config.hierarchy.method == "bottom_up"
    assert config.hierarchy.parent_column == "parent_id"
    assert config.hierarchy.apply_to == ("recovery",)
    assert config.recovery.method == "regression"
    assert config.recovery.score_columns == ("policy", "distance", "recovery")
    assert config.recovery.weights == {
        "policy": 2.0,
        "distance": 1.0,
        "recovery": 1.0,
    }
    assert config.recovery.anchors == {"series_a": 0.5, "series_b": 1.0}
    assert config.curve.curves == ("linear",)
    assert config.curve.quadratic_terminal_weight == 18.0
    assert config.interval.enabled is True
    assert config.interval.alpha == 0.1
    assert config.interval.method == "residual_quantile"


def test_pipeline_config_from_yaml(tmp_path) -> None:
    config_path = tmp_path / "config.yaml"
    config_path.write_text(yaml.safe_dump(compact_config()), encoding="utf-8")

    config = PipelineConfig.from_yaml(config_path)

    assert config.forecast_start == "2024-04"
    assert config.forecast_end == "2024-05"


def test_recovery_dataset_exposes_pipeline_config() -> None:
    dataset = RecoveryDataset(
        series=_series_frame(),
        panel=_panel_frame(),
        config=compact_config(),
    ).validate()

    config = dataset.pipeline_config()

    assert config.terminal_date == "2024-05"
    assert config.reference is not None


def compact_config() -> dict[str, object]:
    return {
        "frequency": "MS",
        "shock": {"start": "2024-01"},
        "dates": {
            "observed_until": "2024-02",
            "initial_date": "2024-03",
            "forecast_start": "2024-04",
            "terminal_date": "2024-05",
            "forecast_end": "2024-05",
        },
        "base": {
            "train_end": "2024-02",
            "validation_start": "2024-01",
            "validation_end": "2024-02",
            "models": ["random_walk_drift"],
            "ensemble": "ridge",
            "selection_fraction": 1.0,
            "validation_metric": "mae",
            "stacking_alpha": 0.5,
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
        "recovery": {
            "method": "regression",
            "score_columns": ["policy", "distance", "recovery"],
            "weights": {"policy": 2.0, "distance": 1.0, "recovery": 1.0},
            "anchors": {"series_a": 0.5, "series_b": 1.0},
        },
        "hierarchy": {
            "enabled": True,
            "method": "bottom_up",
            "parent_column": "parent_id",
            "apply_to": ["recovery"],
        },
        "curve": {"curves": ["linear"]},
        "interval": {"enabled": True, "alpha": 0.1},
    }


def _series_frame() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "series_id": ["series_a"],
            "series_name": ["Series A"],
            "target_name": ["target"],
            "unit": ["count"],
            "coefficient": [0.5],
        }
    )


def _panel_frame() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "date": ["2024-01-01"],
            "series_id": ["series_a"],
            "kind": ["observed"],
            "name": ["target"],
            "value": [100.0],
            "lower": [None],
            "upper": [None],
        }
    )
