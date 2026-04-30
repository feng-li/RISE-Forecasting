"""Entry point placeholder for reproducing the tourism competition example."""

from __future__ import annotations

from pathlib import Path

import yaml

from riseforecast import PipelineConfig, RecoveryForecastingPipeline


def load_config(path: Path) -> dict:
    with path.open("r", encoding="utf-8") as handle:
        return yaml.safe_load(handle)


def main() -> None:
    config_path = Path(__file__).with_name("config.yaml")
    raw_config = load_config(config_path)
    config = PipelineConfig(
        shock_start=raw_config["shock_start"],
        initial_date=raw_config["initial_date"],
        terminal_date=raw_config["terminal_date"],
        forecast_start=raw_config["forecast_start"],
        forecast_end=raw_config["forecast_end"],
        frequency=raw_config.get("frequency", "MS"),
    )
    pipeline = RecoveryForecastingPipeline(config)
    raise SystemExit(
        "Package skeleton is ready. Implement tourism data loading and stage "
        f"ports before running {pipeline.__class__.__name__} end to end."
    )


if __name__ == "__main__":
    main()
