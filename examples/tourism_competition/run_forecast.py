"""Run the migrated tourism recovery forecast from the compact dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import (
    RecoveryCurveForecaster,
    RecoveryDataset,
    intervention_terminal_forecast,
)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    dates = dataset.config["dates"]
    terminal = intervention_terminal_forecast(
        dataset.base_forecast(),
        dataset.coefficients(),
        terminal_date=dates["terminal_date"],
    )
    forecast = RecoveryCurveForecaster(
        initial_date=dates["initial_date"],
        forecast_start=dates["forecast_start"],
        forecast_end=dates["forecast_end"],
    ).forecast(
        initial_forecast=dataset.reference_forecast(),
        terminal_forecast=terminal,
    )

    if args.output is None:
        print(forecast.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        forecast.values.to_excel(args.output)


if __name__ == "__main__":
    main()
