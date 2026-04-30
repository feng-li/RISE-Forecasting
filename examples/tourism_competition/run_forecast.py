"""Run the migrated tourism recovery forecast from the compact dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import (
    InitialForecaster,
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
    parser.add_argument(
        "--initial-source",
        choices=("base_models", "legacy_reference"),
        default="base_models",
    )
    parser.add_argument(
        "--initial-models",
        default="seasonal_naive,random_walk_drift,arima,ets",
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
    if args.initial_source == "legacy_reference":
        initial = dataset.reference_forecast()
    else:
        models = tuple(
            model.strip() for model in args.initial_models.split(",") if model.strip()
        )
        initial = InitialForecaster(
            initial_date=dates["initial_date"],
            train_end=dates.get("observed_until"),
            models=models,
            frequency=dataset.config.get("frequency", "MS"),
        ).forecast(dataset.observed_target()).values
    forecast = RecoveryCurveForecaster(
        initial_date=dates["initial_date"],
        forecast_start=dates["forecast_start"],
        forecast_end=dates["forecast_end"],
    ).forecast(
        initial_forecast=initial,
        terminal_forecast=terminal,
    )

    if args.output is None:
        print(forecast.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        forecast.values.to_excel(args.output)


if __name__ == "__main__":
    main()
