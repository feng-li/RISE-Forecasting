"""Compute external-signal reference forecasts from the converted tourism dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import (
    RecoveryDataset,
    ReferenceForecaster,
    reference_specs_from_config,
)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--start")
    parser.add_argument("--end")
    parser.add_argument("--train-end")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    dates = dataset.config.get("dates", {})
    start = args.start or dates.get("observed_until")
    end = args.end or dates["initial_date"]
    train_end = args.train_end or dates.get("observed_until")

    forecast = ReferenceForecaster(
        start=start,
        end=end,
        train_end=train_end,
        frequency=dataset.config.get("frequency", "MS"),
        specs=reference_specs_from_config(dataset.config),
    ).forecast(
        observed=dataset.observed_target(),
        signals=dataset.exogenous_variables(),
    )

    if args.output is None:
        print(forecast.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        forecast.values.to_excel(args.output)


if __name__ == "__main__":
    main()
