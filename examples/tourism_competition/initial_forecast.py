"""Compute model-based initial forecasts from the converted tourism dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import InitialForecaster, RecoveryDataset


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--initial-date")
    parser.add_argument("--train-end")
    parser.add_argument(
        "--models",
        default="seasonal_naive,random_walk_drift,arima,ets",
    )
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    dates = dataset.config.get("dates", {})
    initial_date = args.initial_date or dates["initial_date"]
    train_end = args.train_end or dates.get("observed_until")
    models = tuple(model.strip() for model in args.models.split(",") if model.strip())
    forecast = InitialForecaster(
        initial_date=initial_date,
        train_end=train_end,
        models=models,
        frequency=dataset.config.get("frequency", "MS"),
    ).forecast(dataset.observed_target())

    if args.output is None:
        print(forecast.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        forecast.values.to_excel(args.output)


if __name__ == "__main__":
    main()
