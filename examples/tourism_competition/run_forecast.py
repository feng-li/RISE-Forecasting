"""Run the migrated tourism recovery forecast from the compact dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import RecoveryDataset, RecoveryForecastingPipeline


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
    forecast = (
        RecoveryForecastingPipeline.from_dataset(dataset)
        .fit_dataset(dataset)
        .predict()
    )

    if args.output is None:
        print(forecast.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        if args.output.suffix.lower() == ".csv":
            forecast.values.to_csv(args.output)
        else:
            forecast.values.to_excel(args.output)


if __name__ == "__main__":
    main()
