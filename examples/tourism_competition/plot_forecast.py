"""Create an interactive Plotly recovery forecast figure."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import (
    RecoveryDataset,
    RecoveryForecastingPipeline,
    plot_recovery_curve,
)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument(
        "--entities",
        nargs="+",
        default=["canada", "mexico", "hong_kong"],
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("examples/tourism_competition/recovery_forecast.html"),
    )
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    if pipeline.state.recovery_curve_forecast is None:
        raise RuntimeError("Pipeline did not produce a recovery curve forecast.")

    figure = plot_recovery_curve(
        pipeline.state.recovery_curve_forecast,
        observed=dataset.observed_target(),
        entities=tuple(args.entities),
        title="Tourism recovery forecast",
        value_name="Outbound tourism",
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    figure.write_html(args.output)
    print(f"Wrote {args.output}")


if __name__ == "__main__":
    main()
