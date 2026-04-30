"""Compute recovery curve forecasts from the converted tourism dataset."""

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
    parser.add_argument("--initial-date")
    parser.add_argument("--forecast-start")
    parser.add_argument("--forecast-end")
    parser.add_argument("--terminal-date")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    dates = dataset.config.get("dates", {})
    initial_date = args.initial_date or dates["initial_date"]
    forecast_start = args.forecast_start or dates["forecast_start"]
    forecast_end = args.forecast_end or dates["forecast_end"]
    terminal_date = args.terminal_date or dates["terminal_date"]

    baseline = dataset.base_forecast()
    reference = dataset.reference_forecast()
    coefficients = dataset.coefficients()
    terminal = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date=terminal_date,
    )
    recovery_curve = RecoveryCurveForecaster(
        initial_date=initial_date,
        forecast_start=forecast_start,
        forecast_end=forecast_end,
    ).forecast(
        initial_forecast=reference,
        terminal_forecast=terminal,
    )

    if args.output is None:
        print(recovery_curve.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        recovery_curve.values.to_excel(args.output)


if __name__ == "__main__":
    main()
