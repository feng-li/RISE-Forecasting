"""Compute recovery curve forecasts from legacy baseline/reference artifacts."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd
from terminal_forecast import LEGACY_RECOVERY_COEFFICIENTS, read_baseline

from riseforecast import RecoveryCurveForecaster, intervention_terminal_forecast


def read_reference(path: Path, start: str = "2023-01") -> pd.DataFrame:
    frame = pd.read_excel(path)
    frame = frame.iloc[:, 1:]
    frame.index = pd.date_range(start=start, periods=len(frame), freq="MS")
    return frame


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--baseline",
        type=Path,
        default=Path("code and supplementary materials/baseline.xlsx"),
    )
    parser.add_argument(
        "--reference",
        type=Path,
        default=Path("code and supplementary materials/reference.xlsx"),
    )
    parser.add_argument("--initial-date", default="2023-06")
    parser.add_argument("--forecast-start", default="2023-08")
    parser.add_argument("--terminal-date", default="2024-07")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    baseline = read_baseline(args.baseline)
    reference = read_reference(args.reference)
    coefficients = pd.Series(LEGACY_RECOVERY_COEFFICIENTS)
    terminal = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date=args.terminal_date,
    )
    recovery_curve = RecoveryCurveForecaster(
        initial_date=args.initial_date,
        forecast_start=args.forecast_start,
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
