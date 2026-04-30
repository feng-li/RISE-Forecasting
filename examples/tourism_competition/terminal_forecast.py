"""Compute terminal forecasts from the converted tourism dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import RecoveryDataset, intervention_terminal_forecast


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--terminal-date")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    dates = dataset.config.get("dates", {})
    terminal_date = args.terminal_date or dates["terminal_date"]
    terminal = intervention_terminal_forecast(
        dataset.base_forecast(),
        dataset.coefficients(),
        terminal_date=terminal_date,
    ).to_frame()

    if args.output is None:
        print(terminal.to_string(index=False))
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        terminal.to_excel(args.output, index=False)


if __name__ == "__main__":
    main()
