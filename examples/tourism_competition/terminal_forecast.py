"""Compute paper-style terminal forecasts from the legacy baseline artifact."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from riseforecast import intervention_terminal_forecast

LEGACY_RECOVERY_COEFFICIENTS = {
    "加拿大": 0.70,
    "智利": 0.70,
    "墨西哥": 1.00,
    "台湾": 0.60,
    "香港": 0.85,
    "日本": 0.80,
    "韩国": 0.80,
    "澳门": 0.85,
    "马尔代夫": 0.80,
    "柬埔寨": 0.80,
    "印尼": 0.80,
    "新加坡": 0.80,
    "新西兰": 0.65,
    "美国": 0.65,
    "泰国": 0.85,
    "土耳其": 0.75,
    "澳大利亚": 0.80,
    "夏威夷": 0.80,
    "奥地利": 0.65,
    "捷克": 0.65,
}


def read_baseline(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    date_column = frame.columns[0]
    frame = frame.rename(columns={date_column: "date"}).set_index("date")
    frame.index = pd.to_datetime(frame.index)
    return frame


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--baseline",
        type=Path,
        default=Path("code and supplementary materials/baseline.xlsx"),
    )
    parser.add_argument("--terminal-date", default="2024-07")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    baseline = read_baseline(args.baseline)
    coefficients = pd.Series(LEGACY_RECOVERY_COEFFICIENTS)
    terminal = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date=args.terminal_date,
    ).to_frame()

    if args.output is None:
        print(terminal.to_string(index=False))
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        terminal.to_excel(args.output, index=False)


if __name__ == "__main__":
    main()
