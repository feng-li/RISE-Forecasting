"""Convert legacy tourism competition artifacts to the compact panel format."""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path

import pandas as pd
import yaml

from riseforecast import intervention_terminal_forecast


@dataclass(frozen=True)
class SeriesSpec:
    series_id: str
    series_name: str
    observed_name: str
    forecast_name: str
    flight_name: str
    group: str
    subgroup: str
    policy: int
    distance: int
    recovery: int
    coefficient: float


SERIES = (
    SeriesSpec(
        "canada",
        "Canada",
        "Canada",
        "加拿大",
        "加拿大",
        "America",
        "",
        3,
        1,
        2,
        0.70,
    ),
    SeriesSpec(
        "chile",
        "Chile",
        "Chile",
        "智利",
        "智利",
        "America",
        "",
        3,
        1,
        3,
        0.70,
    ),
    SeriesSpec(
        "mexico",
        "Mexico",
        "Mexico",
        "墨西哥",
        "墨西哥",
        "America",
        "",
        5,
        1,
        5,
        1.00,
    ),
    SeriesSpec(
        "chinese_taipei",
        "Chinese Taipei",
        "Chinese Taipei",
        "台湾",
        "台北",
        "East Asia",
        "",
        1,
        5,
        1,
        0.60,
    ),
    SeriesSpec(
        "hong_kong",
        "Hong Kong SAR",
        "Hong Kong SAR",
        "香港",
        "香港",
        "East Asia",
        "",
        5,
        5,
        3,
        0.85,
    ),
    SeriesSpec(
        "japan",
        "Japan",
        "Japan",
        "日本",
        "日本",
        "East Asia",
        "",
        4,
        5,
        2,
        0.80,
    ),
    SeriesSpec(
        "korea_rok",
        "Korea (ROK)",
        "Korea (ROK)",
        "韩国",
        "韩国",
        "East Asia",
        "",
        4,
        5,
        2,
        0.80,
    ),
    SeriesSpec(
        "macao",
        "Macao, China",
        "Macao, China",
        "澳门",
        "澳门",
        "East Asia",
        "",
        5,
        5,
        3,
        0.85,
    ),
    SeriesSpec(
        "maldives",
        "Maldives",
        "Maldives",
        "马尔代夫",
        "马尔代夫",
        "Southeast Asia",
        "",
        4,
        3,
        5,
        0.80,
    ),
    SeriesSpec(
        "cambodia",
        "Cambodia",
        "Cambodia",
        "柬埔寨",
        "柬埔寨",
        "Southeast Asia",
        "",
        5,
        3,
        3,
        0.80,
    ),
    SeriesSpec(
        "indonesia",
        "Indonesia",
        "Indonesia",
        "印尼",
        "印度尼西亚",
        "Southeast Asia",
        "",
        4,
        3,
        4,
        0.80,
    ),
    SeriesSpec(
        "singapore",
        "Singapore",
        "Singapore",
        "新加坡",
        "新加坡",
        "Southeast Asia",
        "",
        4,
        3,
        3,
        0.80,
    ),
    SeriesSpec(
        "new_zealand",
        "New Zealand",
        "New Zealand",
        "新西兰",
        "新西兰",
        "Pacific",
        "",
        3,
        1,
        3,
        0.65,
    ),
    SeriesSpec(
        "usa",
        "USA",
        "USA",
        "美国",
        "美国",
        "America",
        "",
        2,
        1,
        3,
        0.65,
    ),
    SeriesSpec(
        "thailand",
        "Thailand",
        "Thailand",
        "泰国",
        "泰国",
        "Southeast Asia",
        "",
        5,
        3,
        4,
        0.85,
    ),
    SeriesSpec(
        "turkey",
        "Turkey",
        "Turkey",
        "土耳其",
        "土耳其",
        "West Asia",
        "",
        4,
        2,
        3,
        0.75,
    ),
    SeriesSpec(
        "australia",
        "Australia",
        "Australia",
        "澳大利亚",
        "澳大利亚",
        "Pacific",
        "",
        4,
        2,
        3,
        0.80,
    ),
    SeriesSpec(
        "hawaii",
        "Hawaii",
        "Hawaii",
        "夏威夷",
        "夏威夷",
        "Pacific",
        "",
        3,
        2,
        4,
        0.80,
    ),
    SeriesSpec(
        "austria",
        "Austria",
        "Austria",
        "奥地利",
        "奥地利",
        "Europe",
        "",
        2,
        2,
        2,
        0.65,
    ),
    SeriesSpec(
        "czech",
        "Czech Republic",
        "Czech",
        "捷克",
        "捷克",
        "Europe",
        "",
        2,
        2,
        2,
        0.65,
    ),
)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--legacy-dir",
        type=Path,
        default=Path("legacypapercode"),
    )
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    args = parser.parse_args()

    args.output_dir.mkdir(parents=True, exist_ok=True)
    series = build_series()
    panel = build_panel(args.legacy_dir)
    config = build_config()

    series.to_csv(args.output_dir / "series.csv", index=False)
    panel.to_csv(args.output_dir / "panel.csv", index=False)
    with (args.output_dir / "config.yaml").open("w", encoding="utf-8") as handle:
        yaml.safe_dump(config, handle, sort_keys=False, allow_unicode=True)

    print(f"Wrote {len(series)} series rows")
    print(f"Wrote {len(panel)} panel rows")
    print(f"Output directory: {args.output_dir}")


def build_series() -> pd.DataFrame:
    rows = []
    for spec in SERIES:
        rows.append(
            {
                "series_id": spec.series_id,
                "series_name": spec.series_name,
                "target_name": "outbound_tourists",
                "unit": "count",
                "group": spec.group,
                "subgroup": spec.subgroup,
                "policy": spec.policy,
                "distance": spec.distance,
                "recovery": spec.recovery,
                "coefficient": spec.coefficient,
                "legacy_observed_name": spec.observed_name,
                "legacy_forecast_name": spec.forecast_name,
                "legacy_flight_name": spec.flight_name,
            }
        )
    return pd.DataFrame(rows)


def build_panel(legacy_dir: Path) -> pd.DataFrame:
    parts = [
        observed_rows(legacy_dir / "data.xlsx"),
        search_signal_rows(legacy_dir / "composite_search_index"),
        flight_signal_rows(legacy_dir / "flight.xlsx"),
        matrix_rows(
            legacy_dir / "baseline.xlsx",
            kind="base_forecast",
            name="legacy_ensemble",
        ),
        reference_rows(legacy_dir / "reference.xlsx"),
        terminal_rows(legacy_dir / "baseline.xlsx"),
        matrix_rows(
            legacy_dir / "point_forecast.xlsx",
            kind="recovery_forecast",
            name="legacy_final",
        ),
    ]
    panel = pd.concat(parts, ignore_index=True)
    panel = panel.dropna(subset=["value"])
    panel["date"] = pd.to_datetime(panel["date"]).dt.strftime("%Y-%m-%d")
    panel = panel.sort_values(["series_id", "date", "kind", "name"])
    return panel.loc[
        :,
        ["date", "series_id", "kind", "name", "value", "lower", "upper"],
    ]


def observed_rows(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    frame = frame.rename(columns={"time": "date"})
    frame["date"] = excel_serial_to_datetime(frame["date"])
    return melt_wide(
        frame,
        columns={spec.observed_name: spec.series_id for spec in SERIES},
        kind="observed",
        name="target",
    )


def search_signal_rows(directory: Path) -> pd.DataFrame:
    rows = []
    for spec in SERIES:
        path = directory / f"{spec.forecast_name}.xlsx"
        if not path.exists():
            continue
        frame = pd.read_excel(path)
        rows.append(
            pd.DataFrame(
                {
                    "date": pd.to_datetime(frame["date"]),
                    "series_id": spec.series_id,
                    "kind": "signal",
                    "name": "search_index",
                    "value": frame["composite_search_index"],
                    "lower": pd.NA,
                    "upper": pd.NA,
                }
            )
        )
    return pd.concat(rows, ignore_index=True)


def flight_signal_rows(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    date_column = frame.columns[0]
    frame = frame.rename(columns={date_column: "date"})
    frame["date"] = pd.to_datetime(frame["date"])
    return melt_wide(
        frame,
        columns={spec.flight_name: spec.series_id for spec in SERIES},
        kind="signal",
        name="flight_capacity",
    )


def reference_rows(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path).iloc[:, 1:]
    frame.index = pd.date_range("2023-01-01", periods=len(frame), freq="MS")
    frame = frame.reset_index(names="date")
    return melt_wide(
        frame,
        columns={spec.forecast_name: spec.series_id for spec in SERIES},
        kind="reference_forecast",
        name="legacy_average",
    )


def terminal_rows(path: Path) -> pd.DataFrame:
    baseline = read_forecast_matrix(path)
    coefficients = pd.Series(
        {spec.forecast_name: spec.coefficient for spec in SERIES},
        dtype=float,
    )
    terminal = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date="2024-07",
    ).values
    rows = []
    forecast_to_id = {spec.forecast_name: spec.series_id for spec in SERIES}
    for legacy_name, value in terminal.items():
        rows.append(
            {
                "date": "2024-07-01",
                "series_id": forecast_to_id[legacy_name],
                "kind": "terminal_forecast",
                "name": "intervention_adjusted",
                "value": value,
                "lower": pd.NA,
                "upper": pd.NA,
            }
        )
    return pd.DataFrame(rows)


def matrix_rows(path: Path, kind: str, name: str) -> pd.DataFrame:
    frame = read_forecast_matrix(path).reset_index(names="date")
    return melt_wide(
        frame,
        columns={spec.forecast_name: spec.series_id for spec in SERIES},
        kind=kind,
        name=name,
    )


def read_forecast_matrix(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    date_column = frame.columns[0]
    frame = frame.rename(columns={date_column: "date"}).set_index("date")
    frame.index = pd.to_datetime(frame.index)
    return frame


def melt_wide(
    frame: pd.DataFrame,
    columns: dict[str, str],
    kind: str,
    name: str,
) -> pd.DataFrame:
    available = {
        column: series_id
        for column, series_id in columns.items()
        if column in frame
    }
    missing = sorted(set(columns) - set(available))
    if missing:
        joined = ", ".join(missing)
        raise ValueError(f"Missing columns in source data: {joined}")

    selected = frame.loc[:, ["date", *available.keys()]]
    long = selected.melt(id_vars="date", var_name="legacy_name", value_name="value")
    long["series_id"] = long["legacy_name"].map(available)
    long["kind"] = kind
    long["name"] = name
    long["lower"] = pd.NA
    long["upper"] = pd.NA
    return long.drop(columns=["legacy_name"])


def excel_serial_to_datetime(values: pd.Series) -> pd.Series:
    return pd.to_datetime(values, unit="D", origin="1899-12-30")


def build_config() -> dict:
    return {
        "frequency": "MS",
        "shock": {
            "start": "2020-01",
            "end": "2022-12",
        },
        "dates": {
            "observed_until": "2023-01",
            "initial_date": "2023-06",
            "forecast_start": "2023-08",
            "terminal_date": "2024-07",
            "forecast_end": "2024-07",
        },
        "reference": {
            "start": "2023-01",
            "end": "2023-06",
            "train_end": "2023-01",
            "x": [
                {
                    "name": "search_arimax",
                    "variables": ["search_index"],
                    "method": "arimax",
                    "lag": 1,
                },
                {
                    "name": "search_ratio",
                    "variables": ["search_index"],
                    "method": "ratio",
                    "lag": 1,
                    "ratio_window": 36,
                },
                {
                    "name": "flight_growth",
                    "variables": ["flight_capacity"],
                    "method": "growth_rate",
                },
            ],
        },
        "columns": {
            "date": "date",
            "series_id": "series_id",
            "kind": "kind",
            "name": "name",
            "value": "value",
            "lower": "lower",
            "upper": "upper",
        },
        "kinds": [
            "observed",
            "signal",
            "base_forecast",
            "reference_forecast",
            "terminal_forecast",
            "recovery_forecast",
        ],
        "notes": [
            "series_id is the general quantity of interest; in this example it "
            "maps to tourism destinations.",
            "coefficient values in series.csv follow the legacy notebook "
            "implementation.",
            "reference_forecast rows are dated January through June 2023 to "
            "match the legacy reference.xlsx artifact.",
        ],
    }


if __name__ == "__main__":
    main()
