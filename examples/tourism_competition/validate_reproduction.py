"""Validate package outputs against legacy tourism competition artifacts.

The script reads the original files under the example's `legacypapercode/`,
normalizes them to the package's date-by-series matrix format, runs the migrated
package pipeline, and reports stage-by-stage differences.
"""

from __future__ import annotations

import argparse
import re
import sys
from dataclasses import dataclass
from pathlib import Path

import numpy as np
import pandas as pd
from convert_legacy_data import SERIES, excel_serial_to_datetime

from riseforecast import RecoveryDataset, RecoveryForecastingPipeline

DEFAULT_OUTPUT = Path("examples/tourism_competition/reproduction_validation.csv")
DEFAULT_LEGACY_DIR = Path(__file__).resolve().parent / "legacypapercode"
LEGACY_LABEL_PREFIX = "examples/tourism_competition/legacypapercode"
INTERPRETATION_NOTE = (
    "\nInterpretation:\n"
    "- converted_vs_legacy rows should pass; they validate compact data conversion.\n"
    "- package_vs_legacy rows are diagnostic; differences are expected for the "
    "generalized Python implementation unless it consumes the same legacy stage "
    "artifacts.\n"
)


@dataclass(frozen=True)
class MatrixComparison:
    """One matrix-to-matrix reproduction check."""

    group: str
    stage: str
    reference_name: str
    candidate_name: str
    reference: pd.DataFrame
    candidate: pd.DataFrame
    note: str = ""


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument(
        "--legacy-dir",
        type=Path,
        default=DEFAULT_LEGACY_DIR,
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=DEFAULT_OUTPUT,
        help="CSV report path. Use '-' to print to stdout only.",
    )
    parser.add_argument("--atol", type=float, default=1e-6)
    parser.add_argument("--rtol", type=float, default=1e-8)
    parser.add_argument(
        "--fail-on-diff",
        choices=("none", "conversion", "all"),
        default="none",
        help="Exit nonzero when all-close checks fail for selected comparison groups.",
    )
    args = parser.parse_args()

    report = validate_reproduction(
        data_dir=args.data_dir,
        legacy_dir=args.legacy_dir,
        atol=args.atol,
        rtol=args.rtol,
    )
    report = report.sort_values(["group", "stage", "level", "series_id"])

    if str(args.output) == "-":
        print(report.to_string(index=False))
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        report.to_csv(args.output, index=False)
        print(f"Wrote {args.output}")
        print(_summary_table(report).to_string(index=False))
    print(INTERPRETATION_NOTE)

    if _should_fail(report, args.fail_on_diff):
        sys.exit(1)


def validate_reproduction(
    data_dir: Path,
    legacy_dir: Path,
    atol: float = 1e-6,
    rtol: float = 1e-8,
) -> pd.DataFrame:
    """Return stage-by-stage reproduction diagnostics."""

    dataset = RecoveryDataset.from_directory(data_dir)
    legacy = load_legacy_artifacts(legacy_dir, dataset)
    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    package = package_artifacts(pipeline)

    comparisons = converted_artifact_comparisons(dataset, legacy)
    comparisons.extend(package_pipeline_comparisons(package, legacy))

    rows = []
    for comparison in comparisons:
        rows.extend(compare_matrices(comparison, atol=atol, rtol=rtol))
    return pd.DataFrame(rows)


def load_legacy_artifacts(
    legacy_dir: Path,
    dataset: RecoveryDataset,
) -> dict[str, pd.DataFrame]:
    """Load original paper artifacts in canonical matrix form."""

    config = dataset.pipeline_config()
    artifacts = {
        "observed": read_observed_matrix(legacy_dir / "data.xlsx"),
        "search_index": read_composite_search_matrix(
            legacy_dir / "composite_search_index"
        ),
        "flight_capacity": read_flight_matrix(legacy_dir / "flight.xlsx"),
        "base": read_forecast_matrix(
            legacy_dir / "baseline.xlsx",
            columns=forecast_name_to_series_id(),
        ),
        "reference": read_reference_matrix(
            legacy_dir / "reference.xlsx",
            start=config.reference.start if config.reference is not None else None,
        ),
        "recovery_point": read_forecast_matrix(
            legacy_dir / "point_forecast.xlsx",
            columns=forecast_name_to_series_id(),
        ),
        "published_point": read_published_point_forecast(
            legacy_dir / "point_and_interval_forecasts.xlsx"
        ),
    }
    artifacts["terminal"] = terminal_from_legacy_baseline(
        baseline=artifacts["base"],
        dataset=dataset,
        terminal_date=config.terminal_date,
    )

    interval_path = legacy_dir / "point_and_interval_forecasts.xlsx"
    if interval_path.exists():
        lower, upper = read_published_interval(interval_path)
        artifacts["recovery_lower_80"] = lower
        artifacts["recovery_upper_80"] = upper

    logistic_path = legacy_dir / "logistic_forecast.csv"
    if logistic_path.exists():
        artifacts["legacy_logistic_curve"] = read_logistic_curve_component(
            logistic_path,
            start=config.forecast_start,
            end=config.forecast_end,
            frequency=config.frequency,
        )

    return artifacts


def package_artifacts(
    pipeline: RecoveryForecastingPipeline,
) -> dict[str, pd.DataFrame]:
    """Collect fitted package-stage outputs as matrices."""

    if pipeline.state.base_forecast is None:
        raise ValueError("Pipeline did not produce a base forecast.")
    if pipeline.state.reference_forecast is None:
        raise ValueError("Pipeline did not produce a reference forecast.")
    if pipeline.state.terminal_forecast is None:
        raise ValueError("Pipeline did not produce a terminal forecast.")
    if pipeline.state.recovery_curve_forecast is None:
        raise ValueError("Pipeline did not produce a recovery curve forecast.")

    recovery = pipeline.predict()
    terminal = pd.DataFrame(
        [pipeline.state.terminal_forecast.values],
        index=[pipeline.state.terminal_forecast.terminal_date],
    )
    artifacts = {
        "base": pipeline.state.base_forecast.values,
        "reference": pipeline.state.reference_forecast.values,
        "terminal": terminal,
        "recovery_point": recovery.values,
    }
    if recovery.lower is not None:
        artifacts["recovery_lower"] = recovery.lower
    if recovery.upper is not None:
        artifacts["recovery_upper"] = recovery.upper
    if "logistic" in pipeline.state.recovery_curve_forecast.components:
        artifacts["logistic_curve"] = (
            pipeline.state.recovery_curve_forecast.components["logistic"]
        )
    return artifacts


def converted_artifact_comparisons(
    dataset: RecoveryDataset,
    legacy: dict[str, pd.DataFrame],
) -> list[MatrixComparison]:
    """Compare compact converted data rows to direct legacy artifact reads."""

    return [
        MatrixComparison(
            "converted_vs_legacy",
            "data.observed_target",
            legacy_label("data.xlsx"),
            "examples panel observed/target",
            legacy["observed"],
            dataset.observed_target(),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "signal.search_index",
            legacy_label("composite_search_index/*.xlsx"),
            "examples panel signal/search_index",
            legacy["search_index"],
            dataset.signal("search_index"),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "signal.flight_capacity",
            legacy_label("flight.xlsx"),
            "examples panel signal/flight_capacity",
            legacy["flight_capacity"],
            dataset.signal("flight_capacity"),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "stage1.base_forecast",
            legacy_label("baseline.xlsx"),
            "examples panel base_forecast/legacy_ensemble",
            legacy["base"],
            dataset.base_forecast(),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "stage2.reference_forecast",
            legacy_label("reference.xlsx"),
            "examples panel reference_forecast/legacy_average",
            legacy["reference"],
            dataset.reference_forecast(),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "stage2.terminal_forecast",
            "baseline.xlsx * legacy coefficients",
            "examples panel terminal_forecast/intervention_adjusted",
            legacy["terminal"],
            dataset.matrix("terminal_forecast", "intervention_adjusted"),
        ),
        MatrixComparison(
            "converted_vs_legacy",
            "stage3.recovery_forecast",
            legacy_label("point_forecast.xlsx"),
            "examples panel recovery_forecast/legacy_final",
            legacy["recovery_point"],
            dataset.recovery_forecast(),
        ),
        MatrixComparison(
            "legacy_publication_consistency",
            "stage3.published_point_forecast",
            legacy_label("point_forecast.xlsx"),
            "point_and_interval_forecasts.xlsx/point forecasts",
            legacy["recovery_point"],
            legacy["published_point"],
        ),
    ]


def package_pipeline_comparisons(
    package: dict[str, pd.DataFrame],
    legacy: dict[str, pd.DataFrame],
) -> list[MatrixComparison]:
    """Compare package-native stage outputs to paper artifacts."""

    comparisons = [
        MatrixComparison(
            "package_vs_legacy",
            "stage1.base_forecast",
            legacy_label("baseline.xlsx"),
            "RecoveryForecastingPipeline.state.base_forecast",
            legacy["base"],
            package["base"],
            "Expected to differ unless package-native base model configuration "
            "exactly reproduces the paper scripts.",
        ),
        MatrixComparison(
            "package_vs_legacy",
            "stage2.reference_forecast",
            legacy_label("reference.xlsx"),
            "RecoveryForecastingPipeline.state.reference_forecast",
            legacy["reference"],
            package["reference"],
            "Expected to differ where generalized X models replace paper-specific "
            "reference construction.",
        ),
        MatrixComparison(
            "package_vs_legacy",
            "stage2.terminal_forecast",
            "baseline.xlsx * legacy coefficients",
            "RecoveryForecastingPipeline.state.terminal_forecast",
            legacy["terminal"],
            package["terminal"],
        ),
        MatrixComparison(
            "package_vs_legacy",
            "stage3.recovery_point_forecast",
            legacy_label("point_forecast.xlsx"),
            "RecoveryForecastingPipeline.predict().values",
            legacy["recovery_point"],
            package["recovery_point"],
        ),
    ]
    if "recovery_lower_80" in legacy and "recovery_lower" in package:
        comparisons.append(
            MatrixComparison(
                "package_vs_legacy",
                "stage3.recovery_lower_80",
                "point_and_interval_forecasts.xlsx/80% lower",
                "RecoveryForecastingPipeline.predict().lower",
                legacy["recovery_lower_80"],
                package["recovery_lower"],
            )
        )
    if "recovery_upper_80" in legacy and "recovery_upper" in package:
        comparisons.append(
            MatrixComparison(
                "package_vs_legacy",
                "stage3.recovery_upper_80",
                "point_and_interval_forecasts.xlsx/80% upper",
                "RecoveryForecastingPipeline.predict().upper",
                legacy["recovery_upper_80"],
                package["recovery_upper"],
            )
        )
    if "legacy_logistic_curve" in legacy and "logistic_curve" in package:
        comparisons.append(
            MatrixComparison(
                "package_vs_legacy",
                "stage3.logistic_curve_component",
                legacy_label("logistic_forecast.csv"),
                "RecoveryCurveForecast.components['logistic']",
                legacy["legacy_logistic_curve"],
                package["logistic_curve"],
            )
        )
    return comparisons


def compare_matrices(
    comparison: MatrixComparison,
    atol: float,
    rtol: float,
) -> list[dict[str, object]]:
    """Summarize one comparison overall and by series."""

    reference, candidate = align_matrices(comparison.reference, comparison.candidate)
    rows = [
        metric_row(
            comparison=comparison,
            level="overall",
            series_id="__overall__",
            reference=reference,
            candidate=candidate,
            atol=atol,
            rtol=rtol,
        )
    ]
    for series_id in reference.columns:
        rows.append(
            metric_row(
                comparison=comparison,
                level="series",
                series_id=str(series_id),
                reference=reference.loc[:, [series_id]],
                candidate=candidate.loc[:, [series_id]],
                atol=atol,
                rtol=rtol,
            )
        )
    return rows


def metric_row(
    comparison: MatrixComparison,
    level: str,
    series_id: str,
    reference: pd.DataFrame,
    candidate: pd.DataFrame,
    atol: float,
    rtol: float,
) -> dict[str, object]:
    y = reference.to_numpy(dtype=float).ravel()
    y_hat = candidate.to_numpy(dtype=float).ravel()
    mask = np.isfinite(y) & np.isfinite(y_hat)
    y = y[mask]
    y_hat = y_hat[mask]
    if y.size == 0:
        raise ValueError(f"No finite overlap for {comparison.stage}: {series_id}")

    error = y_hat - y
    nonzero = y != 0
    denominator = np.abs(y) + np.abs(y_hat)
    smape_mask = denominator != 0
    max_abs_error = float(np.max(np.abs(error)))
    all_close = bool(np.allclose(y_hat, y, atol=atol, rtol=rtol))
    return {
        "group": comparison.group,
        "stage": comparison.stage,
        "level": level,
        "series_id": series_id,
        "reference": comparison.reference_name,
        "candidate": comparison.candidate_name,
        "start": reference.index.min().date().isoformat(),
        "end": reference.index.max().date().isoformat(),
        "n": int(y.size),
        "mae": float(np.mean(np.abs(error))),
        "rmse": float(np.sqrt(np.mean(error**2))),
        "bias": float(np.mean(error)),
        "mape": (
            float(np.mean(np.abs(error[nonzero] / y[nonzero])))
            if np.any(nonzero)
            else np.nan
        ),
        "smape": (
            float(
                np.mean(
                    2
                    * np.abs(error[smape_mask])
                    / denominator[smape_mask]
                )
            )
            if np.any(smape_mask)
            else np.nan
        ),
        "max_abs_error": max_abs_error,
        "max_abs_pct_error": (
            float(np.max(np.abs(error[nonzero] / y[nonzero])))
            if np.any(nonzero)
            else np.nan
        ),
        "all_close": all_close,
        "status": "pass" if all_close else "diff",
        "note": comparison.note,
    }


def align_matrices(
    reference: pd.DataFrame,
    candidate: pd.DataFrame,
) -> tuple[pd.DataFrame, pd.DataFrame]:
    """Align two date-by-series matrices on common dates and columns."""

    reference_matrix = prepare_matrix(reference)
    candidate_matrix = prepare_matrix(candidate)
    reference_aligned, candidate_aligned = reference_matrix.align(
        candidate_matrix,
        join="inner",
        axis=0,
    )
    reference_aligned, candidate_aligned = reference_aligned.align(
        candidate_aligned,
        join="inner",
        axis=1,
    )
    if reference_aligned.empty or reference_aligned.shape[1] == 0:
        raise ValueError("Matrices have no overlapping dates or series.")
    return reference_aligned, candidate_aligned


def prepare_matrix(matrix: pd.DataFrame) -> pd.DataFrame:
    result = matrix.copy()
    result.index = pd.to_datetime(result.index)
    return result.sort_index().astype(float)


def read_observed_matrix(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    frame = frame.rename(columns={"time": "date"})
    frame["date"] = excel_serial_to_datetime(frame["date"])
    return wide_to_matrix(frame, observed_name_to_series_id())


def read_composite_search_matrix(directory: Path) -> pd.DataFrame:
    parts = []
    for spec in SERIES:
        path = directory / f"{spec.forecast_name}.xlsx"
        frame = pd.read_excel(path)
        parts.append(
            pd.Series(
                frame["composite_search_index"].to_numpy(dtype=float),
                index=pd.to_datetime(frame["date"]),
                name=spec.series_id,
            )
        )
    return pd.concat(parts, axis=1).sort_index()


def read_flight_matrix(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path)
    frame = frame.rename(columns={frame.columns[0]: "date"})
    frame["date"] = pd.to_datetime(frame["date"])
    return wide_to_matrix(frame, flight_name_to_series_id())


def read_forecast_matrix(path: Path, columns: dict[str, str]) -> pd.DataFrame:
    frame = pd.read_excel(path)
    frame = frame.rename(columns={frame.columns[0]: "date"})
    frame["date"] = pd.to_datetime(frame["date"])
    return wide_to_matrix(frame, columns)


def read_reference_matrix(path: Path, start: str | None) -> pd.DataFrame:
    frame = pd.read_excel(path).iloc[:, 1:]
    start_date = "2023-01-01" if start is None else pd.Timestamp(start)
    frame.insert(
        0,
        "date",
        pd.date_range(start_date, periods=len(frame), freq="MS"),
    )
    return wide_to_matrix(frame, forecast_name_to_series_id())


def terminal_from_legacy_baseline(
    baseline: pd.DataFrame,
    dataset: RecoveryDataset,
    terminal_date: str,
) -> pd.DataFrame:
    terminal_timestamp = pd.Timestamp(terminal_date)
    if terminal_timestamp not in baseline.index:
        raise ValueError(f"Legacy baseline is missing terminal date {terminal_date}.")
    coefficients = (
        dataset.series.set_index("series_id")
        .loc[baseline.columns, "coefficient"]
        .astype(float)
    )
    terminal = baseline.loc[[terminal_timestamp], :] * coefficients
    terminal.index = pd.DatetimeIndex([terminal_timestamp])
    return terminal


def read_published_point_forecast(path: Path) -> pd.DataFrame:
    frame = pd.read_excel(path, sheet_name="point forecasts")
    frame = frame.rename(columns={frame.columns[0]: "date"})
    frame["date"] = parse_year_month_labels(frame["date"])
    return wide_to_matrix(frame, published_name_to_series_id())


def read_published_interval(path: Path) -> tuple[pd.DataFrame, pd.DataFrame]:
    frame = pd.read_excel(path, sheet_name="80% interval forecasts", header=[0, 1])
    date_column = frame.columns[0]
    dates = parse_year_month_labels(frame[date_column])
    lower_parts = {}
    upper_parts = {}
    columns = published_name_to_series_id()
    for column_name, bound_name in frame.columns[1:]:
        series_id = columns.get(str(column_name))
        if series_id is None:
            continue
        if bound_name == "lower":
            lower_parts[series_id] = frame[(column_name, bound_name)].to_numpy(
                dtype=float
            )
        elif bound_name == "upper":
            upper_parts[series_id] = frame[(column_name, bound_name)].to_numpy(
                dtype=float
            )
    lower = pd.DataFrame(lower_parts, index=dates).sort_index()
    upper = pd.DataFrame(upper_parts, index=dates).sort_index()
    return lower.reindex(columns=series_ids()), upper.reindex(columns=series_ids())


def read_logistic_curve_component(
    path: Path,
    start: str,
    end: str,
    frequency: str,
) -> pd.DataFrame:
    frame = pd.read_csv(path, encoding="gbk")
    dates = pd.date_range(start, end, freq=frequency)
    if len(dates) != len(frame):
        raise ValueError(
            "logistic_forecast.csv row count does not match configured forecast dates."
        )
    frame.insert(0, "date", dates)
    return wide_to_matrix(frame, logistic_name_to_series_id())


def wide_to_matrix(frame: pd.DataFrame, columns: dict[str, str]) -> pd.DataFrame:
    available = {}
    for column, series_id in columns.items():
        if column in frame.columns and series_id not in available.values():
            available[column] = series_id
    covered = set(available.values())
    missing = [series_id for series_id in series_ids() if series_id not in covered]
    if missing:
        raise ValueError(f"Missing legacy series: {', '.join(missing)}")
    matrix = frame.loc[:, ["date", *available]].rename(columns=available)
    matrix = matrix.set_index("date")
    matrix.index = pd.to_datetime(matrix.index)
    return matrix.sort_index().reindex(columns=series_ids())


def parse_year_month_labels(values: pd.Series) -> pd.DatetimeIndex:
    dates = []
    for value in values:
        match = re.fullmatch(r"(\d{4})m(\d{1,2})", str(value).strip())
        if match is None:
            raise ValueError(f"Invalid year-month label: {value!r}")
        year, month = match.groups()
        dates.append(pd.Timestamp(year=int(year), month=int(month), day=1))
    return pd.DatetimeIndex(dates)


def series_ids() -> list[str]:
    return [spec.series_id for spec in SERIES]


def legacy_label(path: str) -> str:
    """Return a display label for an artifact in the example legacy directory."""

    return f"{LEGACY_LABEL_PREFIX}/{path}"


def observed_name_to_series_id() -> dict[str, str]:
    return {spec.observed_name: spec.series_id for spec in SERIES}


def forecast_name_to_series_id() -> dict[str, str]:
    return {spec.forecast_name: spec.series_id for spec in SERIES}


def flight_name_to_series_id() -> dict[str, str]:
    return {spec.flight_name: spec.series_id for spec in SERIES}


def published_name_to_series_id() -> dict[str, str]:
    mapping = observed_name_to_series_id()
    mapping.update({spec.series_name: spec.series_id for spec in SERIES})
    mapping.update(
        {
            "Hong Kong, China": "hong_kong",
            "Macao, China": "macao",
        }
    )
    return mapping


def logistic_name_to_series_id() -> dict[str, str]:
    mapping = forecast_name_to_series_id()
    mapping.update(
        {
            "台北": "chinese_taipei",
            "印度尼西亚": "indonesia",
        }
    )
    return mapping


def _summary_table(report: pd.DataFrame) -> pd.DataFrame:
    return report.loc[
        report["level"] == "overall",
        ["group", "stage", "status", "n", "mae", "rmse", "max_abs_error", "note"],
    ]


def _should_fail(report: pd.DataFrame, mode: str) -> bool:
    if mode == "none":
        return False
    overall = report.loc[report["level"] == "overall"]
    if mode == "conversion":
        overall = overall.loc[overall["group"] == "converted_vs_legacy"]
    return bool((overall["status"] != "pass").any())


if __name__ == "__main__":
    main()
