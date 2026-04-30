"""Evaluate migrated tourism forecasts against converted legacy artifacts."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from riseforecast import (
    RecoveryDataset,
    RecoveryForecastingPipeline,
    evaluate_forecast_matrix,
)

DEFAULT_METRICS = "mae,rmse,mape,smape,bias,mase,rmsse"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--metrics", default=DEFAULT_METRICS)
    parser.add_argument("--seasonality", type=int, default=12)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    report = evaluate_migration(
        dataset=dataset,
        metrics=tuple(
            metric.strip() for metric in args.metrics.split(",") if metric.strip()
        ),
        seasonality=args.seasonality,
    )

    if args.output is None:
        print(report.to_string(index=False))
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        report.to_csv(args.output, index=False)


def evaluate_migration(
    dataset: RecoveryDataset,
    metrics: tuple[str, ...] = (
        "mae",
        "rmse",
        "mape",
        "smape",
        "bias",
        "mase",
        "rmsse",
    ),
    seasonality: int = 12,
) -> pd.DataFrame:
    """Build the configured package pipeline and compare it with legacy outputs."""

    observed = dataset.observed_target()
    train_end = _evaluation_train_end(dataset)
    observed_train = observed.loc[:train_end]

    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    base_forecast = _require_forecast_frame(
        pipeline.state.base_forecast,
        stage_name="base",
    )
    reference_forecast = _require_forecast_frame(
        pipeline.state.reference_forecast,
        stage_name="reference",
    )
    terminal_forecast = pipeline.state.terminal_forecast
    if terminal_forecast is None:
        raise ValueError("Pipeline did not produce a terminal forecast.")
    recovery_forecast = pipeline.predict().values

    terminal_matrix = pd.DataFrame(
        [terminal_forecast.values],
        index=[terminal_forecast.terminal_date],
    )

    checks = {
        "base_native_vs_legacy_baseline": (
            dataset.base_forecast(),
            base_forecast,
        ),
        "reference_native_vs_legacy_reference": (
            dataset.reference_forecast(),
            reference_forecast,
        ),
        "terminal_native_vs_legacy_terminal": (
            dataset.matrix("terminal_forecast", "intervention_adjusted"),
            terminal_matrix,
        ),
        "recovery_native_vs_legacy_final": (
            dataset.recovery_forecast(),
            recovery_forecast,
        ),
    }

    frames = []
    for check_name, (actual, forecast) in checks.items():
        evaluated = evaluate_forecast_matrix(
            actual=actual,
            forecast=forecast,
            train=observed_train,
            model_name="migrated",
            seasonality=seasonality,
            metrics=metrics,
        )
        frames.append(evaluated.assign(check=check_name))

    return pd.concat(frames, ignore_index=True).loc[
        :,
        ["check", "level", "unique_id", "metric", "value"],
    ]


def _evaluation_train_end(dataset: RecoveryDataset) -> pd.Timestamp:
    config = dataset.pipeline_config()
    if config.base is not None:
        return pd.Timestamp(config.base.train_end)
    if config.reference is not None and config.reference.train_end is not None:
        return pd.Timestamp(config.reference.train_end)
    dates = dataset.config.get("dates", {})
    if "observed_until" in dates:
        return pd.Timestamp(dates["observed_until"])
    return pd.Timestamp(dataset.observed_target().index.max())


def _require_forecast_frame(forecast, stage_name: str) -> pd.DataFrame:
    if forecast is None:
        raise ValueError(f"Pipeline did not produce a {stage_name} forecast.")
    return forecast.values


if __name__ == "__main__":
    main()
