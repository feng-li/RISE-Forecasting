"""Inspect package-native tourism recovery forecast artifacts."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from riseforecast import (
    RecoveryDataset,
    RecoveryForecastingPipeline,
    evaluate_forecast_matrix,
    evaluate_interval_matrix,
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
    parser.add_argument("--entities", nargs="+")
    parser.add_argument(
        "--output-dir",
        type=Path,
        help="Optional directory for CSV diagnostic tables.",
    )
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    pipeline = RecoveryForecastingPipeline.from_dataset(dataset).fit_dataset(dataset)
    diagnostics = diagnose_package_forecast(
        dataset=dataset,
        pipeline=pipeline,
        metrics=tuple(
            metric.strip() for metric in args.metrics.split(",") if metric.strip()
        ),
        seasonality=args.seasonality,
        entities=None if args.entities is None else tuple(args.entities),
    )

    print_diagnostics(diagnostics)
    if args.output_dir is not None:
        args.output_dir.mkdir(parents=True, exist_ok=True)
        for name, frame in diagnostics.items():
            frame.to_csv(args.output_dir / f"{name}.csv", index=False)
        print(f"\nWrote diagnostics to {args.output_dir}")


def diagnose_package_forecast(
    dataset: RecoveryDataset,
    pipeline: RecoveryForecastingPipeline,
    metrics: tuple[str, ...] = tuple(DEFAULT_METRICS.split(",")),
    seasonality: int = 12,
    entities: tuple[str, ...] | None = None,
) -> dict[str, pd.DataFrame]:
    """Build package-native diagnostic tables from a fitted pipeline."""

    forecast = pipeline.predict()
    forecast_values = _filter_entities(forecast.values, entities)
    lower = (
        None
        if forecast.lower is None
        else _filter_entities(forecast.lower, entities)
    )
    upper = (
        None
        if forecast.upper is None
        else _filter_entities(forecast.upper, entities)
    )
    actual = _filter_entities(dataset.observed_target(), tuple(forecast_values.columns))

    diagnostics = {
        "selected_base_models": selected_base_models(pipeline),
        "base_validation_errors": base_validation_errors(pipeline),
        "recovery_coefficients": recovery_coefficients(dataset, pipeline, entities),
        "terminal_forecast": terminal_forecast_table(pipeline, entities),
        "final_forecast_summary": forecast_summary(
            forecast_values,
            lower=lower,
            upper=upper,
        ),
        "point_evaluation": point_evaluation(
            actual=actual,
            forecast=forecast_values,
            train=_training_sample(dataset, pipeline),
            metrics=metrics,
            seasonality=seasonality,
        ),
        "interval_evaluation": interval_evaluation(
            actual=actual,
            lower=lower,
            upper=upper,
            alpha=dataset.pipeline_config().interval.alpha,
        ),
    }
    return diagnostics


def selected_base_models(pipeline: RecoveryForecastingPipeline) -> pd.DataFrame:
    """Return selected package-native base models."""

    models = pipeline.state.base_selected_models
    if not models:
        return pd.DataFrame(
            [{"rank": pd.NA, "model": "legacy_or_config_fallback"}]
        )
    return pd.DataFrame(
        [{"rank": index + 1, "model": model} for index, model in enumerate(models)]
    )


def base_validation_errors(pipeline: RecoveryForecastingPipeline) -> pd.DataFrame:
    """Return validation errors used for base model selection."""

    errors = pipeline.state.base_validation_errors
    if errors is None or errors.empty:
        return pd.DataFrame(
            [
                {
                    "model": pd.NA,
                    "metric": pd.NA,
                    "value": pd.NA,
                    "note": "No base validation window was configured.",
                }
            ]
        )
    return (
        errors.rename("value")
        .rename_axis("model")
        .reset_index()
        .assign(metric=errors.name)
        .loc[:, ["model", "metric", "value"]]
    )


def recovery_coefficients(
    dataset: RecoveryDataset,
    pipeline: RecoveryForecastingPipeline,
    entities: tuple[str, ...] | None,
) -> pd.DataFrame:
    """Return fitted recovery coefficients with series metadata."""

    coefficients = pipeline.state.recovery_coefficients
    if coefficients is None:
        raise ValueError("Pipeline did not produce recovery coefficients.")
    metadata = dataset.metadata().set_index("series_id")
    if entities is not None:
        coefficients = coefficients.loc[list(entities)]
    frame = coefficients.rename("coefficient").rename_axis("series_id").reset_index()
    columns = [
        "series_id",
        "series_name",
        "group",
        "policy",
        "distance",
        "recovery",
    ]
    available = [column for column in columns if column in metadata.columns]
    return frame.merge(
        metadata.loc[:, available].reset_index(),
        on="series_id",
        how="left",
    ).loc[:, ["series_id", *available, "coefficient"]]


def terminal_forecast_table(
    pipeline: RecoveryForecastingPipeline,
    entities: tuple[str, ...] | None,
) -> pd.DataFrame:
    """Return terminal baseline, coefficient, and adjusted forecast."""

    terminal = pipeline.state.terminal_forecast
    if terminal is None:
        raise ValueError("Pipeline did not produce a terminal forecast.")
    frame = terminal.to_frame().rename(columns={"entity": "series_id"})
    if entities is None:
        return frame
    return frame.loc[frame["series_id"].isin(entities)].reset_index(drop=True)


def forecast_summary(
    values: pd.DataFrame,
    lower: pd.DataFrame | None,
    upper: pd.DataFrame | None,
) -> pd.DataFrame:
    """Summarize each final package-native forecast path."""

    rows = []
    for series_id in values.columns:
        series = values[series_id].dropna()
        row = {
            "series_id": series_id,
            "start": series.index.min().date().isoformat(),
            "end": series.index.max().date().isoformat(),
            "first": float(series.iloc[0]),
            "last": float(series.iloc[-1]),
            "mean": float(series.mean()),
            "min": float(series.min()),
            "max": float(series.max()),
            "change": float(series.iloc[-1] - series.iloc[0]),
            "pct_change": (
                float(series.iloc[-1] / series.iloc[0] - 1)
                if series.iloc[0] != 0
                else pd.NA
            ),
        }
        if lower is not None and upper is not None:
            row.update(
                {
                    "lower_first": float(lower.loc[series.index[0], series_id]),
                    "upper_first": float(upper.loc[series.index[0], series_id]),
                    "lower_last": float(lower.loc[series.index[-1], series_id]),
                    "upper_last": float(upper.loc[series.index[-1], series_id]),
                }
            )
        rows.append(row)
    return pd.DataFrame(rows)


def point_evaluation(
    actual: pd.DataFrame,
    forecast: pd.DataFrame,
    train: pd.DataFrame,
    metrics: tuple[str, ...],
    seasonality: int,
) -> pd.DataFrame:
    """Evaluate point forecasts when actual observations overlap."""

    if not _has_overlap(actual, forecast):
        return diagnostic_note(
            "No overlapping actual observations are available for the final forecast "
            "period."
        )
    return evaluate_forecast_matrix(
        actual=actual,
        forecast=forecast,
        train=train,
        model_name="package_native",
        seasonality=seasonality,
        metrics=metrics,
    )


def interval_evaluation(
    actual: pd.DataFrame,
    lower: pd.DataFrame | None,
    upper: pd.DataFrame | None,
    alpha: float,
) -> pd.DataFrame:
    """Evaluate interval forecasts when bounds and actuals overlap."""

    if lower is None or upper is None:
        return diagnostic_note("Final forecast has no lower/upper interval bounds.")
    if not _has_overlap(actual, lower) or not _has_overlap(actual, upper):
        return diagnostic_note(
            "No overlapping actual observations are available for interval "
            "evaluation."
        )
    return evaluate_interval_matrix(
        actual=actual,
        lower=lower,
        upper=upper,
        alpha=alpha,
    )


def diagnostic_note(message: str) -> pd.DataFrame:
    return pd.DataFrame([{"note": message}])


def print_diagnostics(diagnostics: dict[str, pd.DataFrame]) -> None:
    """Print compact diagnostic tables."""

    for name, frame in diagnostics.items():
        print(f"\n## {name.replace('_', ' ').title()}")
        print(frame.to_string(index=False))


def _filter_entities(
    matrix: pd.DataFrame,
    entities: tuple[str, ...] | None,
) -> pd.DataFrame:
    if entities is None:
        return matrix
    missing = [entity for entity in entities if entity not in matrix.columns]
    if missing:
        raise ValueError(f"Missing entities: {', '.join(missing)}")
    return matrix.loc[:, list(entities)]


def _training_sample(
    dataset: RecoveryDataset,
    pipeline: RecoveryForecastingPipeline,
) -> pd.DataFrame:
    config = pipeline.config
    observed = dataset.observed_target()
    if config.base is not None:
        return observed.loc[: pd.Timestamp(config.base.train_end)]
    return observed.loc[: pd.Timestamp(config.shock_start)]


def _has_overlap(actual: pd.DataFrame, forecast: pd.DataFrame) -> bool:
    actual_index = pd.DatetimeIndex(pd.to_datetime(actual.index))
    forecast_index = pd.DatetimeIndex(pd.to_datetime(forecast.index))
    common_dates = actual_index.intersection(forecast_index)
    common_columns = actual.columns.intersection(forecast.columns)
    if common_dates.empty or common_columns.empty:
        return False
    overlap = actual.loc[common_dates, common_columns].notna() & forecast.loc[
        common_dates,
        common_columns,
    ].notna()
    return bool(overlap.any().any())


if __name__ == "__main__":
    main()
