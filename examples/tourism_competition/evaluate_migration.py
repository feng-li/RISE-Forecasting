"""Evaluate migrated tourism forecasts against converted legacy artifacts."""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from riseforecast import (
    InitialForecaster,
    RecoveryCurveForecaster,
    RecoveryDataset,
    ReferenceForecaster,
    evaluate_forecast_matrix,
    intervention_terminal_forecast,
    reference_specs_from_config,
)

DEFAULT_INITIAL_MODELS = "seasonal_naive,random_walk_drift,arima,ets"
DEFAULT_METRICS = "mae,rmse,mape,smape,bias,mase,rmsse"


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--initial-models", default=DEFAULT_INITIAL_MODELS)
    parser.add_argument("--metrics", default=DEFAULT_METRICS)
    parser.add_argument("--seasonality", type=int, default=12)
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()

    dataset = RecoveryDataset.from_directory(args.data_dir)
    report = evaluate_migration(
        dataset=dataset,
        initial_models=tuple(
            model.strip() for model in args.initial_models.split(",") if model.strip()
        ),
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
    initial_models: tuple[str, ...] = (
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
    ),
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
    """Build migrated forecasts and compare them with legacy outputs."""

    dates = dataset.config["dates"]
    frequency = dataset.config.get("frequency", "MS")
    train_end = dates.get("observed_until")
    observed = dataset.observed_target()
    observed_train = (
        observed.loc[: pd.Timestamp(train_end)] if train_end is not None else observed
    )

    terminal = intervention_terminal_forecast(
        dataset.base_forecast(),
        dataset.coefficients(),
        terminal_date=dates["terminal_date"],
    )
    terminal_matrix = pd.DataFrame([terminal.values], index=[terminal.terminal_date])

    initial = InitialForecaster(
        initial_date=dates["initial_date"],
        train_end=train_end,
        models=initial_models,
        frequency=frequency,
    ).forecast(observed)
    reference = ReferenceForecaster(
        start=train_end,
        end=dates["initial_date"],
        train_end=train_end,
        specs=reference_specs_from_config(dataset.config),
        frequency=frequency,
    ).forecast(observed, dataset.exogenous_variables())

    recovery_from_legacy_reference = RecoveryCurveForecaster(
        initial_date=dates["initial_date"],
        forecast_start=dates["forecast_start"],
        forecast_end=dates["forecast_end"],
        frequency=frequency,
    ).forecast(
        initial_forecast=dataset.reference_forecast(),
        terminal_forecast=terminal,
    )
    recovery_from_base_initial = RecoveryCurveForecaster(
        initial_date=dates["initial_date"],
        forecast_start=dates["forecast_start"],
        forecast_end=dates["forecast_end"],
        frequency=frequency,
    ).forecast(
        initial_forecast=initial.values,
        terminal_forecast=terminal,
    )
    recovery_from_signal_reference = RecoveryCurveForecaster(
        initial_date=dates["initial_date"],
        forecast_start=dates["forecast_start"],
        forecast_end=dates["forecast_end"],
        frequency=frequency,
    ).forecast(
        initial_forecast=reference.values,
        terminal_forecast=terminal,
    )

    checks = {
        "initial_base_models_vs_legacy_reference": (
            dataset.reference_forecast(),
            initial.values,
        ),
        "reference_signals_vs_legacy_reference": (
            dataset.reference_forecast(),
            reference.values,
        ),
        "terminal_intervention_vs_legacy_terminal": (
            dataset.matrix("terminal_forecast", "intervention_adjusted"),
            terminal_matrix,
        ),
        "recovery_curve_legacy_reference_vs_legacy_final": (
            dataset.recovery_forecast(),
            recovery_from_legacy_reference.values,
        ),
        "recovery_curve_base_initial_vs_legacy_final": (
            dataset.recovery_forecast(),
            recovery_from_base_initial.values,
        ),
        "recovery_curve_signal_reference_vs_legacy_final": (
            dataset.recovery_forecast(),
            recovery_from_signal_reference.values,
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


if __name__ == "__main__":
    main()
