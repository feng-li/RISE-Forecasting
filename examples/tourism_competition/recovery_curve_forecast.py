"""Compute recovery curve forecasts from the converted tourism dataset."""

from __future__ import annotations

import argparse
from pathlib import Path

from riseforecast import (
    InitialForecaster,
    RecoveryCurveForecaster,
    RecoveryDataset,
    ReferenceForecaster,
    extract_trend_component,
    intervention_terminal_forecast,
    reference_specs_from_config,
)
from riseforecast.preprocessing import stl_monthly_seasonal_multipliers


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--data-dir",
        type=Path,
        default=Path("examples/tourism_competition/data"),
    )
    parser.add_argument("--initial-date")
    parser.add_argument(
        "--initial-source",
        choices=("signal_reference", "base_models", "legacy_reference"),
        default="legacy_reference",
    )
    parser.add_argument(
        "--initial-models",
        default="seasonal_naive,random_walk_drift,arima,ets",
    )
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

    observed = dataset.observed_target()
    baseline = dataset.base_forecast()
    curve_config = dataset.config.get("curve", {})
    base_config = dataset.config.get("base", {})
    seasonality_train_end = base_config.get("train_end", dates.get("observed_until"))
    seasonal_multipliers = stl_monthly_seasonal_multipliers(
        observed.loc[:seasonality_train_end]
    )
    if args.initial_source == "legacy_reference":
        reference = dataset.reference_forecast()
    elif args.initial_source == "signal_reference":
        reference = ReferenceForecaster(
            start=dates.get("observed_until"),
            end=initial_date,
            train_end=dates.get("observed_until"),
            frequency=dataset.config.get("frequency", "MS"),
            specs=reference_specs_from_config(dataset.config),
        ).forecast(
            observed=observed,
            signals=dataset.exogenous_variables(),
        ).values
    else:
        models = tuple(
            model.strip() for model in args.initial_models.split(",") if model.strip()
        )
        reference = InitialForecaster(
            initial_date=initial_date,
            train_end=dates.get("observed_until"),
            models=models,
            frequency=dataset.config.get("frequency", "MS"),
        ).forecast(observed).values
    coefficients = dataset.coefficients()
    terminal = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date=terminal_date,
    )
    trend_history = None
    if curve_config.get("trend_history_start") is not None:
        history_dates = observed.reindex(index=reference.index.union(observed.index))
        history_dates = history_dates.loc[
            curve_config["trend_history_start"] : curve_config.get(
                "trend_history_end",
                initial_date,
            )
        ]
        history_values = history_dates.combine_first(
            reference.reindex(history_dates.index)
        )
        trend_history = extract_trend_component(history_values, seasonal_multipliers)

    recovery_curve = RecoveryCurveForecaster(
        initial_date=initial_date,
        forecast_start=forecast_start,
        forecast_end=forecast_end,
        curve_names=tuple(
            curve_config.get("curves", ("linear", "quadratic", "logistic"))
        ),
        quadratic_terminal_weight=float(
            curve_config.get("quadratic_terminal_weight", 18.0)
        ),
        logistic_anchor_dates=tuple(curve_config.get("logistic_anchor_dates", ())),
    ).forecast(
        initial_forecast=reference,
        terminal_forecast=terminal,
        seasonal_multipliers=seasonal_multipliers,
        trend_history=trend_history,
        base_forecast=baseline,
    )

    if args.output is None:
        print(recovery_curve.values.to_string())
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        recovery_curve.values.to_excel(args.output)


if __name__ == "__main__":
    main()
