import numpy as np
import pandas as pd

from riseforecast.ensembles import (
    combine_panel_forecasts,
    error_weighted_average,
    forecast_errors,
    positive_stacking_panel,
    select_top_models,
)


def test_forecast_errors_and_top_model_selection() -> None:
    actual = pd.DataFrame(
        {"a": [100.0, 120.0]},
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )
    forecasts = {
        "good": pd.DataFrame({"a": [100.0, 120.0]}, index=actual.index),
        "mid": pd.DataFrame({"a": [90.0, 130.0]}, index=actual.index),
        "bad": pd.DataFrame({"a": [50.0, 60.0]}, index=actual.index),
    }

    errors = forecast_errors(forecasts, actual=actual, metric="mae")
    selected = select_top_models(errors, fraction=0.8)

    assert errors.loc["good"] == 0.0
    assert selected == ("good", "mid")


def test_error_weighted_average_handles_zero_error_model() -> None:
    index = pd.date_range("2024-01-01", periods=2, freq="MS")
    forecasts = {
        "perfect": pd.DataFrame({"a": [10.0, 20.0]}, index=index),
        "other": pd.DataFrame({"a": [100.0, 200.0]}, index=index),
    }
    errors = pd.Series({"perfect": 0.0, "other": 10.0})

    combined = error_weighted_average(forecasts, errors)

    assert np.allclose(combined["a"], [10.0, 20.0])


def test_positive_stacking_panel_returns_future_matrix() -> None:
    validation_index = pd.date_range("2024-01-01", periods=3, freq="MS")
    future_index = pd.date_range("2024-04-01", periods=2, freq="MS")
    validation_actual = pd.DataFrame({"a": [10.0, 20.0, 30.0]}, index=validation_index)
    validation_forecasts = {
        "low": pd.DataFrame({"a": [8.0, 16.0, 24.0]}, index=validation_index),
        "high": pd.DataFrame({"a": [12.0, 24.0, 36.0]}, index=validation_index),
    }
    future_forecasts = {
        "low": pd.DataFrame({"a": [32.0, 40.0]}, index=future_index),
        "high": pd.DataFrame({"a": [48.0, 60.0]}, index=future_index),
    }

    combined = positive_stacking_panel(
        validation_forecasts=validation_forecasts,
        validation_actual=validation_actual,
        future_forecasts=future_forecasts,
        method="ridge",
    )

    assert combined.index.tolist() == list(future_index)
    assert np.isfinite(combined.to_numpy()).all()


def test_combine_panel_forecasts_supports_all_base_ensembles() -> None:
    validation_index = pd.date_range("2024-01-01", periods=3, freq="MS")
    future_index = pd.date_range("2024-04-01", periods=2, freq="MS")
    validation_actual = pd.DataFrame({"a": [10.0, 20.0, 30.0]}, index=validation_index)
    validation_forecasts = {
        "model_a": pd.DataFrame({"a": [9.0, 19.0, 29.0]}, index=validation_index),
        "model_b": pd.DataFrame({"a": [11.0, 21.0, 31.0]}, index=validation_index),
    }
    future_forecasts = {
        "model_a": pd.DataFrame({"a": [39.0, 49.0]}, index=future_index),
        "model_b": pd.DataFrame({"a": [41.0, 51.0]}, index=future_index),
    }
    validation_errors = forecast_errors(
        validation_forecasts,
        actual=validation_actual,
        metric="mae",
    )

    for ensemble in ("mean", "error_weighted", "ridge", "lasso"):
        combined = combine_panel_forecasts(
            future_forecasts=future_forecasts,
            ensemble=ensemble,
            validation_errors=validation_errors,
            validation_forecasts=validation_forecasts,
            validation_actual=validation_actual,
        )
        assert combined.shape == (2, 1)
        assert np.isfinite(combined.to_numpy()).all()
