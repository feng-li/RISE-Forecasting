import numpy as np
import pandas as pd

from riseforecast.curves import (
    CurveAnchors,
    RecoveryCurveForecaster,
    build_recovery_curve,
    extract_trend_component,
    linear_curve,
    recover_full_forecast,
    recovery_curve_forecast,
)
from riseforecast.intervention import intervention_terminal_forecast


def test_linear_curve_hits_anchors() -> None:
    curve = linear_curve(CurveAnchors(initial=10.0, terminal=20.0, periods=4))
    assert np.allclose(curve, [10.0, 13.3333333333, 16.6666666667, 20.0])


def test_build_recovery_curve_returns_expected_length() -> None:
    curve = build_recovery_curve(10.0, 20.0, periods=12)
    assert len(curve) == 12
    assert curve[0] == 10.0
    assert curve[-1] == 20.0


def test_recovery_curve_forecaster_uses_intervention_terminal_forecast() -> None:
    baseline = pd.DataFrame(
        {"Canada": [100.0]},
        index=pd.to_datetime(["2024-10-01"]),
    )
    terminal = intervention_terminal_forecast(
        baseline,
        pd.Series({"Canada": 0.5}),
        terminal_date="2024-10",
    )
    initial = pd.Series({"Canada": 10.0})

    result = recovery_curve_forecast(
        initial_forecast=initial,
        terminal_forecast=terminal,
        initial_date="2024-06",
        forecast_start="2024-08",
        curve_names=("linear",),
    )

    assert result.values.index.tolist() == list(
        pd.to_datetime(["2024-08-01", "2024-09-01", "2024-10-01"])
    )
    assert np.allclose(result.values["Canada"], [30.0, 40.0, 50.0])


def test_recovery_curve_forecaster_accepts_initial_forecast_matrix() -> None:
    initial = pd.DataFrame(
        {"Canada": [8.0, 10.0], "Mexico": [18.0, 20.0]},
        index=pd.to_datetime(["2024-05-01", "2024-06-01"]),
    )
    terminal = pd.Series({"Canada": 50.0, "Mexico": 60.0})

    result = RecoveryCurveForecaster(
        initial_date="2024-06",
        forecast_start="2024-08",
        forecast_end="2024-10",
        curve_names=("linear",),
    ).forecast(initial, terminal)

    assert list(result.values.columns) == ["Canada", "Mexico"]
    assert np.allclose(result.values["Canada"], [30.0, 40.0, 50.0])
    assert np.allclose(result.values["Mexico"], [40.0, 50.0, 60.0])


def test_recovery_curve_forecaster_applies_seasonal_multipliers() -> None:
    initial = pd.Series({"Canada": 10.0})
    terminal = pd.Series({"Canada": 80.0})
    seasonal = pd.Series({1: 1.0, 2: 2.0, 3: 2.0})

    result = recovery_curve_forecast(
        initial_forecast=initial,
        terminal_forecast=terminal,
        initial_date="2024-01",
        forecast_start="2024-02",
        forecast_end="2024-03",
        curve_names=("linear",),
        seasonal_multipliers=seasonal,
    )

    assert np.allclose(result.values["Canada"], [50.0, 80.0])
    assert result.trend_values is not None
    assert np.allclose(result.trend_values["Canada"], [25.0, 40.0])
    assert result.recovery_curve is not None
    assert np.allclose(result.recovery_curve["Canada"], [25.0, 40.0])
    assert result.seasonal_components is not None
    assert np.allclose(result.seasonal_components["Canada"], [2.0, 2.0])
    assert np.allclose(
        result.values,
        recover_full_forecast(result.recovery_curve, result.seasonal_components),
    )
    assert np.allclose(result.trend_components["linear"]["Canada"], [25.0, 40.0])
    assert np.allclose(result.components["linear"]["Canada"], [50.0, 80.0])
    assert result.initial_trend is not None
    assert result.terminal_trend is not None
    assert result.initial_trend.loc["Canada"] == 10.0
    assert result.terminal_trend.loc["Canada"] == 40.0


def test_recover_full_forecast_accepts_monthly_seasonal_components() -> None:
    recovery_curve = pd.DataFrame(
        {"Canada": [25.0, 40.0]},
        index=pd.to_datetime(["2024-02-01", "2024-03-01"]),
    )
    seasonal = pd.Series({2: 2.0, 3: 1.5})

    forecast = recover_full_forecast(recovery_curve, seasonal)

    assert np.allclose(forecast["Canada"], [50.0, 60.0])


def test_extract_trend_component_removes_seasonal_components() -> None:
    full = pd.DataFrame(
        {"Canada": [50.0, 60.0]},
        index=pd.to_datetime(["2024-02-01", "2024-03-01"]),
    )
    seasonal = pd.Series({2: 2.0, 3: 1.5})

    trend = extract_trend_component(full, seasonal)

    assert np.allclose(trend["Canada"], [25.0, 40.0])


def test_quadratic_curve_uses_trend_history_and_weighted_terminal() -> None:
    trend_history = pd.DataFrame(
        {"Canada": [10.0, 14.0, 20.0]},
        index=pd.date_range("2024-01-01", periods=3, freq="MS"),
    )
    initial = pd.Series({"Canada": 30.0})
    terminal = pd.Series({"Canada": 80.0})

    result = RecoveryCurveForecaster(
        initial_date="2024-04",
        forecast_start="2024-05",
        forecast_end="2024-06",
        curve_names=("quadratic",),
        quadratic_terminal_weight=18.0,
    ).forecast(
        initial_forecast=initial,
        terminal_forecast=terminal,
        trend_history=trend_history,
    )

    assert result.trend_history is not None
    assert np.isclose(
        result.trend_components["quadratic"].loc["2024-05-01", "Canada"],
        54.45176180950921,
    )


def test_logistic_curve_uses_base_forecast_anchor_dates() -> None:
    trend_history = pd.DataFrame(
        {"Canada": [10.0]},
        index=pd.to_datetime(["2024-01-01"]),
    )
    base_forecast = pd.DataFrame(
        {"Canada": [40.0, 120.0]},
        index=pd.to_datetime(["2024-04-01", "2024-06-01"]),
    )
    initial = pd.Series({"Canada": 20.0})
    terminal = pd.Series({"Canada": 60.0})

    result = RecoveryCurveForecaster(
        initial_date="2024-03",
        forecast_start="2024-04",
        forecast_end="2024-05",
        curve_names=("logistic",),
        logistic_anchor_dates=("2024-04", "2024-06"),
    ).forecast(
        initial_forecast=initial,
        terminal_forecast=terminal,
        trend_history=trend_history,
        base_forecast=base_forecast,
    )

    endpoint_only = RecoveryCurveForecaster(
        initial_date="2024-03",
        forecast_start="2024-04",
        forecast_end="2024-05",
        curve_names=("logistic",),
    ).forecast(initial_forecast=initial, terminal_forecast=terminal)

    assert not np.allclose(
        result.trend_components["logistic"],
        endpoint_only.trend_components["logistic"],
    )
