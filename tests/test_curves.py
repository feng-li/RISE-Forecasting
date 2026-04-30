import numpy as np
import pandas as pd

from riseforecast.curves import (
    CurveAnchors,
    RecoveryCurveForecaster,
    build_recovery_curve,
    linear_curve,
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
