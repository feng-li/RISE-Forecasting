import pandas as pd

from riseforecast.recovery import (
    apply_recovery_coefficients,
    average_recovery_score,
    calibrate_linear_coefficients,
)


def test_average_recovery_score() -> None:
    scores = pd.DataFrame(
        {
            "policy": [3, 5],
            "distance": [1, 1],
            "recovery": [2, 5],
        },
        index=["Canada", "Mexico"],
    )
    averaged = average_recovery_score(scores)
    assert averaged.loc["Canada"] == 2.0
    assert averaged.loc["Mexico"] == 11 / 3


def test_linear_recovery_calibration_and_application() -> None:
    average_scores = pd.Series({"Canada": 2.0, "Mexico": 3.7, "Hong Kong": 4.3})
    anchors = pd.Series({"Canada": 0.65, "Mexico": 1.0, "Hong Kong": 0.85})
    calibration = calibrate_linear_coefficients(average_scores, anchors)
    coefficients = pd.Series(
        calibration.predict(average_scores),
        index=average_scores.index,
    )
    base = pd.DataFrame({"Canada": [100.0], "Mexico": [100.0], "Hong Kong": [100.0]})
    adjusted = apply_recovery_coefficients(base, coefficients)
    assert adjusted.shape == base.shape
    assert adjusted.loc[0, "Canada"] < adjusted.loc[0, "Mexico"]
