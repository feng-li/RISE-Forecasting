import pandas as pd
import pytest

from riseforecast.recovery import (
    RecoveryCoefficientEstimator,
    apply_recovery_coefficients,
    average_recovery_score,
    calibrate_linear_coefficients,
    direct_recovery_coefficients,
    regress_recovery_coefficients,
    weighted_recovery_score,
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


def test_weighted_recovery_score_uses_factor_weights() -> None:
    scores = pd.DataFrame(
        {
            "policy": [5, 1],
            "distance": [1, 5],
            "recovery": [1, 1],
        },
        index=["policy_led", "distance_led"],
    )

    weighted = weighted_recovery_score(
        scores,
        weights={"policy": 3.0, "distance": 1.0, "recovery": 0.0},
    )

    assert weighted.loc["policy_led"] == 0.75
    assert weighted.loc["distance_led"] == 0.25


def test_direct_recovery_coefficients_clip_to_bounds() -> None:
    metadata = pd.DataFrame(
        {
            "series_id": ["series_a", "series_b"],
            "coefficient": [0.5, 1.2],
        }
    )

    coefficients = direct_recovery_coefficients(metadata)

    assert coefficients.loc["series_a"] == 0.5
    assert coefficients.loc["series_b"] == 1.0


def test_regress_recovery_coefficients_from_anchor_scores() -> None:
    metadata = paper_style_metadata()

    calibration = regress_recovery_coefficients(
        metadata,
        anchors={"low": 0.5, "high": 1.0},
    )
    coefficients = calibration.predict(metadata)

    assert coefficients.loc["low"] == pytest.approx(0.5)
    assert coefficients.loc["mid"] == pytest.approx(0.75)
    assert coefficients.loc["high"] == pytest.approx(1.0)


def test_recovery_coefficient_estimator_preserves_regression_anchors() -> None:
    metadata = paper_style_metadata()

    coefficients = RecoveryCoefficientEstimator(
        method="regression",
        anchors={"low": 0.5, "high": 1.0},
    ).estimate(metadata)

    assert coefficients.loc["low"] == pytest.approx(0.5)
    assert coefficients.loc["mid"] == pytest.approx(0.75)
    assert coefficients.loc["high"] == pytest.approx(1.0)


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


def paper_style_metadata() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "series_id": ["low", "mid", "high"],
            "policy": [1, 3, 5],
            "distance": [1, 3, 5],
            "recovery": [1, 3, 5],
        }
    )
