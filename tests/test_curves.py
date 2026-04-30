import numpy as np

from riseforecast.curves import CurveAnchors, build_recovery_curve, linear_curve


def test_linear_curve_hits_anchors() -> None:
    curve = linear_curve(CurveAnchors(initial=10.0, terminal=20.0, periods=4))
    assert np.allclose(curve, [10.0, 13.3333333333, 16.6666666667, 20.0])


def test_build_recovery_curve_returns_expected_length() -> None:
    curve = build_recovery_curve(10.0, 20.0, periods=12)
    assert len(curve) == 12
    assert curve[0] == 10.0
    assert curve[-1] == 20.0
