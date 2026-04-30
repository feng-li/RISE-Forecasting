from riseforecast.metrics import mae, mase, winkler_score


def test_mae() -> None:
    assert mae([1, 2, 3], [1, 4, 2]) == 1.0


def test_mase() -> None:
    assert mase([3, 5], [2, 7], insample=[1, 2, 3], seasonal_period=1) == 1.5


def test_winkler_score_inside_interval() -> None:
    assert winkler_score([10], [8], [12], alpha=0.2) == 4.0
