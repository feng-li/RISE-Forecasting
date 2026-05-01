import numpy as np
import pandas as pd
import pytest

from riseforecast.preprocessing import (
    impute_time_series,
    kalman_smooth_impute,
    stl_monthly_seasonal_multipliers,
)


def test_kalman_smooth_impute_fills_missing_and_preserves_observed() -> None:
    series = pd.Series(
        [10.0, 12.0, np.nan, 16.0, 18.0, np.nan, 22.0],
        index=pd.date_range("2024-01-01", periods=7, freq="MS"),
    )

    imputed = kalman_smooth_impute(series)

    assert not imputed.isna().any()
    assert np.allclose(imputed.loc[series.notna()], series.dropna())
    assert np.isfinite(imputed.loc[series.isna()]).all()


def test_impute_time_series_uses_kalman_alias() -> None:
    series = pd.Series(
        [1.0, 2.0, np.nan, 4.0],
        index=pd.date_range("2024-01-01", periods=4, freq="MS"),
    )

    imputed = impute_time_series(series, method="kalman_smoothing")

    assert not imputed.isna().any()
    assert imputed.loc["2024-02-01"] == 2.0


def test_kalman_smooth_impute_falls_back_for_short_series() -> None:
    series = pd.Series(
        [1.0, np.nan, 3.0],
        index=pd.date_range("2024-01-01", periods=3, freq="MS"),
    )

    imputed = kalman_smooth_impute(series, min_observations=4)

    assert imputed.iloc[1] == 2.0


def test_stl_monthly_seasonal_multipliers_returns_month_factors() -> None:
    index = pd.date_range("2021-01-01", periods=36, freq="MS")
    month_factors = np.array(
        [1.0, 1.2, 0.8, 1.1, 0.9, 1.3, 1.0, 0.95, 1.05, 0.85, 1.15, 1.0]
    )
    repeated = np.resize(month_factors, len(index))
    matrix = pd.DataFrame(
        {
            "series_a": 100.0 * repeated,
            "series_b": 200.0 * repeated,
        },
        index=index,
    )

    factors = stl_monthly_seasonal_multipliers(matrix)

    assert factors.index.tolist() == list(range(1, 13))
    assert list(factors.columns) == ["series_a", "series_b"]
    assert factors.loc[6, "series_a"] > factors.loc[3, "series_a"]
    assert factors.loc[2, "series_b"] > factors.loc[5, "series_b"]


def test_stl_monthly_seasonal_multipliers_requires_enough_data() -> None:
    matrix = pd.DataFrame(
        {"series_a": np.arange(12.0) + 1.0},
        index=pd.date_range("2024-01-01", periods=12, freq="MS"),
    )

    with pytest.raises(ValueError, match="two full seasonal periods"):
        stl_monthly_seasonal_multipliers(matrix)
