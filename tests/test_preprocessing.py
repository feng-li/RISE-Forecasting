import numpy as np
import pandas as pd
import pytest

from riseforecast.preprocessing import stl_monthly_seasonal_multipliers


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
