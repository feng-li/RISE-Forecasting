import numpy as np
import pandas as pd
import pytest

from riseforecast.hierarchy import (
    HierarchySpec,
    build_summing_matrix,
    hierarchy_from_series,
    reconcile_forecasts,
)


def hierarchy_series() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "series_id": ["total", "region", "series_a", "series_b"],
            "series_name": ["Total", "Region", "Series A", "Series B"],
            "target_name": ["target", "target", "target", "target"],
            "unit": ["count", "count", "count", "count"],
            "parent_id": [None, "total", "region", "region"],
        }
    )


def test_hierarchy_from_series_infers_bottom_nodes_without_is_bottom() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())

    assert hierarchy.root_ids == ("total",)
    assert hierarchy.aggregate_ids == ("total", "region")
    assert hierarchy.bottom_ids == ("series_a", "series_b")


def test_build_summing_matrix_from_parent_ids() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())

    matrix = build_summing_matrix(hierarchy)

    assert matrix.index.tolist() == ["total", "region", "series_a", "series_b"]
    assert matrix.columns.tolist() == ["series_a", "series_b"]
    assert np.allclose(matrix.loc["total"], [1.0, 1.0])
    assert np.allclose(matrix.loc["region"], [1.0, 1.0])
    assert np.allclose(matrix.loc["series_a"], [1.0, 0.0])


def test_bottom_up_reconciliation_sums_to_aggregate_nodes() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())
    bottom = pd.DataFrame(
        {"series_a": [10.0, 12.0], "series_b": [20.0, 22.0]},
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )

    result = reconcile_forecasts(bottom, hierarchy, method="bottom_up")

    assert result.method == "bottom_up"
    assert result.values.columns.tolist() == [
        "total",
        "region",
        "series_a",
        "series_b",
    ]
    assert np.allclose(result.values["total"], [30.0, 34.0])
    assert np.allclose(result.values["region"], [30.0, 34.0])


def test_wls_struct_reconciliation_uses_hierarchicalforecast_mintrace() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())
    forecasts = all_node_forecasts()

    result = reconcile_forecasts(forecasts, hierarchy, method="wls_struct")

    assert result.method == "wls_struct"
    assert np.allclose(
        result.values["total"],
        result.values["series_a"] + result.values["series_b"],
    )
    assert np.allclose(result.values["total"], result.values["region"])


def test_top_down_reconciliation_preserves_top_forecast() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())
    forecasts = all_node_forecasts()

    result = reconcile_forecasts(
        forecasts,
        hierarchy,
        method="top_down_forecast_proportions",
    )

    assert np.allclose(result.values["total"], forecasts["total"])
    assert np.allclose(
        result.values["total"],
        result.values["series_a"] + result.values["series_b"],
    )


def test_mint_shrink_requires_and_uses_insample_residuals() -> None:
    hierarchy = HierarchySpec.from_series(hierarchy_series())
    forecasts = all_node_forecasts()
    insample = pd.DataFrame(
        {
            "total": [30.0, 36.0, 42.0, 48.0],
            "region": [30.0, 36.0, 42.0, 48.0],
            "series_a": [10.0, 12.0, 14.0, 16.0],
            "series_b": [20.0, 24.0, 28.0, 32.0],
        },
        index=pd.date_range("2023-09-01", periods=4, freq="MS"),
    )
    fitted = insample * 0.95

    with pytest.raises(ValueError, match="requires insample and fitted"):
        reconcile_forecasts(forecasts, hierarchy, method="mint_shrink")

    result = reconcile_forecasts(
        forecasts,
        hierarchy,
        method="mint_shrink",
        insample=insample,
        fitted=fitted,
    )

    assert np.allclose(
        result.values["total"],
        result.values["series_a"] + result.values["series_b"],
    )


def test_hierarchy_from_series_requires_parent_rows() -> None:
    series = pd.DataFrame(
        {
            "series_id": ["series_a"],
            "parent_id": ["missing_parent"],
        }
    )

    with pytest.raises(ValueError, match="parent_id values"):
        HierarchySpec.from_series(series)


def test_hierarchy_from_series_returns_none_without_parent_column() -> None:
    series = pd.DataFrame({"series_id": ["series_a"]})

    assert hierarchy_from_series(series) is None


def all_node_forecasts() -> pd.DataFrame:
    return pd.DataFrame(
        {
            "total": [35.0, 45.0],
            "region": [34.0, 44.0],
            "series_a": [10.0, 12.0],
            "series_b": [20.0, 25.0],
        },
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )
