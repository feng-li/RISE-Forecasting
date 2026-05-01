"""Hierarchy helpers for grouped and reconciled forecasts."""

from __future__ import annotations

import os
from dataclasses import dataclass

import numpy as np
import pandas as pd

HierarchyMethod = str


@dataclass(frozen=True)
class HierarchySpec:
    """Tree hierarchy inferred from `series_id` and `parent_id` metadata."""

    nodes: pd.DataFrame
    series_column: str = "series_id"
    parent_column: str = "parent_id"

    @classmethod
    def from_series(
        cls,
        series: pd.DataFrame,
        series_column: str = "series_id",
        parent_column: str = "parent_id",
    ) -> HierarchySpec:
        """Build a hierarchy from series metadata.

        Bottom nodes are inferred as nodes that never appear as a parent.
        """

        if parent_column not in series.columns:
            raise ValueError(f"Missing hierarchy parent column: {parent_column}")
        if series_column not in series.columns:
            raise ValueError(f"Missing hierarchy series column: {series_column}")

        nodes = series.loc[:, [series_column, parent_column]].copy()
        nodes[series_column] = nodes[series_column].astype(str)
        nodes[parent_column] = nodes[parent_column].map(_normalize_parent_id)
        if nodes[series_column].duplicated().any():
            duplicates = nodes.loc[nodes[series_column].duplicated(), series_column]
            joined = ", ".join(duplicates.astype(str))
            raise ValueError(f"Duplicate hierarchy series_id values: {joined}")
        nodes = nodes.set_index(series_column, drop=False)
        _validate_parent_ids(nodes, parent_column=parent_column)
        spec = cls(
            nodes=nodes,
            series_column=series_column,
            parent_column=parent_column,
        )
        _validate_acyclic(spec)
        return spec

    @property
    def node_ids(self) -> tuple[str, ...]:
        """All hierarchy nodes in metadata order."""

        return tuple(self.nodes.index.astype(str))

    @property
    def root_ids(self) -> tuple[str, ...]:
        """Nodes without a parent."""

        parents = self.nodes[self.parent_column]
        return tuple(self.nodes.index[parents.isna()].astype(str))

    @property
    def bottom_ids(self) -> tuple[str, ...]:
        """Leaf nodes inferred from `parent_id`."""

        parent_ids = set(self.nodes[self.parent_column].dropna().astype(str))
        return tuple(node for node in self.node_ids if node not in parent_ids)

    @property
    def aggregate_ids(self) -> tuple[str, ...]:
        """Non-leaf hierarchy nodes."""

        bottom = set(self.bottom_ids)
        return tuple(node for node in self.node_ids if node not in bottom)

    @property
    def entities(self) -> tuple[str, ...]:
        """Backward-compatible alias for bottom-level entities."""

        return self.bottom_ids

    @property
    def groups(self) -> tuple[str, ...]:
        """Backward-compatible alias for aggregate groups."""

        return self.aggregate_ids


@dataclass(frozen=True)
class ReconciliationResult:
    """Forecast matrix after applying a hierarchy reconciliation method."""

    values: pd.DataFrame
    method: str
    summing_matrix: pd.DataFrame


def hierarchy_from_series(
    series: pd.DataFrame,
    parent_column: str = "parent_id",
    series_column: str = "series_id",
) -> HierarchySpec | None:
    """Return a hierarchy spec when a non-empty parent column is present."""

    if parent_column not in series.columns:
        return None
    parents = series[parent_column].map(_normalize_parent_id)
    if parents.dropna().empty:
        return None
    return HierarchySpec.from_series(
        series,
        series_column=series_column,
        parent_column=parent_column,
    )


def aggregate_bottom_level(
    bottom_level: pd.DataFrame,
    hierarchy: HierarchySpec,
) -> pd.DataFrame:
    """Aggregate bottom-level forecasts to all hierarchy nodes."""

    return bottom_up_reconcile(bottom_level, hierarchy).values


def build_summing_matrix(hierarchy: HierarchySpec) -> pd.DataFrame:
    """Build an all-nodes by bottom-nodes summing matrix."""

    matrix = pd.DataFrame(
        np.zeros((len(hierarchy.node_ids), len(hierarchy.bottom_ids))),
        index=hierarchy.node_ids,
        columns=hierarchy.bottom_ids,
        dtype=float,
    )
    descendants = _bottom_descendants_by_node(hierarchy)
    for node, bottom_ids in descendants.items():
        matrix.loc[node, list(bottom_ids)] = 1.0
    return matrix


def reconcile_forecasts(
    forecasts: pd.DataFrame,
    hierarchy: HierarchySpec,
    method: HierarchyMethod = "bottom_up",
    insample: pd.DataFrame | None = None,
    fitted: pd.DataFrame | None = None,
) -> ReconciliationResult:
    """Reconcile forecast columns according to an explicit hierarchy."""

    if method == "bottom_up":
        return bottom_up_reconcile(forecasts, hierarchy)

    return hierarchicalforecast_reconcile(
        forecasts=forecasts,
        hierarchy=hierarchy,
        method=method,
        insample=insample,
        fitted=fitted,
    )


def reconciliation_requires_insample(method: HierarchyMethod) -> bool:
    """Return whether a reconciliation method needs insample fitted values."""

    return _requires_insample(method)


def bottom_up_reconcile(
    bottom_level: pd.DataFrame,
    hierarchy: HierarchySpec,
) -> ReconciliationResult:
    """Produce coherent all-node forecasts by summing bottom-level forecasts."""

    forecast = _prepare_matrix(bottom_level)
    missing = [node for node in hierarchy.bottom_ids if node not in forecast.columns]
    if missing:
        raise ValueError(
            f"Missing bottom-level forecasts for hierarchy nodes: {', '.join(missing)}"
        )
    bottom = forecast.loc[:, list(hierarchy.bottom_ids)]
    summing_matrix = build_summing_matrix(hierarchy)
    reconciled = bottom.to_numpy(dtype=float) @ summing_matrix.T.to_numpy(dtype=float)
    values = pd.DataFrame(
        reconciled,
        index=bottom.index,
        columns=summing_matrix.index,
    )
    return ReconciliationResult(
        values=values,
        method="bottom_up",
        summing_matrix=summing_matrix,
    )


def hierarchicalforecast_reconcile(
    forecasts: pd.DataFrame,
    hierarchy: HierarchySpec,
    method: HierarchyMethod,
    insample: pd.DataFrame | None = None,
    fitted: pd.DataFrame | None = None,
) -> ReconciliationResult:
    """Reconcile forecasts with Nixtla's `hierarchicalforecast` methods.

    If `forecasts` contains only bottom-level columns, aggregate-node forecasts
    are initialized with bottom-up sums before calling `hierarchicalforecast`.
    Methods such as `wls_var` and `mint_shrink` require matching `insample` and
    `fitted` matrices.
    """

    reconciler = _hierarchicalforecast_reconciler(method)
    all_forecasts = _coerce_all_node_matrix(forecasts, hierarchy)
    y_hat = all_forecasts.loc[:, list(hierarchy.node_ids)].T.to_numpy(dtype=float)
    summing_matrix = build_summing_matrix(hierarchy)
    kwargs = {
        "S": summing_matrix.to_numpy(dtype=float),
        "y_hat": y_hat,
        "tags": _hierarchicalforecast_tags(hierarchy),
    }

    if _requires_insample(method):
        if insample is None or fitted is None:
            raise ValueError(
                f"{method} reconciliation requires insample and fitted matrices."
            )
    if insample is not None:
        y_insample = _coerce_all_node_matrix(insample, hierarchy)
        kwargs["y_insample"] = y_insample.loc[
            :,
            list(hierarchy.node_ids),
        ].T.to_numpy(dtype=float)
    if fitted is not None:
        y_hat_insample = _coerce_all_node_matrix(fitted, hierarchy)
        kwargs["y_hat_insample"] = y_hat_insample.loc[
            :,
            list(hierarchy.node_ids),
        ].T.to_numpy(dtype=float)

    output = reconciler.fit_predict(**kwargs)
    values = pd.DataFrame(
        output["mean"].T,
        index=all_forecasts.index,
        columns=hierarchy.node_ids,
    )
    return ReconciliationResult(
        values=values,
        method=method,
        summing_matrix=summing_matrix,
    )


def _bottom_descendants_by_node(
    hierarchy: HierarchySpec,
) -> dict[str, tuple[str, ...]]:
    descendants = {}
    for node in hierarchy.node_ids:
        descendants[node] = tuple(
            bottom
            for bottom in hierarchy.bottom_ids
            if node == bottom or node in _ancestors(bottom, hierarchy)
        )
    return descendants


def _coerce_all_node_matrix(
    matrix: pd.DataFrame,
    hierarchy: HierarchySpec,
) -> pd.DataFrame:
    frame = _prepare_matrix(matrix)
    if set(hierarchy.node_ids).issubset(frame.columns):
        return frame.loc[:, list(hierarchy.node_ids)]
    if set(hierarchy.bottom_ids).issubset(frame.columns):
        return bottom_up_reconcile(
            frame.loc[:, list(hierarchy.bottom_ids)],
            hierarchy,
        ).values
    missing = sorted(set(hierarchy.bottom_ids) - set(frame.columns))
    raise ValueError(
        "Forecast matrix must contain either all hierarchy nodes or all bottom "
        f"nodes. Missing bottom nodes: {', '.join(missing)}"
    )


def _hierarchicalforecast_reconciler(method: HierarchyMethod):
    os.environ.setdefault("MPLCONFIGDIR", "/tmp")
    from hierarchicalforecast.methods import MinTrace, TopDown

    if method in {"top_down", "top_down_forecast_proportions"}:
        return TopDown("forecast_proportions")
    if method == "top_down_average_proportions":
        return TopDown("average_proportions")
    if method == "top_down_proportion_averages":
        return TopDown("proportion_averages")
    if method == "mint":
        return MinTrace("mint_shrink")
    if method in {"ols", "wls_struct", "wls_var", "mint_shrink", "mint_cov"}:
        return MinTrace(method)
    raise ValueError(f"Unsupported hierarchy reconciliation method: {method}")


def _requires_insample(method: HierarchyMethod) -> bool:
    return method in {
        "top_down_average_proportions",
        "top_down_proportion_averages",
        "wls_var",
        "mint",
        "mint_shrink",
        "mint_cov",
    }


def _hierarchicalforecast_tags(hierarchy: HierarchySpec) -> dict[str, np.ndarray]:
    positions = {node: index for index, node in enumerate(hierarchy.node_ids)}
    depths = _node_depths(hierarchy)
    tags = {}
    for depth in sorted(set(depths.values())):
        tags[f"level_{depth}"] = np.array(
            [
                positions[node]
                for node, node_depth in depths.items()
                if node_depth == depth
            ],
            dtype=int,
        )
    return tags


def _node_depths(hierarchy: HierarchySpec) -> dict[str, int]:
    return {node: len(_ancestors(node, hierarchy)) for node in hierarchy.node_ids}


def _ancestors(node: str, hierarchy: HierarchySpec) -> tuple[str, ...]:
    ancestors = []
    current = node
    while True:
        parent = hierarchy.nodes.loc[current, hierarchy.parent_column]
        if pd.isna(parent):
            return tuple(ancestors)
        current = str(parent)
        ancestors.append(current)


def _validate_parent_ids(nodes: pd.DataFrame, parent_column: str) -> None:
    parent_ids = set(nodes[parent_column].dropna().astype(str))
    unknown = sorted(parent_ids - set(nodes.index.astype(str)))
    if unknown:
        raise ValueError(
            "Hierarchy parent_id values are not series_id rows: "
            f"{', '.join(unknown)}"
        )
    if not nodes[parent_column].isna().any():
        raise ValueError("Hierarchy must contain at least one root node.")


def _validate_acyclic(hierarchy: HierarchySpec) -> None:
    for node in hierarchy.node_ids:
        seen = {node}
        current = node
        while True:
            parent = hierarchy.nodes.loc[current, hierarchy.parent_column]
            if pd.isna(parent):
                break
            current = str(parent)
            if current in seen:
                raise ValueError(f"Hierarchy contains a cycle at node: {current}")
            seen.add(current)


def _prepare_matrix(frame: pd.DataFrame) -> pd.DataFrame:
    matrix = frame.copy()
    matrix.index = pd.to_datetime(matrix.index)
    return matrix.sort_index().astype(float)


def _normalize_parent_id(value: object) -> str | None:
    if value is None or pd.isna(value):
        return None
    text = str(value).strip()
    return None if text == "" else text
