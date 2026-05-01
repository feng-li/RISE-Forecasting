"""Hierarchy helpers for grouped and reconciled forecasts."""

from __future__ import annotations

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
) -> ReconciliationResult:
    """Reconcile forecast columns according to an explicit hierarchy."""

    if method == "bottom_up":
        return bottom_up_reconcile(forecasts, hierarchy)
    raise ValueError(f"Unsupported hierarchy reconciliation method: {method}")


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
