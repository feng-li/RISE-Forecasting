"""Hierarchy helpers for grouped and reconciled forecasts."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd


@dataclass(frozen=True)
class HierarchySpec:
    """Mapping from bottom-level entities to aggregate groups."""

    entity_to_group: dict[str, str]

    @property
    def entities(self) -> tuple[str, ...]:
        return tuple(self.entity_to_group)

    @property
    def groups(self) -> tuple[str, ...]:
        return tuple(sorted(set(self.entity_to_group.values())))


def aggregate_bottom_level(
    bottom_level: pd.DataFrame,
    hierarchy: HierarchySpec,
) -> pd.DataFrame:
    """Aggregate entity forecasts by hierarchy group."""

    missing = [
        entity for entity in hierarchy.entities if entity not in bottom_level.columns
    ]
    if missing:
        raise ValueError(f"Missing bottom-level entities: {', '.join(missing)}")
    grouped = {}
    for group in hierarchy.groups:
        members = [
            entity
            for entity, entity_group in hierarchy.entity_to_group.items()
            if entity_group == group
        ]
        grouped[group] = bottom_level.loc[:, members].sum(axis=1)
    return pd.DataFrame(grouped, index=bottom_level.index)


def build_summing_matrix(hierarchy: HierarchySpec) -> pd.DataFrame:
    """Build a simple group-plus-bottom summing matrix."""

    rows = list(hierarchy.groups) + list(hierarchy.entities)
    matrix = pd.DataFrame(
        np.zeros((len(rows), len(hierarchy.entities))),
        index=rows,
        columns=hierarchy.entities,
    )
    for entity, group in hierarchy.entity_to_group.items():
        matrix.loc[group, entity] = 1.0
        matrix.loc[entity, entity] = 1.0
    return matrix
