"""Canonical data structures and validation helpers."""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass

import pandas as pd


@dataclass(frozen=True)
class PanelSchema:
    """Column names for long-form panel time series data."""

    date: str = "date"
    entity: str = "entity"
    value: str = "value"


@dataclass(frozen=True)
class SignalSchema:
    """Column names for long-form external signal data."""

    date: str = "date"
    entity: str = "entity"
    signal_name: str = "signal_name"
    signal_value: str = "signal_value"


@dataclass(frozen=True)
class ForecastFrame:
    """A forecast matrix indexed by date with one column per entity."""

    values: pd.DataFrame
    lower: pd.DataFrame | None = None
    upper: pd.DataFrame | None = None


DEFAULT_PANEL_SCHEMA = PanelSchema()
DEFAULT_SIGNAL_SCHEMA = SignalSchema()


def require_columns(frame: pd.DataFrame, columns: Iterable[str]) -> None:
    """Raise a clear error if a data frame is missing required columns."""

    missing = [column for column in columns if column not in frame.columns]
    if missing:
        joined = ", ".join(missing)
        raise ValueError(f"Missing required columns: {joined}")


def validate_panel_frame(
    frame: pd.DataFrame,
    schema: PanelSchema = DEFAULT_PANEL_SCHEMA,
) -> pd.DataFrame:
    """Validate and normalize a long-form panel frame."""

    require_columns(frame, (schema.date, schema.entity, schema.value))
    result = frame.copy()
    result[schema.date] = pd.to_datetime(result[schema.date])
    result = result.sort_values([schema.entity, schema.date])
    return result


def validate_signal_frame(
    frame: pd.DataFrame,
    schema: SignalSchema = DEFAULT_SIGNAL_SCHEMA,
) -> pd.DataFrame:
    """Validate and normalize a long-form external-signal frame."""

    require_columns(frame, (schema.date, schema.entity, schema.signal_name))
    require_columns(frame, (schema.signal_value,))
    result = frame.copy()
    result[schema.date] = pd.to_datetime(result[schema.date])
    result = result.sort_values([schema.entity, schema.signal_name, schema.date])
    return result


def panel_to_matrix(
    frame: pd.DataFrame,
    schema: PanelSchema = DEFAULT_PANEL_SCHEMA,
) -> pd.DataFrame:
    """Convert long-form panel data into a date-by-entity matrix."""

    normalized = validate_panel_frame(frame, schema)
    matrix = normalized.pivot(
        index=schema.date,
        columns=schema.entity,
        values=schema.value,
    )
    matrix.index = pd.DatetimeIndex(matrix.index)
    return matrix.sort_index()


def matrix_to_panel(
    matrix: pd.DataFrame,
    schema: PanelSchema = DEFAULT_PANEL_SCHEMA,
) -> pd.DataFrame:
    """Convert a date-by-entity matrix into canonical long-form panel data."""

    panel = matrix.copy()
    panel.index.name = schema.date
    return (
        panel.reset_index()
        .melt(id_vars=schema.date, var_name=schema.entity, value_name=schema.value)
        .sort_values([schema.entity, schema.date])
        .reset_index(drop=True)
    )
