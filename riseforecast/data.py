"""Canonical data structures and validation helpers."""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

import pandas as pd
import yaml

if TYPE_CHECKING:
    from riseforecast.config import PipelineConfig
    from riseforecast.hierarchy import HierarchySpec


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
class RecoveryDatasetSchema:
    """Column names for the compact recovery forecasting panel format."""

    date: str = "date"
    series_id: str = "series_id"
    kind: str = "kind"
    name: str = "name"
    value: str = "value"
    lower: str = "lower"
    upper: str = "upper"


@dataclass(frozen=True)
class ForecastFrame:
    """A forecast matrix indexed by date with one column per entity."""

    values: pd.DataFrame
    lower: pd.DataFrame | None = None
    upper: pd.DataFrame | None = None


DEFAULT_PANEL_SCHEMA = PanelSchema()
DEFAULT_SIGNAL_SCHEMA = SignalSchema()
DEFAULT_RECOVERY_DATASET_SCHEMA = RecoveryDatasetSchema()


@dataclass(frozen=True)
class RecoveryDataset:
    """Three-file recovery forecasting dataset.

    The dataset consists of `series.csv`, `panel.csv`, and `config.yaml`.
    """

    series: pd.DataFrame
    panel: pd.DataFrame
    config: dict[str, Any]
    schema: RecoveryDatasetSchema = DEFAULT_RECOVERY_DATASET_SCHEMA

    @classmethod
    def from_directory(
        cls,
        directory: str | Path,
        schema: RecoveryDatasetSchema = DEFAULT_RECOVERY_DATASET_SCHEMA,
    ) -> RecoveryDataset:
        """Load `series.csv`, `panel.csv`, and `config.yaml` from a directory."""

        data_dir = Path(directory)
        series_path = data_dir / "series.csv"
        panel_path = data_dir / "panel.csv"
        config_path = data_dir / "config.yaml"
        missing = [
            str(path)
            for path in (series_path, panel_path, config_path)
            if not path.exists()
        ]
        if missing:
            raise FileNotFoundError(
                f"Missing recovery dataset files: {', '.join(missing)}"
            )

        series = pd.read_csv(series_path)
        panel = pd.read_csv(panel_path)
        with config_path.open("r", encoding="utf-8") as handle:
            config = yaml.safe_load(handle) or {}
        return cls(series=series, panel=panel, config=config, schema=schema).validate()

    def validate(self) -> RecoveryDataset:
        """Validate required columns and normalize date ordering."""

        require_columns(
            self.series,
            ("series_id", "series_name", "target_name", "unit"),
        )
        require_columns(
            self.panel,
            (
                self.schema.date,
                self.schema.series_id,
                self.schema.kind,
                self.schema.name,
                self.schema.value,
                self.schema.lower,
                self.schema.upper,
            ),
        )
        unknown = set(self.panel[self.schema.series_id]) - set(self.series["series_id"])
        if unknown:
            joined = ", ".join(sorted(str(item) for item in unknown))
            raise ValueError(f"Panel contains unknown series_id values: {joined}")

        series = self.series.copy()
        panel = self.panel.copy()
        panel[self.schema.date] = pd.to_datetime(panel[self.schema.date])
        panel = panel.sort_values(
            [
                self.schema.series_id,
                self.schema.date,
                self.schema.kind,
                self.schema.name,
            ]
        ).reset_index(drop=True)
        return RecoveryDataset(
            series=series,
            panel=panel,
            config=self.config,
            schema=self.schema,
        )

    @property
    def series_ids(self) -> pd.Index:
        """Series identifiers in metadata order."""

        return pd.Index(self.series["series_id"])

    def filter_panel(self, kind: str, name: str | None = None) -> pd.DataFrame:
        """Filter panel rows by kind and optional name."""

        mask = self.panel[self.schema.kind] == kind
        if name is not None:
            mask &= self.panel[self.schema.name] == name
        return self.panel.loc[mask].copy()

    def matrix(self, kind: str, name: str, value_column: str = "value") -> pd.DataFrame:
        """Return a date-by-series matrix for one kind/name pair."""

        allowed = {self.schema.value, self.schema.lower, self.schema.upper}
        if value_column not in allowed:
            raise ValueError("value_column must be one of value, lower, or upper.")
        data = self.filter_panel(kind=kind, name=name)
        if data.empty:
            raise ValueError(f"No panel rows found for kind={kind!r}, name={name!r}.")
        matrix = data.pivot(
            index=self.schema.date,
            columns=self.schema.series_id,
            values=value_column,
        )
        matrix = matrix.reindex(columns=self.series_ids)
        matrix.index = pd.DatetimeIndex(matrix.index)
        return matrix.sort_index()

    def forecast_frame(self, kind: str, name: str) -> ForecastFrame:
        """Return a forecast matrix plus optional lower/upper bounds."""

        values = self.matrix(kind=kind, name=name, value_column=self.schema.value)
        lower = self._optional_matrix(
            kind=kind,
            name=name,
            value_column=self.schema.lower,
        )
        upper = self._optional_matrix(
            kind=kind,
            name=name,
            value_column=self.schema.upper,
        )
        return ForecastFrame(values=values, lower=lower, upper=upper)

    def coefficients(self, column: str = "coefficient") -> pd.Series:
        """Return intervention coefficients indexed by series_id."""

        if column not in self.series.columns:
            raise ValueError(f"Missing coefficient column: {column}")
        coefficients = self.series.set_index("series_id")[column].astype(float)
        return coefficients.reindex(self.series_ids)

    def metadata(self) -> pd.DataFrame:
        """Return static series metadata."""

        return self.series.copy()

    def hierarchy(self, parent_column: str = "parent_id") -> HierarchySpec | None:
        """Return hierarchy metadata when `parent_column` is present."""

        from riseforecast.hierarchy import hierarchy_from_series

        return hierarchy_from_series(self.series, parent_column=parent_column)

    def pipeline_config(self) -> PipelineConfig:
        """Return this dataset's config as a `PipelineConfig`."""

        from riseforecast.config import PipelineConfig

        return PipelineConfig.from_dict(self.config)

    def observed_target(self) -> pd.DataFrame:
        """Return observed target values."""

        return self.matrix(kind="observed", name="target")

    def signal(self, name: str) -> pd.DataFrame:
        """Return one external signal matrix."""

        return self.matrix(kind="signal", name=name)

    def exogenous(self, name: str) -> pd.DataFrame:
        """Return one exogenous variable matrix."""

        try:
            return self.matrix(kind="exogenous", name=name)
        except ValueError:
            return self.matrix(kind="signal", name=name)

    def signals(self) -> dict[str, pd.DataFrame]:
        """Return all external signal matrices keyed by signal name."""

        signal_rows = self.filter_panel(kind="signal")
        return {
            str(name): self.matrix(kind="signal", name=str(name))
            for name in sorted(signal_rows[self.schema.name].dropna().unique())
        }

    def exogenous_variables(self) -> dict[str, pd.DataFrame]:
        """Return all exogenous variable matrices keyed by variable name."""

        rows = self.panel.loc[
            self.panel[self.schema.kind].isin(["signal", "exogenous"])
        ]
        variables = sorted(rows[self.schema.name].dropna().unique())
        return {str(name): self.exogenous(str(name)) for name in variables}

    def base_forecast(self, name: str = "legacy_ensemble") -> pd.DataFrame:
        """Return baseline/counterfactual forecast values."""

        return self.matrix(kind="base_forecast", name=name)

    def reference_forecast(self, name: str = "legacy_average") -> pd.DataFrame:
        """Return reference/initial forecast values."""

        return self.matrix(kind="reference_forecast", name=name)

    def recovery_forecast(self, name: str = "legacy_final") -> pd.DataFrame:
        """Return final recovery forecast values."""

        return self.matrix(kind="recovery_forecast", name=name)

    def _optional_matrix(
        self,
        kind: str,
        name: str,
        value_column: str,
    ) -> pd.DataFrame | None:
        matrix = self.matrix(kind=kind, name=name, value_column=value_column)
        if matrix.isna().all().all():
            return None
        return matrix


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
