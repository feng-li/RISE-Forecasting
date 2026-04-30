"""Configuration objects for recovery-informed forecasting pipelines."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Literal

CurveName = Literal["linear", "quadratic", "logistic"]
EnsembleName = Literal["mean", "error_weighted", "ridge", "lasso"]
ReferenceMethodName = Literal["ratio", "exogenous", "growth_rate"]


@dataclass(frozen=True)
class BaseForecastConfig:
    """Configuration for the counterfactual baseline stage."""

    train_end: str
    validation_start: str | None = None
    validation_end: str | None = None
    horizon: int = 24
    models: tuple[str, ...] = (
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
        "holt",
        "holt_winters",
        "stl_arima",
        "stl_ets",
        "tbats",
        "nnetar",
    )
    ensemble: EnsembleName = "mean"


@dataclass(frozen=True)
class ReferenceForecastConfig:
    """Configuration for the external-signal reference forecast stage."""

    start: str
    end: str
    methods: tuple[ReferenceMethodName, ...] = ("ratio", "growth_rate")
    signal_lag: int = 1
    combine: EnsembleName = "mean"


@dataclass(frozen=True)
class InitialForecastConfig:
    """Configuration for model-based initial recovery forecasts."""

    train_end: str | None = None
    models: tuple[str, ...] = (
        "seasonal_naive",
        "random_walk_drift",
        "arima",
        "ets",
    )
    ensemble: EnsembleName = "mean"


@dataclass(frozen=True)
class RecoveryCoefficientConfig:
    """Configuration for destination/entity recovery coefficients."""

    score_columns: tuple[str, ...] = ("policy", "distance", "recovery")
    anchors: dict[str, float] = field(default_factory=dict)
    default_coefficient: float = 1.0
    min_coefficient: float = 0.0
    max_coefficient: float = 1.0


@dataclass(frozen=True)
class CurveConfig:
    """Configuration for recovery curve interpolation."""

    curves: tuple[CurveName, ...] = ("linear", "quadratic", "logistic")
    seasonal_period: int = 12
    combine: EnsembleName = "mean"


@dataclass(frozen=True)
class PipelineConfig:
    """Top-level configuration for a recovery-informed forecast."""

    shock_start: str
    initial_date: str
    terminal_date: str
    forecast_start: str
    forecast_end: str
    frequency: str = "MS"
    initial: InitialForecastConfig | None = None
    base: BaseForecastConfig | None = None
    reference: ReferenceForecastConfig | None = None
    recovery: RecoveryCoefficientConfig = field(
        default_factory=RecoveryCoefficientConfig
    )
    curve: CurveConfig = field(default_factory=CurveConfig)
