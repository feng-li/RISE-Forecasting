"""Configuration objects for recovery-informed forecasting pipelines."""

from __future__ import annotations

from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Literal

import yaml

CurveName = Literal["linear", "quadratic", "logistic"]
EnsembleName = Literal["mean", "error_weighted", "ridge", "lasso"]
ReferenceMethodName = Literal["ratio", "growth_rate", "arimax"]
RatioStatisticName = Literal["mean", "median"]
RecoveryCoefficientMethodName = Literal["direct", "weighted_score", "regression"]

DEFAULT_BASE_MODELS: tuple[str, ...] = (
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
DEFAULT_INITIAL_MODELS: tuple[str, ...] = (
    "seasonal_naive",
    "random_walk_drift",
    "arima",
    "ets",
)


@dataclass(frozen=True)
class ReferenceXConfig:
    """One exogenous-variable case for the reference forecast stage."""

    name: str
    variables: tuple[str, ...]
    method: ReferenceMethodName
    lag: int = 0
    ratio_window: int | None = 24
    ratio_statistic: RatioStatisticName = "mean"
    base_date: str | None = None
    seasonal_period: int = 12
    min_train_size: int = 8


@dataclass(frozen=True)
class BaseForecastConfig:
    """Configuration for the counterfactual baseline stage."""

    train_end: str
    validation_start: str | None = None
    validation_end: str | None = None
    horizon: int = 24
    models: tuple[str, ...] = DEFAULT_BASE_MODELS
    ensemble: EnsembleName = "mean"


@dataclass(frozen=True)
class ReferenceForecastConfig:
    """Configuration for the exogenous-variable reference forecast stage."""

    start: str
    end: str
    train_end: str | None = None
    x: tuple[ReferenceXConfig, ...] = ()
    signals: tuple[str, ...] = ()
    methods: tuple[ReferenceMethodName, ...] = ("ratio", "growth_rate")
    signal_lag: int = 0
    ratio_window: int | None = 24
    combine: EnsembleName = "mean"


@dataclass(frozen=True)
class InitialForecastConfig:
    """Configuration for model-based initial recovery forecasts."""

    train_end: str | None = None
    models: tuple[str, ...] = DEFAULT_INITIAL_MODELS
    ensemble: EnsembleName = "mean"


@dataclass(frozen=True)
class RecoveryCoefficientConfig:
    """Configuration for destination/entity recovery coefficients."""

    method: RecoveryCoefficientMethodName = "direct"
    coefficient_column: str = "coefficient"
    score_columns: tuple[str, ...] = ("policy", "distance", "recovery")
    weights: dict[str, float] = field(default_factory=dict)
    anchors: dict[str, float] = field(default_factory=dict)
    default_coefficient: float = 1.0
    min_coefficient: float = 0.0
    max_coefficient: float = 1.0
    score_min: float = 1.0
    score_max: float = 5.0
    fit_intercept: bool = True
    preserve_anchors: bool = True


@dataclass(frozen=True)
class CurveConfig:
    """Configuration for recovery curve interpolation."""

    curves: tuple[CurveName, ...] = ("linear", "quadratic", "logistic")
    seasonal_period: int = 12
    combine: EnsembleName = "mean"
    trend_history_start: str | None = None
    trend_history_end: str | None = None
    quadratic_terminal_weight: float = 18.0
    logistic_anchor_dates: tuple[str, ...] = ()


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

    @classmethod
    def from_yaml(cls, path: str | Path) -> PipelineConfig:
        """Load a pipeline config from YAML."""

        with Path(path).open("r", encoding="utf-8") as handle:
            data = yaml.safe_load(handle) or {}
        return cls.from_dict(data)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> PipelineConfig:
        """Build a pipeline config from a recovery dataset config dictionary."""

        dates = data.get("dates", {})
        shock = data.get("shock", {})
        reference_data = data.get("reference")
        return cls(
            shock_start=_get_required(data, dates, shock, "shock_start", "start"),
            initial_date=_get_required(data, dates, shock, "initial_date"),
            terminal_date=_get_required(data, dates, shock, "terminal_date"),
            forecast_start=_get_required(data, dates, shock, "forecast_start"),
            forecast_end=_get_required(data, dates, shock, "forecast_end"),
            frequency=str(data.get("frequency", "MS")),
            initial=_parse_initial(data.get("initial")),
            base=_parse_base(data.get("base")),
            reference=(
                None
                if reference_data is None
                else _parse_reference(reference_data, dates)
            ),
            recovery=_parse_recovery(data.get("recovery")),
            curve=_parse_curve(data.get("curve")),
        )


def _get_required(
    data: dict[str, Any],
    dates: dict[str, Any],
    shock: dict[str, Any],
    root_key: str,
    shock_key: str | None = None,
) -> str:
    if root_key in data:
        return str(data[root_key])
    if root_key in dates:
        return str(dates[root_key])
    lookup_key = shock_key or root_key
    if lookup_key in shock:
        return str(shock[lookup_key])
    raise ValueError(f"Missing required pipeline config value: {root_key}")


def _parse_base(data: Any) -> BaseForecastConfig | None:
    if data is None:
        return None
    return BaseForecastConfig(
        train_end=str(data["train_end"]),
        validation_start=_optional_str(data.get("validation_start")),
        validation_end=_optional_str(data.get("validation_end")),
        horizon=int(data.get("horizon", 24)),
        models=tuple(data.get("models", DEFAULT_BASE_MODELS)),
        ensemble=data.get("ensemble", "mean"),
    )


def _parse_initial(data: Any) -> InitialForecastConfig | None:
    if data is None:
        return None
    return InitialForecastConfig(
        train_end=_optional_str(data.get("train_end")),
        models=tuple(data.get("models", DEFAULT_INITIAL_MODELS)),
        ensemble=data.get("ensemble", "mean"),
    )


def _parse_reference(
    data: dict[str, Any],
    dates: dict[str, Any],
) -> ReferenceForecastConfig:
    start = data.get("start", dates.get("observed_until"))
    end = data.get("end", dates.get("initial_date"))
    if start is None:
        raise ValueError("reference.start is required when reference is configured.")
    if end is None:
        raise ValueError("reference.end is required when reference is configured.")
    return ReferenceForecastConfig(
        start=str(start),
        end=str(end),
        train_end=_optional_str(data.get("train_end", dates.get("observed_until"))),
        x=tuple(_parse_reference_x(item) for item in data.get("x", ())),
        signals=tuple(data.get("signals", ())),
        methods=tuple(data.get("methods", ("ratio", "growth_rate"))),
        signal_lag=int(data.get("signal_lag", 0)),
        ratio_window=data.get("ratio_window", 24),
        combine=data.get("combine", "mean"),
    )


def _parse_reference_x(data: dict[str, Any]) -> ReferenceXConfig:
    variables = data.get("variables", data.get("signals", data.get("signal_name")))
    if variables is None:
        raise ValueError("Each reference.x item must define variables.")
    if isinstance(variables, str):
        variables = (variables,)
    return ReferenceXConfig(
        name=str(data["name"]),
        variables=tuple(str(item) for item in variables),
        method=data["method"],
        lag=int(data.get("lag", data.get("signal_lag", 0))),
        ratio_window=data.get("ratio_window", 24),
        ratio_statistic=data.get("ratio_statistic", "mean"),
        base_date=_optional_str(data.get("base_date")),
        seasonal_period=int(data.get("seasonal_period", 12)),
        min_train_size=int(data.get("min_train_size", 8)),
    )


def _parse_recovery(data: Any) -> RecoveryCoefficientConfig:
    if data is None:
        return RecoveryCoefficientConfig()
    return RecoveryCoefficientConfig(
        method=data.get("method", "direct"),
        coefficient_column=str(data.get("coefficient_column", "coefficient")),
        score_columns=tuple(
            data.get("score_columns", ("policy", "distance", "recovery"))
        ),
        weights={
            str(key): float(value)
            for key, value in dict(data.get("weights", {})).items()
        },
        anchors=dict(data.get("anchors", {})),
        default_coefficient=float(data.get("default_coefficient", 1.0)),
        min_coefficient=float(data.get("min_coefficient", 0.0)),
        max_coefficient=float(data.get("max_coefficient", 1.0)),
        score_min=float(data.get("score_min", 1.0)),
        score_max=float(data.get("score_max", 5.0)),
        fit_intercept=bool(data.get("fit_intercept", True)),
        preserve_anchors=bool(data.get("preserve_anchors", True)),
    )


def _parse_curve(data: Any) -> CurveConfig:
    if data is None:
        return CurveConfig()
    return CurveConfig(
        curves=tuple(data.get("curves", ("linear", "quadratic", "logistic"))),
        seasonal_period=int(data.get("seasonal_period", 12)),
        combine=data.get("combine", "mean"),
        trend_history_start=_optional_str(data.get("trend_history_start")),
        trend_history_end=_optional_str(data.get("trend_history_end")),
        quadratic_terminal_weight=float(data.get("quadratic_terminal_weight", 18.0)),
        logistic_anchor_dates=tuple(data.get("logistic_anchor_dates", ())),
    )


def _optional_str(value: Any) -> str | None:
    return None if value is None else str(value)
