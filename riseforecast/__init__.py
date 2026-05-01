"""Recovery-informed forecasting tools.

The package exposes a reusable implementation surface for the RISE forecasting
scheme: estimate a no-shock baseline, estimate the current recovery state from
external signals, apply recovery coefficients, then connect the anchors with a
recovery curve.
"""

from importlib.metadata import PackageNotFoundError, version

from riseforecast.base_models import (
    BaseForecast,
    BaseForecaster,
    default_model_registry,
    forecast_hierarchical_panel,
    forecast_panel,
    forecast_series,
    is_hierarchical_base_model,
    parse_hierarchical_base_model,
)
from riseforecast.config import IntervalConfig, PipelineConfig, ReferenceXConfig
from riseforecast.curves import (
    RecoveryCurveForecast,
    RecoveryCurveForecaster,
    extract_trend_component,
    recover_full_forecast,
    recovery_curve_forecast,
)
from riseforecast.data import ForecastFrame, RecoveryDataset
from riseforecast.ensembles import (
    combine_panel_forecasts,
    error_weighted_average,
    forecast_errors,
    positive_stacking_panel,
    select_top_models,
    simple_average,
)
from riseforecast.evaluation import evaluate_forecast_matrix, evaluate_interval_matrix
from riseforecast.hierarchy import (
    HierarchySpec,
    ReconciliationResult,
    aggregate_bottom_level,
    bottom_up_reconcile,
    build_summing_matrix,
    hierarchicalforecast_reconcile,
    hierarchy_from_series,
    reconcile_forecasts,
    reconciliation_requires_insample,
)
from riseforecast.initial import (
    InitialForecast,
    InitialForecaster,
    initial_forecast,
)
from riseforecast.intervention import (
    InterventionTerminalForecast,
    InterventionTerminalForecaster,
    intervention_terminal_forecast,
)
from riseforecast.metrics import interval_coverage, winkler_score
from riseforecast.pipeline import RecoveryForecastingPipeline
from riseforecast.preprocessing import stl_monthly_seasonal_multipliers
from riseforecast.recovery import (
    FactorCoefficientRegression,
    RecoveryCoefficientEstimator,
    RecoveryCoefficientRegression,
    apply_recovery_coefficients,
    average_recovery_score,
    direct_recovery_coefficients,
    regress_recovery_coefficients,
    weighted_factor_matrix,
    weighted_recovery_score,
)
from riseforecast.reference import (
    ReferenceForecast,
    ReferenceForecaster,
    ReferenceSignalSpec,
    ReferenceXSpec,
    reference_forecast,
    reference_specs_from_config,
)
from riseforecast.visualization import plot_forecast, plot_recovery_curve

try:
    __version__ = version("riseforecast")
except PackageNotFoundError:  # pragma: no cover - editable tree before install
    __version__ = "0.0.0"

__all__ = [
    "BaseForecast",
    "BaseForecaster",
    "InitialForecast",
    "InitialForecaster",
    "IntervalConfig",
    "InterventionTerminalForecast",
    "InterventionTerminalForecaster",
    "HierarchySpec",
    "PipelineConfig",
    "ReferenceForecast",
    "ReferenceForecaster",
    "ReferenceSignalSpec",
    "ReferenceXConfig",
    "ReferenceXSpec",
    "RecoveryCurveForecast",
    "RecoveryCurveForecaster",
    "RecoveryCoefficientEstimator",
    "RecoveryCoefficientRegression",
    "RecoveryDataset",
    "RecoveryForecastingPipeline",
    "ReconciliationResult",
    "__version__",
    "aggregate_bottom_level",
    "apply_recovery_coefficients",
    "average_recovery_score",
    "bottom_up_reconcile",
    "build_summing_matrix",
    "combine_panel_forecasts",
    "default_model_registry",
    "direct_recovery_coefficients",
    "error_weighted_average",
    "evaluate_forecast_matrix",
    "evaluate_interval_matrix",
    "extract_trend_component",
    "FactorCoefficientRegression",
    "ForecastFrame",
    "forecast_errors",
    "forecast_hierarchical_panel",
    "forecast_panel",
    "forecast_series",
    "hierarchicalforecast_reconcile",
    "hierarchy_from_series",
    "initial_forecast",
    "intervention_terminal_forecast",
    "interval_coverage",
    "is_hierarchical_base_model",
    "parse_hierarchical_base_model",
    "positive_stacking_panel",
    "plot_forecast",
    "plot_recovery_curve",
    "recover_full_forecast",
    "reference_forecast",
    "reference_specs_from_config",
    "reconcile_forecasts",
    "reconciliation_requires_insample",
    "regress_recovery_coefficients",
    "recovery_curve_forecast",
    "select_top_models",
    "simple_average",
    "stl_monthly_seasonal_multipliers",
    "weighted_factor_matrix",
    "weighted_recovery_score",
    "winkler_score",
]
