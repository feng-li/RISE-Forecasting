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
    forecast_panel,
    forecast_series,
)
from riseforecast.config import PipelineConfig, ReferenceXConfig
from riseforecast.curves import (
    RecoveryCurveForecast,
    RecoveryCurveForecaster,
    extract_trend_component,
    recover_full_forecast,
    recovery_curve_forecast,
)
from riseforecast.data import RecoveryDataset
from riseforecast.ensembles import (
    combine_panel_forecasts,
    error_weighted_average,
    forecast_errors,
    positive_stacking_panel,
    select_top_models,
    simple_average,
)
from riseforecast.evaluation import evaluate_forecast_matrix
from riseforecast.hierarchy import (
    HierarchySpec,
    ReconciliationResult,
    aggregate_bottom_level,
    bottom_up_reconcile,
    build_summing_matrix,
    hierarchy_from_series,
    reconcile_forecasts,
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

try:
    __version__ = version("riseforecast")
except PackageNotFoundError:  # pragma: no cover - editable tree before install
    __version__ = "0.0.0"

__all__ = [
    "BaseForecast",
    "BaseForecaster",
    "InitialForecast",
    "InitialForecaster",
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
    "extract_trend_component",
    "FactorCoefficientRegression",
    "forecast_errors",
    "forecast_panel",
    "forecast_series",
    "hierarchy_from_series",
    "initial_forecast",
    "intervention_terminal_forecast",
    "positive_stacking_panel",
    "recover_full_forecast",
    "reference_forecast",
    "reference_specs_from_config",
    "reconcile_forecasts",
    "regress_recovery_coefficients",
    "recovery_curve_forecast",
    "select_top_models",
    "simple_average",
    "stl_monthly_seasonal_multipliers",
    "weighted_factor_matrix",
    "weighted_recovery_score",
]
