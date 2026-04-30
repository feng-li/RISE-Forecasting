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
from riseforecast.evaluation import evaluate_forecast_matrix
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
    "PipelineConfig",
    "ReferenceForecast",
    "ReferenceForecaster",
    "ReferenceSignalSpec",
    "ReferenceXConfig",
    "ReferenceXSpec",
    "RecoveryCurveForecast",
    "RecoveryCurveForecaster",
    "RecoveryDataset",
    "RecoveryForecastingPipeline",
    "__version__",
    "default_model_registry",
    "evaluate_forecast_matrix",
    "extract_trend_component",
    "forecast_panel",
    "forecast_series",
    "initial_forecast",
    "intervention_terminal_forecast",
    "recover_full_forecast",
    "reference_forecast",
    "reference_specs_from_config",
    "recovery_curve_forecast",
    "stl_monthly_seasonal_multipliers",
]
