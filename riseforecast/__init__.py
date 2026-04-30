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
from riseforecast.config import PipelineConfig
from riseforecast.curves import (
    RecoveryCurveForecast,
    RecoveryCurveForecaster,
    recovery_curve_forecast,
)
from riseforecast.data import RecoveryDataset
from riseforecast.intervention import (
    InterventionTerminalForecast,
    InterventionTerminalForecaster,
    intervention_terminal_forecast,
)
from riseforecast.pipeline import RecoveryForecastingPipeline

try:
    __version__ = version("riseforecast")
except PackageNotFoundError:  # pragma: no cover - editable tree before install
    __version__ = "0.0.0"

__all__ = [
    "BaseForecast",
    "BaseForecaster",
    "InterventionTerminalForecast",
    "InterventionTerminalForecaster",
    "PipelineConfig",
    "RecoveryCurveForecast",
    "RecoveryCurveForecaster",
    "RecoveryDataset",
    "RecoveryForecastingPipeline",
    "__version__",
    "default_model_registry",
    "forecast_panel",
    "forecast_series",
    "intervention_terminal_forecast",
    "recovery_curve_forecast",
]
