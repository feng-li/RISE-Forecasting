"""Recovery-informed forecasting tools.

The package exposes a reusable implementation surface for the RISE forecasting
scheme: estimate a no-shock baseline, estimate the current recovery state from
external signals, apply recovery coefficients, then connect the anchors with a
recovery curve.
"""

from importlib.metadata import PackageNotFoundError, version

from riseforecast.config import PipelineConfig
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
    "InterventionTerminalForecast",
    "InterventionTerminalForecaster",
    "PipelineConfig",
    "RecoveryForecastingPipeline",
    "__version__",
    "intervention_terminal_forecast",
]
