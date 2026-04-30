"""End-to-end recovery forecasting pipeline interface."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

from riseforecast.config import PipelineConfig

if TYPE_CHECKING:
    import pandas as pd

    from riseforecast.curves import RecoveryCurveForecast
    from riseforecast.data import ForecastFrame
    from riseforecast.initial import InitialForecast
    from riseforecast.intervention import InterventionTerminalForecast


@dataclass
class PipelineState:
    """Fitted artifacts produced by the three RISE stages."""

    base_forecast: ForecastFrame | None = None
    initial_forecast: InitialForecast | None = None
    reference_forecast: ForecastFrame | None = None
    recovery_coefficients: pd.Series | None = None
    terminal_forecast: InterventionTerminalForecast | None = None
    recovery_curve_forecast: RecoveryCurveForecast | None = None


@dataclass
class RecoveryForecastingPipeline:
    """High-level orchestration object for RISE-style forecasting.

    The initial skeleton records the public API and fitted artifacts. Concrete model
    ports will fill in the stage implementations.
    """

    config: PipelineConfig
    state: PipelineState = field(default_factory=PipelineState)

    def fit(
        self,
        observed: pd.DataFrame,
        external_signals: pd.DataFrame | None = None,
        hierarchy: object | None = None,
        recovery_scores: pd.DataFrame | None = None,
    ) -> RecoveryForecastingPipeline:
        """Fit the recovery forecasting pipeline.

        This method is intentionally thin in the package skeleton. The migration work
        should implement each stage behind this stable interface.
        """

        self._observed_columns = tuple(observed.columns)
        self._has_external_signals = external_signals is not None
        self._has_hierarchy = hierarchy is not None
        self._has_recovery_scores = recovery_scores is not None
        if self.config.initial is not None:
            from riseforecast.initial import InitialForecaster

            self.state.initial_forecast = InitialForecaster(
                initial_date=self.config.initial_date,
                train_end=self.config.initial.train_end,
                models=self.config.initial.models,
                ensemble=self.config.initial.ensemble,
                frequency=self.config.frequency,
            ).forecast(observed)
            self.state.reference_forecast = (
                self.state.initial_forecast.as_forecast_frame()
            )
        return self

    def predict(self) -> ForecastFrame:
        """Return fitted recovery forecasts once stage implementations are available."""

        raise NotImplementedError(
            "RecoveryForecastingPipeline.predict will be implemented after the "
            "base, reference, recovery coefficient, and curve stages are ported."
        )
