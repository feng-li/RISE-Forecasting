"""End-to-end recovery forecasting pipeline interface."""

from __future__ import annotations

import warnings
from collections.abc import Mapping
from dataclasses import dataclass, field
from typing import TYPE_CHECKING

import pandas as pd

from riseforecast.config import PipelineConfig

if TYPE_CHECKING:
    from riseforecast.curves import RecoveryCurveForecast
    from riseforecast.data import ForecastFrame, RecoveryDataset
    from riseforecast.initial import InitialForecast
    from riseforecast.intervention import InterventionTerminalForecast


@dataclass
class PipelineState:
    """Fitted artifacts produced by the three RISE stages."""

    base_forecast: ForecastFrame | None = None
    initial_forecast: InitialForecast | None = None
    reference_forecast: ForecastFrame | None = None
    recovery_coefficients: pd.Series | None = None
    seasonal_multipliers: pd.DataFrame | None = None
    trend_history: pd.DataFrame | None = None
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

    @classmethod
    def from_dataset(cls, dataset: RecoveryDataset) -> RecoveryForecastingPipeline:
        """Create a pipeline from a recovery dataset's config."""

        return cls(dataset.pipeline_config())

    def fit(
        self,
        observed: pd.DataFrame,
        external_signals: pd.DataFrame | Mapping[str, pd.DataFrame] | None = None,
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
        if self.config.reference is not None and external_signals is not None:
            from riseforecast.reference import (
                ReferenceForecaster,
                ReferenceSignalSpec,
                coerce_signal_matrices,
            )

            signal_matrices = coerce_signal_matrices(external_signals)
            if self.config.reference.x:
                specs = tuple(
                    ReferenceSignalSpec(
                        name=item.name,
                        signal_name=(
                            item.variables[0]
                            if len(item.variables) == 1
                            else item.variables
                        ),
                        method=item.method,
                        signal_lag=item.lag,
                        ratio_window=item.ratio_window,
                        ratio_statistic=item.ratio_statistic,
                        base_date=item.base_date,
                        seasonal_period=item.seasonal_period,
                        min_train_size=item.min_train_size,
                    )
                    for item in self.config.reference.x
                )
            else:
                signal_names = self.config.reference.signals or tuple(signal_matrices)
                specs = tuple(
                    ReferenceSignalSpec(
                        signal_name=signal_name,
                        method=method,
                        signal_lag=self.config.reference.signal_lag,
                        ratio_window=self.config.reference.ratio_window,
                    )
                    for signal_name in signal_names
                    for method in self.config.reference.methods
                )
            self.state.reference_forecast = ReferenceForecaster(
                start=self.config.reference.start,
                end=self.config.reference.end,
                train_end=self.config.reference.train_end,
                specs=specs,
                frequency=self.config.frequency,
                combine=self.config.reference.combine,
            ).forecast(observed, signal_matrices).as_forecast_frame()
        return self

    def fit_dataset(
        self,
        dataset: RecoveryDataset,
        base_forecast_name: str = "legacy_ensemble",
        reference_forecast_name: str = "legacy_average",
    ) -> RecoveryForecastingPipeline:
        """Fit all implemented stages using a `RecoveryDataset`."""

        observed = dataset.observed_target()
        self.fit(
            observed=observed,
            external_signals=dataset.exogenous_variables(),
        )
        self.state.base_forecast = self._fit_base_forecast(
            dataset=dataset,
            observed=observed,
            fallback_name=base_forecast_name,
        )
        self.state.seasonal_multipliers = self._seasonal_multipliers(
            observed=observed,
            base_forecast=self.state.base_forecast.values,
        )
        self.state.recovery_coefficients = dataset.coefficients()

        from riseforecast.intervention import intervention_terminal_forecast

        self.state.terminal_forecast = intervention_terminal_forecast(
            base_forecast=self.state.base_forecast,
            coefficients=self.state.recovery_coefficients,
            terminal_date=self.config.terminal_date,
            coefficient_bounds=(
                self.config.recovery.min_coefficient,
                self.config.recovery.max_coefficient,
            ),
        )

        initial_forecast = self._initial_forecast_for_curve(
            dataset=dataset,
            fallback_name=reference_forecast_name,
        )
        self.state.trend_history = self._trend_history_for_curve(
            observed=observed,
            initial_forecast=initial_forecast,
        )
        from riseforecast.curves import RecoveryCurveForecaster

        self.state.recovery_curve_forecast = RecoveryCurveForecaster(
            initial_date=self.config.initial_date,
            forecast_start=self.config.forecast_start,
            forecast_end=self.config.forecast_end,
            frequency=self.config.frequency,
            curve_names=self.config.curve.curves,
            quadratic_terminal_weight=self.config.curve.quadratic_terminal_weight,
            logistic_anchor_dates=self.config.curve.logistic_anchor_dates,
        ).forecast(
            initial_forecast=initial_forecast,
            terminal_forecast=self.state.terminal_forecast,
            seasonal_multipliers=self.state.seasonal_multipliers,
            trend_history=self.state.trend_history,
            base_forecast=self.state.base_forecast.values,
        )
        return self

    def predict(self) -> ForecastFrame:
        """Return the final fitted recovery forecast."""

        if self.state.recovery_curve_forecast is None:
            raise RuntimeError(
                "RecoveryForecastingPipeline must be fitted before predict."
            )
        from riseforecast.data import ForecastFrame

        return ForecastFrame(values=self.state.recovery_curve_forecast.values)

    def _fit_base_forecast(
        self,
        dataset: RecoveryDataset,
        observed: pd.DataFrame,
        fallback_name: str,
    ) -> ForecastFrame:
        from riseforecast.data import ForecastFrame

        if self.config.base is None:
            return dataset.forecast_frame(kind="base_forecast", name=fallback_name)
        if self.config.base.ensemble != "mean":
            raise ValueError("Only mean base forecast ensembles are implemented.")

        from riseforecast.base_models import forecast_panel
        from riseforecast.ensembles import simple_average

        horizon = max(
            self.config.base.horizon,
            _horizon_to_date(
                train_end=pd.Timestamp(self.config.base.train_end),
                forecast_end=pd.Timestamp(self.config.terminal_date),
                frequency=self.config.frequency,
            ),
        )
        forecasts = forecast_panel(
            observed=observed,
            models=self.config.base.models,
            horizon=horizon,
            train_end=self.config.base.train_end,
            frequency=self.config.frequency,
        )
        values = simple_average(
            {name: forecast.values for name, forecast in forecasts.items()}
        )
        return ForecastFrame(values=values)

    def _initial_forecast_for_curve(
        self,
        dataset: RecoveryDataset,
        fallback_name: str,
    ) -> pd.DataFrame:
        if self.state.reference_forecast is not None:
            return self.state.reference_forecast.values
        if self.state.initial_forecast is not None:
            return self.state.initial_forecast.values
        return dataset.reference_forecast(name=fallback_name)

    def _seasonal_multipliers(
        self,
        observed: pd.DataFrame,
        base_forecast: pd.DataFrame,
    ) -> pd.DataFrame | None:
        from riseforecast.preprocessing import stl_monthly_seasonal_multipliers

        period = self.config.curve.seasonal_period
        seasonality_train = self._seasonality_training_sample(observed)
        if len(seasonality_train.dropna(how="all")) >= period * 2:
            try:
                return stl_monthly_seasonal_multipliers(
                    seasonality_train,
                    period=period,
                )
            except ValueError as exc:
                warnings.warn(
                    f"Skipping STL seasonal decomposition of observed history: {exc}",
                    stacklevel=2,
                )

        if len(base_forecast.dropna(how="all")) < period * 2:
            return None

        try:
            return stl_monthly_seasonal_multipliers(
                base_forecast,
                period=period,
            )
        except ValueError as exc:
            warnings.warn(
                f"Skipping STL seasonal decomposition of base forecast: {exc}",
                stacklevel=2,
            )
            return None

    def _seasonality_training_sample(self, observed: pd.DataFrame) -> pd.DataFrame:
        if self.config.base is not None:
            end = pd.Timestamp(self.config.base.train_end)
        else:
            offset = pd.tseries.frequencies.to_offset(self.config.frequency)
            end = pd.Timestamp(self.config.shock_start) - offset
        return observed.loc[:end]

    def _trend_history_for_curve(
        self,
        observed: pd.DataFrame,
        initial_forecast: pd.DataFrame,
    ) -> pd.DataFrame | None:
        if self.config.curve.trend_history_start is None:
            return None

        start = pd.Timestamp(self.config.curve.trend_history_start)
        end = pd.Timestamp(
            self.config.curve.trend_history_end or self.config.initial_date
        )
        dates = pd.date_range(start, end, freq=self.config.frequency)
        if dates.empty:
            return None

        history = observed.reindex(dates).combine_first(
            initial_forecast.reindex(dates)
        )
        if history.dropna(how="all").empty:
            return None

        from riseforecast.curves import extract_trend_component

        return extract_trend_component(history, self.state.seasonal_multipliers)


def _horizon_to_date(
    train_end: pd.Timestamp,
    forecast_end: pd.Timestamp,
    frequency: str,
) -> int:
    offset = pd.tseries.frequencies.to_offset(frequency)
    dates = pd.date_range(train_end + offset, forecast_end, freq=frequency)
    if dates.empty:
        raise ValueError("forecast_end must be later than base train_end.")
    return len(dates)
