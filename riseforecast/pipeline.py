"""End-to-end recovery forecasting pipeline interface."""

from __future__ import annotations

import warnings
from collections.abc import Mapping
from dataclasses import dataclass, field, replace
from typing import TYPE_CHECKING

import pandas as pd

from riseforecast.config import PipelineConfig

if TYPE_CHECKING:
    from riseforecast.curves import RecoveryCurveForecast
    from riseforecast.data import ForecastFrame, RecoveryDataset
    from riseforecast.hierarchy import HierarchySpec, ReconciliationResult
    from riseforecast.initial import InitialForecast
    from riseforecast.intervention import InterventionTerminalForecast


@dataclass
class PipelineState:
    """Fitted artifacts produced by the three RISE stages."""

    base_forecast: ForecastFrame | None = None
    base_validation_errors: pd.Series | None = None
    base_selected_models: tuple[str, ...] = ()
    hierarchy: HierarchySpec | None = None
    hierarchy_reconciliation: ReconciliationResult | None = None
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

        self.state.hierarchy = self._dataset_hierarchy(dataset)
        observed = dataset.observed_target()
        model_observed = _bottom_level_matrix(observed, self.state.hierarchy)
        self.fit(
            observed=model_observed,
            external_signals=_bottom_level_signals(
                dataset.exogenous_variables(),
                self.state.hierarchy,
            ),
        )
        self.state.base_forecast = self._fit_base_forecast(
            dataset=dataset,
            observed=model_observed,
            fallback_name=base_forecast_name,
        )
        self.state.seasonal_multipliers = self._seasonal_multipliers(
            observed=model_observed,
            base_forecast=self.state.base_forecast.values,
        )
        self.state.recovery_coefficients = self._estimate_recovery_coefficients(
            dataset
        )

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
        initial_forecast = _bottom_level_forecast_frame(
            initial_forecast,
            self.state.hierarchy,
        )
        self.state.trend_history = self._trend_history_for_curve(
            observed=model_observed,
            initial_forecast=initial_forecast.values,
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
        self._reconcile_recovery_forecast()
        return self

    def predict(self) -> ForecastFrame:
        """Return the final fitted recovery forecast."""

        if self.state.recovery_curve_forecast is None:
            raise RuntimeError(
                "RecoveryForecastingPipeline must be fitted before predict."
            )
        from riseforecast.data import ForecastFrame

        return ForecastFrame(
            values=self.state.recovery_curve_forecast.values,
            lower=self.state.recovery_curve_forecast.lower,
            upper=self.state.recovery_curve_forecast.upper,
        )

    def _fit_base_forecast(
        self,
        dataset: RecoveryDataset,
        observed: pd.DataFrame,
        fallback_name: str,
    ) -> ForecastFrame:
        from riseforecast.data import ForecastFrame

        if self.config.base is None:
            return _bottom_level_forecast_frame(
                dataset.forecast_frame(kind="base_forecast", name=fallback_name),
                self.state.hierarchy,
            )

        from riseforecast.base_models import forecast_panel
        from riseforecast.ensembles import (
            combine_panel_forecasts,
            forecast_errors,
            select_top_models,
        )

        _validate_base_validation_config(self.config.base)
        horizon = max(
            self.config.base.horizon,
            _horizon_to_date(
                train_end=pd.Timestamp(self.config.base.train_end),
                forecast_end=pd.Timestamp(self.config.terminal_date),
                frequency=self.config.frequency,
            ),
        )
        validation_forecasts = None
        validation_actual = None
        selected_models = tuple(self.config.base.models)
        if self._has_base_validation_period():
            validation_actual = _validation_actual(
                observed=observed,
                validation_start=pd.Timestamp(self.config.base.validation_start),
                validation_end=pd.Timestamp(self.config.base.validation_end),
            )
            validation_horizon = _horizon_to_date(
                train_end=_previous_period(
                    pd.Timestamp(self.config.base.validation_start),
                    frequency=self.config.frequency,
                ),
                forecast_end=pd.Timestamp(self.config.base.validation_end),
                frequency=self.config.frequency,
            )
            validation_components = forecast_panel(
                observed=observed,
                models=self.config.base.models,
                horizon=validation_horizon,
                train_end=_previous_period(
                    pd.Timestamp(self.config.base.validation_start),
                    frequency=self.config.frequency,
                ),
                frequency=self.config.frequency,
                hierarchy=self.state.hierarchy,
            )
            validation_forecasts = {
                name: forecast.values.reindex(validation_actual.index)
                for name, forecast in validation_components.items()
            }
            self.state.base_validation_errors = forecast_errors(
                validation_forecasts,
                actual=validation_actual,
                metric=self.config.base.validation_metric,
            )
            selected_models = select_top_models(
                self.state.base_validation_errors,
                fraction=self.config.base.selection_fraction,
            )
        elif self.config.base.ensemble != "mean":
            raise ValueError(
                f"{self.config.base.ensemble} base ensemble requires "
                "validation_start and validation_end."
            )
        self.state.base_selected_models = selected_models

        forecasts = forecast_panel(
            observed=observed,
            models=selected_models,
            horizon=horizon,
            train_end=self.config.base.train_end,
            frequency=self.config.frequency,
            hierarchy=self.state.hierarchy,
        )
        future_forecasts = {
            name: forecast.values for name, forecast in forecasts.items()
        }
        if validation_forecasts is not None:
            validation_forecasts = {
                name: validation_forecasts[name] for name in selected_models
            }
        validation_errors = (
            None
            if self.state.base_validation_errors is None
            else self.state.base_validation_errors.loc[list(selected_models)]
        )
        values = combine_panel_forecasts(
            future_forecasts=future_forecasts,
            ensemble=self.config.base.ensemble,
            validation_errors=validation_errors,
            validation_forecasts=validation_forecasts,
            validation_actual=validation_actual,
            stacking_alpha=self.config.base.stacking_alpha,
        )
        lower = None
        upper = None
        if (
            self.config.interval.enabled
            and validation_forecasts is not None
            and validation_actual is not None
        ):
            try:
                validation_values = combine_panel_forecasts(
                    future_forecasts=validation_forecasts,
                    ensemble=self.config.base.ensemble,
                    validation_errors=validation_errors,
                    validation_forecasts=validation_forecasts,
                    validation_actual=validation_actual,
                    stacking_alpha=self.config.base.stacking_alpha,
                )
                from riseforecast.intervals import residual_quantile_bounds

                lower, upper = residual_quantile_bounds(
                    actual=validation_actual,
                    validation_forecast=validation_values,
                    future_forecast=values,
                    alpha=self.config.interval.alpha,
                )
            except ValueError as exc:
                warnings.warn(
                    f"Skipping residual-calibrated base intervals: {exc}",
                    stacklevel=2,
                )
        return ForecastFrame(values=values, lower=lower, upper=upper)

    def _dataset_hierarchy(self, dataset: RecoveryDataset) -> HierarchySpec | None:
        if not self.config.hierarchy.enabled:
            return None
        hierarchy = dataset.hierarchy(parent_column=self.config.hierarchy.parent_column)
        if hierarchy is None:
            raise ValueError(
                "hierarchy.enabled is true, but the dataset has no hierarchy "
                f"column: {self.config.hierarchy.parent_column}"
            )
        return hierarchy

    def _reconcile_recovery_forecast(self) -> None:
        if self.state.hierarchy is None:
            return
        if "recovery" not in self.config.hierarchy.apply_to:
            return
        if self.state.recovery_curve_forecast is None:
            return

        from riseforecast.hierarchy import reconcile_forecasts

        result = reconcile_forecasts(
            self.state.recovery_curve_forecast.values,
            hierarchy=self.state.hierarchy,
            method=self.config.hierarchy.method,
        )
        lower = self.state.recovery_curve_forecast.lower
        upper = self.state.recovery_curve_forecast.upper
        if lower is not None:
            lower = reconcile_forecasts(
                lower,
                hierarchy=self.state.hierarchy,
                method=self.config.hierarchy.method,
            ).values
        if upper is not None:
            upper = reconcile_forecasts(
                upper,
                hierarchy=self.state.hierarchy,
                method=self.config.hierarchy.method,
            ).values
        if lower is not None and upper is not None:
            from riseforecast.intervals import order_interval_bounds

            lower, upper = order_interval_bounds(
                lower,
                upper,
                values=result.values,
            )
        self.state.hierarchy_reconciliation = result
        self.state.recovery_curve_forecast = replace(
            self.state.recovery_curve_forecast,
            values=result.values,
            lower=lower,
            upper=upper,
        )

    def _has_base_validation_period(self) -> bool:
        if self.config.base is None:
            return False
        return (
            self.config.base.validation_start is not None
            and self.config.base.validation_end is not None
        )

    def _estimate_recovery_coefficients(
        self,
        dataset: RecoveryDataset,
    ) -> pd.Series:
        from riseforecast.recovery import RecoveryCoefficientEstimator

        return RecoveryCoefficientEstimator.from_config(
            self.config.recovery
        ).estimate(dataset.metadata())

    def _initial_forecast_for_curve(
        self,
        dataset: RecoveryDataset,
        fallback_name: str,
    ) -> ForecastFrame:
        from riseforecast.data import ForecastFrame

        if self.state.reference_forecast is not None:
            return self.state.reference_forecast
        if self.state.initial_forecast is not None:
            return self.state.initial_forecast.as_forecast_frame()
        try:
            return dataset.forecast_frame(
                kind="reference_forecast",
                name=fallback_name,
            )
        except ValueError:
            return ForecastFrame(values=dataset.reference_forecast(name=fallback_name))

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


def _previous_period(date: pd.Timestamp, frequency: str) -> pd.Timestamp:
    offset = pd.tseries.frequencies.to_offset(frequency)
    return pd.Timestamp(date) - offset


def _validation_actual(
    observed: pd.DataFrame,
    validation_start: pd.Timestamp,
    validation_end: pd.Timestamp,
) -> pd.DataFrame:
    actual = observed.copy()
    actual.index = pd.to_datetime(actual.index)
    actual = actual.sort_index().loc[validation_start:validation_end]
    if actual.empty:
        raise ValueError("Base validation period has no observed target rows.")
    return actual


def _bottom_level_matrix(
    matrix: pd.DataFrame,
    hierarchy: HierarchySpec | None,
) -> pd.DataFrame:
    if hierarchy is None:
        return matrix
    missing = [node for node in hierarchy.bottom_ids if node not in matrix.columns]
    if missing:
        raise ValueError(
            f"Matrix is missing hierarchy bottom nodes: {', '.join(missing)}"
        )
    return matrix.loc[:, list(hierarchy.bottom_ids)]


def _bottom_level_forecast_frame(
    forecast: ForecastFrame,
    hierarchy: HierarchySpec | None,
) -> ForecastFrame:
    if hierarchy is None:
        return forecast

    from riseforecast.data import ForecastFrame

    return ForecastFrame(
        values=_bottom_level_matrix(forecast.values, hierarchy),
        lower=(
            None
            if forecast.lower is None
            else _bottom_level_matrix(forecast.lower, hierarchy)
        ),
        upper=(
            None
            if forecast.upper is None
            else _bottom_level_matrix(forecast.upper, hierarchy)
        ),
    )


def _bottom_level_signals(
    signals: Mapping[str, pd.DataFrame],
    hierarchy: HierarchySpec | None,
) -> dict[str, pd.DataFrame]:
    if hierarchy is None:
        return dict(signals)
    return {
        name: _bottom_level_matrix(signal, hierarchy)
        for name, signal in signals.items()
    }


def _validate_base_validation_config(base_config: object) -> None:
    validation_start = base_config.validation_start
    validation_end = base_config.validation_end
    if (validation_start is None) != (validation_end is None):
        raise ValueError(
            "base.validation_start and base.validation_end must be configured together."
        )
    if validation_start is None or validation_end is None:
        return
    start = pd.Timestamp(validation_start)
    end = pd.Timestamp(validation_end)
    train_end = pd.Timestamp(base_config.train_end)
    if start > end:
        raise ValueError("base.validation_start must be before validation_end.")
    if end > train_end:
        raise ValueError("base.validation_end cannot be after base.train_end.")
