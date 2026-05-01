"""Plotly visualization helpers for recovery forecasts."""

from __future__ import annotations

from collections.abc import Sequence
from typing import TYPE_CHECKING

import pandas as pd

from riseforecast.data import ForecastFrame

if TYPE_CHECKING:
    from riseforecast.curves import RecoveryCurveForecast


DEFAULT_COLORS = (
    "#2563eb",
    "#dc2626",
    "#16a34a",
    "#9333ea",
    "#ea580c",
    "#0891b2",
    "#be123c",
    "#4f46e5",
)


def plot_forecast(
    forecast: ForecastFrame | pd.DataFrame,
    observed: pd.DataFrame | None = None,
    entities: Sequence[str] | None = None,
    title: str = "Recovery forecast",
    value_name: str = "Value",
    show_interval: bool = True,
):
    """Plot forecast paths, optional observed history, and optional intervals."""

    go = _plotly_go()
    forecast_frame = _coerce_forecast_frame(forecast)
    values = _select_entities(_prepare_matrix(forecast_frame.values), entities)
    observed_values = (
        None
        if observed is None
        else _select_entities(_prepare_matrix(observed), tuple(values.columns))
    )
    lower = (
        None
        if forecast_frame.lower is None
        else _select_entities(
            _prepare_matrix(forecast_frame.lower),
            tuple(values.columns),
        )
    )
    upper = (
        None
        if forecast_frame.upper is None
        else _select_entities(
            _prepare_matrix(forecast_frame.upper),
            tuple(values.columns),
        )
    )

    fig = go.Figure()
    for index, entity in enumerate(values.columns):
        color = DEFAULT_COLORS[index % len(DEFAULT_COLORS)]
        group = str(entity)

        if observed_values is not None:
            fig.add_trace(
                go.Scatter(
                    x=observed_values.index,
                    y=observed_values[entity],
                    mode="lines",
                    name=f"{entity} observed",
                    legendgroup=group,
                    line={"color": color, "dash": "dot"},
                )
            )

        if show_interval and lower is not None and upper is not None:
            fig.add_trace(
                go.Scatter(
                    x=lower.index,
                    y=lower[entity],
                    mode="lines",
                    name=f"{entity} lower",
                    legendgroup=group,
                    showlegend=False,
                    hoverinfo="skip",
                    line={"color": _rgba(color, 0.0), "width": 0},
                )
            )
            fig.add_trace(
                go.Scatter(
                    x=upper.index,
                    y=upper[entity],
                    mode="lines",
                    name=f"{entity} interval",
                    legendgroup=group,
                    fill="tonexty",
                    fillcolor=_rgba(color, 0.14),
                    hoverinfo="skip",
                    line={"color": _rgba(color, 0.0), "width": 0},
                )
            )

        fig.add_trace(
            go.Scatter(
                x=values.index,
                y=values[entity],
                mode="lines+markers",
                name=f"{entity} forecast",
                legendgroup=group,
                line={"color": color},
            )
        )

    fig.update_layout(
        title=title,
        template="plotly_white",
        hovermode="x unified",
        xaxis_title="Date",
        yaxis_title=value_name,
        legend_title_text="Series",
        margin={"l": 56, "r": 24, "t": 64, "b": 48},
    )
    return fig


def plot_recovery_curve(
    forecast: RecoveryCurveForecast,
    observed: pd.DataFrame | None = None,
    entities: Sequence[str] | None = None,
    title: str = "Recovery curve forecast",
    value_name: str = "Value",
    include_component_curves: bool = True,
):
    """Plot the final recovery forecast and optional curve components."""

    go = _plotly_go()
    fig = plot_forecast(
        ForecastFrame(values=forecast.values),
        observed=observed,
        entities=entities,
        title=title,
        value_name=value_name,
        show_interval=False,
    )
    values = _select_entities(_prepare_matrix(forecast.values), entities)

    if include_component_curves:
        for curve_name, component in forecast.components.items():
            component_frame = _select_entities(
                _prepare_matrix(component),
                tuple(values.columns),
            )
            for index, entity in enumerate(values.columns):
                color = DEFAULT_COLORS[index % len(DEFAULT_COLORS)]
                fig.add_trace(
                    go.Scatter(
                        x=component_frame.index,
                        y=component_frame[entity],
                        mode="lines",
                        name=f"{entity} {curve_name}",
                        legendgroup=str(entity),
                        opacity=0.45,
                        line={"color": color, "dash": "dash"},
                    )
                )

    fig.add_vline(
        x=forecast.initial_date,
        line_color="#64748b",
        line_dash="dot",
        opacity=0.8,
    )
    fig.add_vline(
        x=forecast.terminal_date,
        line_color="#64748b",
        line_dash="dot",
        opacity=0.8,
    )
    return fig


def _plotly_go():
    try:
        import plotly.graph_objects as go
    except ImportError as exc:
        raise ImportError(
            "Plotly visualizations require the optional dependency `plotly`. "
            "Install it with `pip install -e .[plot]` or `pip install plotly`."
        ) from exc
    return go


def _coerce_forecast_frame(forecast: ForecastFrame | pd.DataFrame) -> ForecastFrame:
    if isinstance(forecast, ForecastFrame):
        return forecast
    return ForecastFrame(values=forecast)


def _prepare_matrix(matrix: pd.DataFrame) -> pd.DataFrame:
    result = matrix.copy()
    result.index = pd.to_datetime(result.index)
    return result.sort_index().astype(float)


def _select_entities(
    matrix: pd.DataFrame,
    entities: Sequence[str] | None,
) -> pd.DataFrame:
    if entities is None:
        return matrix
    missing = [entity for entity in entities if entity not in matrix.columns]
    if missing:
        raise ValueError(f"Missing plot entities: {', '.join(map(str, missing))}")
    return matrix.loc[:, list(entities)]


def _rgba(hex_color: str, alpha: float) -> str:
    color = hex_color.lstrip("#")
    red = int(color[0:2], 16)
    green = int(color[2:4], 16)
    blue = int(color[4:6], 16)
    return f"rgba({red}, {green}, {blue}, {alpha})"
