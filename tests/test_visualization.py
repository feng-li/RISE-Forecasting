import numpy as np
import pandas as pd
import pytest

from riseforecast.curves import recovery_curve_forecast
from riseforecast.data import ForecastFrame
from riseforecast.visualization import plot_forecast, plot_recovery_curve


def test_plot_forecast_returns_plotly_figure() -> None:
    go = pytest.importorskip("plotly.graph_objects")
    index = pd.date_range("2024-03-01", periods=2, freq="MS")
    forecast = ForecastFrame(
        values=pd.DataFrame({"series_a": [120.0, 140.0]}, index=index),
        lower=pd.DataFrame({"series_a": [100.0, 120.0]}, index=index),
        upper=pd.DataFrame({"series_a": [140.0, 160.0]}, index=index),
    )
    observed = pd.DataFrame(
        {"series_a": [90.0, 100.0]},
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )

    figure = plot_forecast(forecast, observed=observed, entities=("series_a",))

    assert isinstance(figure, go.Figure)
    assert [trace.name for trace in figure.data] == [
        "series_a observed",
        "series_a lower",
        "series_a interval",
        "series_a forecast",
    ]


def test_plot_recovery_curve_includes_component_curves() -> None:
    go = pytest.importorskip("plotly.graph_objects")
    result = recovery_curve_forecast(
        initial_forecast=pd.Series({"series_a": 100.0}),
        terminal_forecast=pd.Series({"series_a": 160.0}),
        initial_date="2024-01",
        forecast_start="2024-02",
        forecast_end="2024-04",
        curve_names=("linear", "logistic"),
    )

    figure = plot_recovery_curve(result, entities=("series_a",))
    trace_names = [trace.name for trace in figure.data]

    assert isinstance(figure, go.Figure)
    assert "series_a forecast" in trace_names
    assert "series_a linear" in trace_names
    assert "series_a logistic" in trace_names
    assert len(figure.layout.shapes) == 2
    assert np.isfinite(figure.data[0].y).all()


def test_plot_recovery_curve_includes_interval_band() -> None:
    go = pytest.importorskip("plotly.graph_objects")
    index = pd.to_datetime(["2024-01-01"])
    initial = ForecastFrame(
        values=pd.DataFrame({"series_a": [100.0]}, index=index),
        lower=pd.DataFrame({"series_a": [80.0]}, index=index),
        upper=pd.DataFrame({"series_a": [120.0]}, index=index),
    )
    result = recovery_curve_forecast(
        initial_forecast=initial,
        terminal_forecast=pd.Series({"series_a": 160.0}),
        initial_date="2024-01",
        forecast_start="2024-02",
        forecast_end="2024-04",
        curve_names=("linear",),
    )

    figure = plot_recovery_curve(
        result,
        entities=("series_a",),
        include_component_curves=False,
        interval_opacity=0.25,
    )

    assert isinstance(figure, go.Figure)
    assert [trace.name for trace in figure.data] == [
        "series_a lower",
        "series_a interval",
        "series_a forecast",
    ]
    assert figure.data[1].fill == "tonexty"
    assert figure.data[1].fillcolor == "rgba(37, 99, 235, 0.25)"


def test_plot_forecast_labels_interval_level() -> None:
    go = pytest.importorskip("plotly.graph_objects")
    index = pd.date_range("2024-03-01", periods=2, freq="MS")
    forecast = ForecastFrame(
        values=pd.DataFrame({"series_a": [120.0, 140.0]}, index=index),
        lower=pd.DataFrame({"series_a": [100.0, 120.0]}, index=index),
        upper=pd.DataFrame({"series_a": [140.0, 160.0]}, index=index),
    )

    figure = plot_forecast(
        forecast,
        entities=("series_a",),
        interval_level=0.8,
    )

    assert isinstance(figure, go.Figure)
    assert figure.data[1].name == "series_a 80% interval"


def test_plot_recovery_curve_uses_custom_interval_label() -> None:
    go = pytest.importorskip("plotly.graph_objects")
    index = pd.to_datetime(["2024-01-01"])
    initial = ForecastFrame(
        values=pd.DataFrame({"series_a": [100.0]}, index=index),
        lower=pd.DataFrame({"series_a": [80.0]}, index=index),
        upper=pd.DataFrame({"series_a": [120.0]}, index=index),
    )
    result = recovery_curve_forecast(
        initial_forecast=initial,
        terminal_forecast=pd.Series({"series_a": 160.0}),
        initial_date="2024-01",
        forecast_start="2024-02",
        forecast_end="2024-04",
        curve_names=("linear",),
    )

    figure = plot_recovery_curve(
        result,
        entities=("series_a",),
        include_component_curves=False,
        interval_label="95% PI",
    )

    assert isinstance(figure, go.Figure)
    assert figure.data[1].name == "series_a 95% PI"
