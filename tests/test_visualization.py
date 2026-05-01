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
