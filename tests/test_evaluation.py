import numpy as np
import pandas as pd

from riseforecast.evaluation import evaluate_forecast_matrix, forecast_matrix_to_long


def test_forecast_matrix_to_long_aligns_forecast_and_actual() -> None:
    actual = pd.DataFrame(
        {"a": [10.0, 20.0], "b": [30.0, 40.0]},
        index=pd.date_range("2024-01-01", periods=2, freq="MS"),
    )
    forecast = pd.DataFrame(
        {"a": [11.0], "b": [39.0]},
        index=pd.to_datetime(["2024-02-01"]),
    )

    frame = forecast_matrix_to_long(actual, forecast, "candidate")

    assert frame.shape == (2, 4)
    assert set(frame["unique_id"]) == {"a", "b"}
    assert frame["y"].tolist() == [20.0, 40.0]
    assert frame["candidate"].tolist() == [11.0, 39.0]


def test_evaluate_forecast_matrix_uses_utilsforecast_metrics() -> None:
    index = pd.date_range("2024-03-01", periods=2, freq="MS")
    actual = pd.DataFrame({"a": [10.0, 20.0]}, index=index)
    forecast = pd.DataFrame({"a": [9.0, 22.0]}, index=index)
    train = pd.DataFrame(
        {"a": [4.0, 6.0, 8.0]},
        index=pd.date_range("2023-12-01", periods=3, freq="MS"),
    )

    report = evaluate_forecast_matrix(
        actual=actual,
        forecast=forecast,
        train=train,
        model_name="candidate",
        seasonality=1,
        metrics=("mae", "mase"),
    )

    overall = report.loc[report["level"] == "overall"].set_index("metric")
    assert np.isclose(overall.loc["mae", "value"], 1.5)
    assert np.isclose(overall.loc["mase", "value"], 0.75)
