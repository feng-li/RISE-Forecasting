from riseforecast import (
    PipelineConfig,
    RecoveryCurveForecaster,
    RecoveryForecastingPipeline,
    default_model_registry,
)


def test_package_imports() -> None:
    config = PipelineConfig(
        shock_start="2020-01",
        initial_date="2023-06",
        terminal_date="2024-07",
        forecast_start="2023-08",
        forecast_end="2024-07",
    )
    pipeline = RecoveryForecastingPipeline(config)
    forecaster = RecoveryCurveForecaster(
        initial_date="2023-06",
        forecast_start="2023-08",
    )
    assert pipeline.config.initial_date == "2023-06"
    assert forecaster.forecast_start == "2023-08"
    assert "arima" in default_model_registry().names()
