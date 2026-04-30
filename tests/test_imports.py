from riseforecast import PipelineConfig, RecoveryForecastingPipeline


def test_package_imports() -> None:
    config = PipelineConfig(
        shock_start="2020-01",
        initial_date="2023-06",
        terminal_date="2024-07",
        forecast_start="2023-08",
        forecast_end="2024-07",
    )
    pipeline = RecoveryForecastingPipeline(config)
    assert pipeline.config.initial_date == "2023-06"
