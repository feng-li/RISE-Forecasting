import pandas as pd
import pytest

from riseforecast.data import ForecastFrame
from riseforecast.intervention import (
    InterventionTerminalForecaster,
    align_coefficients,
    intervention_terminal_forecast,
    select_forecast_date,
)


def test_intervention_terminal_forecast_multiplies_terminal_row() -> None:
    baseline = pd.DataFrame(
        {
            "Canada": [100.0, 120.0],
            "Mexico": [200.0, 240.0],
        },
        index=pd.to_datetime(["2024-06-01", "2024-07-01"]),
    )
    coefficients = pd.Series({"Canada": 0.65, "Mexico": 1.0})

    result = intervention_terminal_forecast(
        baseline,
        coefficients,
        terminal_date="2024-07",
    )

    assert result.values.loc["Canada"] == 78.0
    assert result.values.loc["Mexico"] == 240.0
    assert result.baseline.loc["Canada"] == 120.0


def test_intervention_terminal_forecast_adjusts_intervals() -> None:
    frame = ForecastFrame(
        values=pd.DataFrame(
            {"Canada": [120.0], "Mexico": [240.0]},
            index=pd.to_datetime(["2024-07-01"]),
        ),
        lower=pd.DataFrame(
            {"Canada": [100.0], "Mexico": [200.0]},
            index=pd.to_datetime(["2024-07-01"]),
        ),
        upper=pd.DataFrame(
            {"Canada": [140.0], "Mexico": [260.0]},
            index=pd.to_datetime(["2024-07-01"]),
        ),
    )
    coefficients = pd.Series({"Canada": 0.5, "Mexico": 0.8})

    result = InterventionTerminalForecaster("2024-07").forecast(
        frame,
        coefficients,
    )

    assert result.lower is not None
    assert result.upper is not None
    assert result.lower.loc["Canada"] == 50.0
    assert result.upper.loc["Mexico"] == 208.0


def test_select_forecast_date_errors_when_date_missing() -> None:
    baseline = pd.DataFrame(
        {"Canada": [100.0]},
        index=pd.to_datetime(["2024-06-01"]),
    )

    with pytest.raises(ValueError, match="Forecast date"):
        select_forecast_date(baseline, "2024-07")


def test_align_coefficients_requires_all_entities() -> None:
    coefficients = pd.Series({"Canada": 0.65})

    with pytest.raises(ValueError, match="Missing coefficients"):
        align_coefficients(coefficients, pd.Index(["Canada", "Mexico"]))


def test_align_coefficients_clips_by_default() -> None:
    coefficients = pd.Series({"Canada": -0.1, "Mexico": 1.2})

    aligned = align_coefficients(coefficients, pd.Index(["Canada", "Mexico"]))

    assert aligned.loc["Canada"] == 0.0
    assert aligned.loc["Mexico"] == 1.0
