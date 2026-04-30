"""Recovery coefficient utilities."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd


@dataclass(frozen=True)
class LinearCoefficientCalibration:
    """Linear mapping from average recovery score to coefficient."""

    intercept: float
    slope: float
    min_coefficient: float = 0.0
    max_coefficient: float = 1.0

    def predict(self, scores: pd.Series | np.ndarray) -> np.ndarray:
        values = self.intercept + self.slope * np.asarray(scores, dtype=float)
        return np.clip(values, self.min_coefficient, self.max_coefficient)


def average_recovery_score(
    scores: pd.DataFrame,
    columns: tuple[str, ...] = ("policy", "distance", "recovery"),
) -> pd.Series:
    """Average structured recovery indicators by entity."""

    missing = [column for column in columns if column not in scores.columns]
    if missing:
        raise ValueError(f"Missing recovery score columns: {', '.join(missing)}")
    return scores.loc[:, list(columns)].mean(axis=1)


def calibrate_linear_coefficients(
    average_scores: pd.Series,
    anchor_coefficients: pd.Series,
    min_coefficient: float = 0.0,
    max_coefficient: float = 1.0,
) -> LinearCoefficientCalibration:
    """Fit coefficient = intercept + slope * average_score from anchors."""

    joined = pd.concat(
        [
            average_scores.rename("score"),
            anchor_coefficients.rename("coefficient"),
        ],
        axis=1,
        join="inner",
    ).dropna()
    if len(joined) < 2:
        raise ValueError("At least two anchor coefficients are required.")

    x = joined["score"].to_numpy(dtype=float)
    y = joined["coefficient"].to_numpy(dtype=float)
    slope, intercept = np.polyfit(x, y, deg=1)
    return LinearCoefficientCalibration(
        intercept=float(intercept),
        slope=float(slope),
        min_coefficient=min_coefficient,
        max_coefficient=max_coefficient,
    )


def apply_recovery_coefficients(
    base_forecast: pd.Series | pd.DataFrame,
    coefficients: pd.Series,
) -> pd.Series | pd.DataFrame:
    """Shrink a no-shock baseline by entity-specific recovery coefficients."""

    if isinstance(base_forecast, pd.DataFrame):
        missing = [
            entity
            for entity in base_forecast.columns
            if entity not in coefficients.index
        ]
    else:
        missing = []
    if missing:
        raise ValueError(f"Missing coefficients for entities: {', '.join(missing)}")
    axis = "columns" if isinstance(base_forecast, pd.DataFrame) else "index"
    return base_forecast.multiply(coefficients, axis=axis)
