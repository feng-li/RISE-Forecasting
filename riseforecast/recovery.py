"""Recovery coefficient utilities."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal

import numpy as np
import pandas as pd

if TYPE_CHECKING:
    from riseforecast.config import RecoveryCoefficientConfig

RecoveryCoefficientMethod = Literal["direct", "weighted_score", "regression"]


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


@dataclass(frozen=True)
class RecoveryCoefficientRegression:
    """Paper-style regression from weighted recovery score to coefficient."""

    intercept: float
    slope: float
    score_columns: tuple[str, ...]
    weights: dict[str, float]
    score_min: float = 1.0
    score_max: float = 5.0
    min_coefficient: float = 0.0
    max_coefficient: float = 1.0

    def predict(self, scores: pd.DataFrame) -> pd.Series:
        frame = _metadata_by_series_id(scores)
        score = weighted_recovery_score(
            frame,
            columns=self.score_columns,
            weights=self.weights,
            score_min=self.score_min,
            score_max=self.score_max,
        )
        values = self.intercept + self.slope * score
        values = values.clip(self.min_coefficient, self.max_coefficient)
        return values.rename("coefficient")


# Backward-compatible name for callers that think of the score columns as factors.
FactorCoefficientRegression = RecoveryCoefficientRegression


@dataclass(frozen=True)
class RecoveryCoefficientEstimator:
    """Estimate intervention coefficients from direct values or recovery factors."""

    method: RecoveryCoefficientMethod = "direct"
    coefficient_column: str = "coefficient"
    score_columns: tuple[str, ...] = ("policy", "distance", "recovery")
    weights: dict[str, float] | None = None
    anchors: dict[str, float] | None = None
    default_coefficient: float = 1.0
    min_coefficient: float = 0.0
    max_coefficient: float = 1.0
    score_min: float = 1.0
    score_max: float = 5.0
    fit_intercept: bool = True
    preserve_anchors: bool = True

    @classmethod
    def from_config(
        cls,
        config: RecoveryCoefficientConfig,
    ) -> RecoveryCoefficientEstimator:
        """Build an estimator from `PipelineConfig.recovery`."""

        return cls(
            method=config.method,
            coefficient_column=config.coefficient_column,
            score_columns=config.score_columns,
            weights=config.weights,
            anchors=config.anchors,
            default_coefficient=config.default_coefficient,
            min_coefficient=config.min_coefficient,
            max_coefficient=config.max_coefficient,
            score_min=config.score_min,
            score_max=config.score_max,
            fit_intercept=config.fit_intercept,
            preserve_anchors=config.preserve_anchors,
        )

    def estimate(self, metadata: pd.DataFrame) -> pd.Series:
        """Estimate coefficients indexed by `series_id`."""

        frame = _metadata_by_series_id(metadata)
        if self.method == "direct":
            return direct_recovery_coefficients(
                frame,
                coefficient_column=self.coefficient_column,
                min_coefficient=self.min_coefficient,
                max_coefficient=self.max_coefficient,
                default_coefficient=self.default_coefficient,
            )
        if self.method == "weighted_score":
            score = weighted_recovery_score(
                frame,
                columns=self.score_columns,
                weights=self.weights,
                score_min=self.score_min,
                score_max=self.score_max,
            )
            values = self.min_coefficient + score * (
                self.max_coefficient - self.min_coefficient
            )
            return values.rename("coefficient")
        if self.method == "regression":
            calibration = regress_recovery_coefficients(
                frame,
                coefficient_column=self.coefficient_column,
                columns=self.score_columns,
                weights=self.weights,
                anchors=self.anchors,
                score_min=self.score_min,
                score_max=self.score_max,
                min_coefficient=self.min_coefficient,
                max_coefficient=self.max_coefficient,
                fit_intercept=self.fit_intercept,
            )
            coefficients = calibration.predict(frame)
            if self.preserve_anchors and self.anchors:
                anchor_values = _explicit_anchor_coefficients(self.anchors)
                coefficients.update(anchor_values)
            return coefficients.fillna(self.default_coefficient).clip(
                self.min_coefficient,
                self.max_coefficient,
            )
        raise ValueError(f"Unknown recovery coefficient method: {self.method}")


def average_recovery_score(
    scores: pd.DataFrame,
    columns: tuple[str, ...] = ("policy", "distance", "recovery"),
) -> pd.Series:
    """Average structured recovery indicators by entity."""

    missing = [column for column in columns if column not in scores.columns]
    if missing:
        raise ValueError(f"Missing recovery score columns: {', '.join(missing)}")
    return scores.loc[:, list(columns)].mean(axis=1)


def weighted_recovery_score(
    scores: pd.DataFrame,
    columns: tuple[str, ...] = ("policy", "distance", "recovery"),
    weights: dict[str, float] | None = None,
    score_min: float = 1.0,
    score_max: float = 5.0,
) -> pd.Series:
    """Weighted normalized recovery score in [0, 1]."""

    weighted = weighted_factor_matrix(
        scores,
        columns=columns,
        weights=weights,
        score_min=score_min,
        score_max=score_max,
    )
    denominator = sum(_factor_weights(columns, weights).values())
    values = weighted.sum(axis=1) / denominator
    return values.clip(0.0, 1.0).rename("recovery_score")


def weighted_factor_matrix(
    scores: pd.DataFrame,
    columns: tuple[str, ...] = ("policy", "distance", "recovery"),
    weights: dict[str, float] | None = None,
    score_min: float = 1.0,
    score_max: float = 5.0,
) -> pd.DataFrame:
    """Return normalized score columns multiplied by factor weights."""

    normalized = _normalized_score_matrix(
        scores,
        columns=columns,
        score_min=score_min,
        score_max=score_max,
    )
    factor_weights = _factor_weights(columns, weights)
    return normalized.multiply(pd.Series(factor_weights), axis="columns")


def direct_recovery_coefficients(
    metadata: pd.DataFrame,
    coefficient_column: str = "coefficient",
    min_coefficient: float = 0.0,
    max_coefficient: float = 1.0,
    default_coefficient: float | None = None,
) -> pd.Series:
    """Read recovery coefficients directly from metadata."""

    frame = _metadata_by_series_id(metadata)
    if coefficient_column not in frame.columns:
        raise ValueError(f"Missing coefficient column: {coefficient_column}")
    values = frame[coefficient_column].astype(float)
    if default_coefficient is not None:
        values = values.fillna(default_coefficient)
    return values.clip(min_coefficient, max_coefficient).rename("coefficient")


def regress_recovery_coefficients(
    metadata: pd.DataFrame,
    coefficient_column: str = "coefficient",
    columns: tuple[str, ...] = ("policy", "distance", "recovery"),
    weights: dict[str, float] | None = None,
    anchors: dict[str, float] | None = None,
    score_min: float = 1.0,
    score_max: float = 5.0,
    min_coefficient: float = 0.0,
    max_coefficient: float = 1.0,
    fit_intercept: bool = True,
) -> RecoveryCoefficientRegression:
    """Regress coefficients on a weighted recovery score.

    This generalizes the paper's procedure: combine policy, distance, and
    recovery scores into one weighted score, then use least squares to calibrate a
    linear coefficient mapping from anchor coefficients.
    """

    frame = _metadata_by_series_id(metadata)
    score = weighted_recovery_score(
        frame,
        columns=columns,
        weights=weights,
        score_min=score_min,
        score_max=score_max,
    )
    y = _anchor_coefficients(
        frame,
        coefficient_column=coefficient_column,
        anchors=anchors,
    )
    joined = pd.concat(
        [score.rename("score"), y.rename("coefficient")],
        axis=1,
        join="inner",
    ).dropna()
    if len(joined) < 2:
        raise ValueError("At least two recovery coefficient anchors are required.")

    x = joined["score"].to_numpy(dtype=float)
    if fit_intercept:
        design = np.column_stack([np.ones(len(joined)), x])
    else:
        design = x.reshape(-1, 1)
    target = joined["coefficient"].to_numpy(dtype=float)
    fitted, *_ = np.linalg.lstsq(design, target, rcond=None)
    if fit_intercept:
        intercept = float(fitted[0])
        slope = float(fitted[1])
    else:
        intercept = 0.0
        slope = float(fitted[0])
    return RecoveryCoefficientRegression(
        intercept=intercept,
        slope=slope,
        score_columns=columns,
        weights=_factor_weights(columns, weights),
        score_min=score_min,
        score_max=score_max,
        min_coefficient=min_coefficient,
        max_coefficient=max_coefficient,
    )


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


def _metadata_by_series_id(metadata: pd.DataFrame) -> pd.DataFrame:
    frame = metadata.copy()
    if "series_id" in frame.columns:
        frame["series_id"] = frame["series_id"].astype(str)
        frame = frame.set_index("series_id", drop=False)
    if frame.index.has_duplicates:
        duplicates = frame.index[frame.index.duplicated()].unique()
        joined = ", ".join(str(item) for item in duplicates)
        raise ValueError(f"Duplicate series_id values: {joined}")
    return frame


def _normalized_score_matrix(
    scores: pd.DataFrame,
    columns: tuple[str, ...],
    score_min: float,
    score_max: float,
) -> pd.DataFrame:
    missing = [column for column in columns if column not in scores.columns]
    if missing:
        raise ValueError(f"Missing recovery score columns: {', '.join(missing)}")
    if score_max <= score_min:
        raise ValueError("score_max must be greater than score_min.")
    normalized = (
        scores.loc[:, list(columns)].astype(float) - score_min
    ) / (score_max - score_min)
    return normalized.clip(0.0, 1.0)


def _factor_weights(
    columns: tuple[str, ...],
    weights: dict[str, float] | None,
) -> dict[str, float]:
    supplied = (
        {}
        if weights is None
        else {str(key): value for key, value in weights.items()}
    )
    unknown = sorted(set(supplied) - set(columns))
    if unknown:
        raise ValueError(f"Unknown recovery weight columns: {', '.join(unknown)}")

    result = {column: float(supplied.get(column, 1.0)) for column in columns}
    invalid = [
        column
        for column, value in result.items()
        if not np.isfinite(value) or value < 0.0
    ]
    if invalid:
        raise ValueError(
            f"Recovery factor weights must be finite and nonnegative: "
            f"{', '.join(invalid)}"
        )
    if sum(result.values()) <= 0.0:
        raise ValueError("At least one recovery factor weight must be positive.")
    return result


def _anchor_coefficients(
    metadata: pd.DataFrame,
    coefficient_column: str,
    anchors: dict[str, float] | None,
) -> pd.Series:
    if anchors:
        return _explicit_anchor_coefficients(anchors)
    if coefficient_column not in metadata.columns:
        raise ValueError(f"Missing coefficient column: {coefficient_column}")
    return metadata[coefficient_column].astype(float).rename("coefficient")


def _explicit_anchor_coefficients(anchors: dict[str, float]) -> pd.Series:
    return pd.Series(
        {str(key): float(value) for key, value in anchors.items()},
        name="coefficient",
        dtype=float,
    )
