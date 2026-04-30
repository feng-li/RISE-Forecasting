"""Recovery curve construction."""

from __future__ import annotations

from dataclasses import dataclass

import numpy as np
import pandas as pd


@dataclass(frozen=True)
class CurveAnchors:
    """Initial and terminal anchors for a recovery path."""

    initial: float
    terminal: float
    periods: int

    def __post_init__(self) -> None:
        if self.periods < 2:
            raise ValueError("periods must be at least 2.")


def linear_curve(anchors: CurveAnchors) -> np.ndarray:
    """Straight-line recovery path including both anchors."""

    return np.linspace(anchors.initial, anchors.terminal, anchors.periods)


def quadratic_curve(
    anchors: CurveAnchors,
    history: pd.Series | None = None,
    terminal_weight: float = 18.0,
) -> np.ndarray:
    """Quadratic recovery path.

    If history is supplied, fit the curve to historical trend values and a weighted
    terminal anchor. Otherwise return a convex quadratic path between anchors.
    """

    if history is None or history.dropna().empty:
        x = np.linspace(0.0, 1.0, anchors.periods)
        return anchors.initial + (anchors.terminal - anchors.initial) * x**2

    y = history.dropna().to_numpy(dtype=float)
    x = np.arange(len(y), dtype=float)
    terminal_x = float(len(y) + anchors.periods - 1)
    x_fit = np.concatenate([x, np.repeat(terminal_x, int(terminal_weight))])
    y_fit = np.concatenate([y, np.repeat(anchors.terminal, int(terminal_weight))])
    coefficients = np.polyfit(x_fit, y_fit, deg=2)
    forecast_x = np.arange(len(y), len(y) + anchors.periods, dtype=float)
    curve = np.polyval(coefficients, forecast_x)
    curve[0] = anchors.initial
    return curve


def logistic_curve(
    anchors: CurveAnchors,
    midpoint: float | None = None,
    growth_rate: float = 0.8,
) -> np.ndarray:
    """Smooth S-shaped recovery path including both anchors."""

    if growth_rate <= 0:
        raise ValueError("growth_rate must be positive.")
    x = np.arange(anchors.periods, dtype=float)
    midpoint = (anchors.periods - 1) / 2 if midpoint is None else midpoint
    raw = 1 / (1 + np.exp(-growth_rate * (x - midpoint)))
    scaled = (raw - raw[0]) / (raw[-1] - raw[0])
    return anchors.initial + (anchors.terminal - anchors.initial) * scaled


def average_curves(curves: list[np.ndarray]) -> np.ndarray:
    """Average multiple recovery paths."""

    if not curves:
        raise ValueError("At least one curve is required.")
    lengths = {len(curve) for curve in curves}
    if len(lengths) != 1:
        raise ValueError("All curves must have the same length.")
    return np.vstack(curves).mean(axis=0)


def build_recovery_curve(
    initial: float,
    terminal: float,
    periods: int,
    curve_names: tuple[str, ...] = ("linear", "quadratic", "logistic"),
) -> np.ndarray:
    """Build and average named recovery curves."""

    anchors = CurveAnchors(initial=initial, terminal=terminal, periods=periods)
    built: list[np.ndarray] = []
    for name in curve_names:
        if name == "linear":
            built.append(linear_curve(anchors))
        elif name == "quadratic":
            built.append(quadratic_curve(anchors))
        elif name == "logistic":
            built.append(logistic_curve(anchors))
        else:
            raise ValueError(f"Unknown curve name: {name}")
    return average_curves(built)
