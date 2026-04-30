"""Interfaces for counterfactual base forecasting models."""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass

import pandas as pd


@dataclass(frozen=True)
class BaseForecast:
    """Counterfactual no-shock forecast values."""

    values: pd.DataFrame
    model_name: str
    lower: pd.DataFrame | None = None
    upper: pd.DataFrame | None = None


class BaseForecaster(ABC):
    """Minimal interface for baseline model wrappers."""

    name: str

    @abstractmethod
    def fit(self, y: pd.Series) -> BaseForecaster:
        """Fit a forecaster to one entity series."""

    @abstractmethod
    def predict(self, horizon: int, frequency: str = "MS") -> pd.Series:
        """Forecast a fitted entity series."""


class ModelRegistry:
    """Registry for base forecasting model factories."""

    def __init__(self) -> None:
        self._factories: dict[str, type[BaseForecaster]] = {}

    def register(self, name: str, factory: type[BaseForecaster]) -> None:
        self._factories[name] = factory

    def create(self, name: str) -> BaseForecaster:
        if name not in self._factories:
            raise KeyError(f"Unknown base forecaster: {name}")
        return self._factories[name]()

    def names(self) -> tuple[str, ...]:
        return tuple(sorted(self._factories))
