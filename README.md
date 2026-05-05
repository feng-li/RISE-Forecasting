# `riseforecast`: General Recovery Forecasting with RISE

This Python package  `riseforecast` forecast how a quantity of interest recovers after a shock. It implements the **RISE** (Recovery-Informed Strategy Enhancement) framework as a reusable recovery
forecasting module.

The package is entity-neutral: a series can be a country, route, product, store,
market, sector, or any other time series whose post-shock recovery needs to be
forecast. 

That original RISE application was awarded **1st place in point forecasting** and
**3rd place in interval forecasting** in the **Tourism Forecasting Competition II
(2023)**. The tourism competition workflow in the package is kept as an example application.

## Repository Status

The reusable package is developed in:

```text
riseforecast/
```

The tourism competition example, including converted data and example runners, is
kept in:

```text
examples/tourism_competition/
```

The exact paper implementation is now scoped to that example and preserved in:

```text
examples/tourism_competition/legacypapercode/
```

Use `examples/tourism_competition/legacypapercode/` if you need to check,
reproduce, or compare against the implementation used for the paper. Use
`riseforecast/` for the general recovery forecasting package.

Because the current package replaces parts of the original R/notebook workflow with
standard Python package APIs, package-native forecasts can differ from the legacy
paper outputs. Small numerical differences can come from library implementations,
optimizer behavior, missing-value handling, date alignment, and floating point
rounding. Other differences are also possible where the Python package uses
generalized model choices instead of the paper's tourism-specific scripts.

Use the package-native diagnostics for the recovery forecasting workflow. Use the legacy
folder only when you need exact paper reproduction or a direct legacy audit.

## Methodology

RISE decomposes a post-shock recovery forecast into three components:

1. **Base forecast**
   Estimate the no-shock counterfactual trajectory from pre-shock historical data.

2. **Reference forecast**
   Estimate the short-term recovery state using timely external indicators, such as
   search indices or flight capacity.

3. **Recovery curve forecast**
   Connect the initial recovery state and the terminal recovery state using
   recovery curves such as linear, quadratic, and logistic paths.

The key idea is to avoid directly extrapolating a structurally broken time series.
Instead, RISE anchors the forecast between a near-term recovery estimate and a
longer-run intervention-adjusted terminal estimate.

## Package Overview

The Python package is named `riseforecast`.

Current package skeleton:

```text
riseforecast/
  base_models.py
  config.py
  data.py
  initial.py
  intervention.py
  recovery.py
  curves.py
  ensembles.py
  reference.py
  hierarchy.py
  metrics.py
  pipeline.py
```

Implemented stages now include model-based initial forecasts, external-signal
reference forecasts, intervention-adjusted terminal forecasts, and recovery curve
forecasts:

```python
from riseforecast import (
    InitialForecaster,
    RecoveryCurveForecaster,
    intervention_terminal_forecast,
)

initial = InitialForecaster(
    initial_date="2023-06",
    train_end="2023-01",
    models=("seasonal_naive", "random_walk_drift", "arima", "ets"),
).forecast(observed_df)

terminal = intervention_terminal_forecast(
    base_forecast=baseline_df,
    coefficients=recovery_coefficients,
    terminal_date="2024-07",
)

forecast = RecoveryCurveForecaster(
    initial_date="2023-06",
    forecast_start="2023-08",
).forecast(
    initial_forecast=initial.values,
    terminal_forecast=terminal,
)
```

The same workflow can start from the compact data format:

```python
from riseforecast import RecoveryDataset, RecoveryForecastingPipeline

dataset = RecoveryDataset.from_directory("examples/tourism_competition/data")

observed = dataset.observed_target()
baseline = dataset.base_forecast()
reference = dataset.reference_forecast()
coefficients = dataset.coefficients()

forecast = (
    RecoveryForecastingPipeline.from_dataset(dataset)
    .fit_dataset(dataset)
    .predict()
)
```

Base forecasts can now be generated from observed data:

```python
from riseforecast import forecast_panel

base_forecasts = forecast_panel(
    observed=dataset.observed_target(),
    models=("seasonal_naive", "random_walk_drift", "arima", "ets", "holt"),
    horizon=24,
    train_end="2019-12",
)

baseline = base_forecasts["holt"].values
```

Before fitting package-native base models, internal missing values are imputed with
a structural state-space Kalman smoother. Observed values are preserved, and the
imputer falls back to deterministic interpolation when a series is too short for a
stable state-space fit.

The pipeline can also reproduce the paper's validation-driven base combination
logic. When `base.validation_start` and `base.validation_end` are configured, each
candidate model is trained before the validation window, scored on that window,
the best `selection_fraction` of models is kept, and the selected models are
refit through `base.train_end` before combination. Supported base ensembles are
`mean`, `error_weighted`, `ridge`, and `lasso`.

Initial forecasts can also be generated from observed data with the same model
registry. This creates the near-term path ending at `initial_date`; the final row is
the initial anchor for the recovery curve:

```python
from riseforecast import InitialForecaster

initial = InitialForecaster(
    initial_date="2023-06",
    train_end="2023-01",
    models=("seasonal_naive", "random_walk_drift", "arima", "ets"),
).forecast(dataset.observed_target())

initial_anchor = initial.initial
```

Reference forecasts estimate the near-term recovery state from arbitrary
exogenous variables `X`. In the tourism competition, Baidu search and flights are
just two tourism-specific X variables. Reference cases are configured as named X
cases:

```python
from riseforecast import ReferenceForecaster, ReferenceXSpec

reference = ReferenceForecaster(
    start="2023-01",
    end="2023-06",
    train_end="2023-01",
    specs=(
        ReferenceXSpec("search_index", method="arimax", name="search_arimax", signal_lag=1),
        ReferenceXSpec("search_index", method="prophet", name="search_prophet", signal_lag=1),
        ReferenceXSpec("search_index", method="ratio", name="search_ratio", signal_lag=1),
        ReferenceXSpec("flight_capacity", method="growth_rate", name="flight_growth"),
    ),
).forecast(
    observed=dataset.observed_target(),
    signals=dataset.exogenous_variables(),
)
```

The `prophet` reference method uses Prophet with the configured X variables as
regressors. Prophet is included in the core package dependencies.

Recovery coefficients can now be estimated from general metadata rather than
hard-coded destination values. The package supports direct coefficients, a
weighted score, and a paper-style regression that first combines the configured
factors, usually `policy`, `distance`, and `recovery`, into a weighted recovery
score and then calibrates a least-squares mapping from anchor coefficients:

```python
from riseforecast import RecoveryCoefficientEstimator

coefficients = RecoveryCoefficientEstimator(
    method="regression",
    score_columns=("policy", "distance", "recovery"),
    weights={"policy": 1.0, "distance": 1.0, "recovery": 1.0},
    anchors={"canada": 0.65, "mexico": 1.0, "hong_kong": 0.85},
).estimate(dataset.metadata())
```

Implemented base model names include:

```text
seasonal_naive
random_walk_drift
arima
ets
holt
holt_winters
stl_arima
stl_ets
tbats
nnetar
```

When an explicit hierarchy is configured, Stage 1 can also include hierarchical
base forecast candidates in `base.models`. These candidates are validated and
ensembled like ordinary base models, but return bottom-level forecasts for the
terminal and recovery-curve stages. Supported names include:

```text
top_down_arima
top_down_ets
wls_struct
mint_shrink
```

Names may also combine a reconciliation method with a registered base model, for
example `wls_struct_ets` or `mint_shrink_random_walk_drift`. Method-only names
such as `wls_struct` and `mint_shrink` use ARIMA as the default base model.

The paper base models now use direct StatsForecast implementations where
available, including `AutoARIMA`, `AutoETS`, `Holt`, `HoltWinters`,
`SeasonalNaive`, `RandomWalkWithDrift`, `MSTL`, and `AutoTBATS`. The `nnetar`
wrapper remains a scikit-learn autoregression approximation.

This applies:

```text
terminal forecast = no-shock baseline forecast at terminal date * intervention coefficient
```

For the recovery curve stage, the package follows the paper's
seasonal-trend factorization. Historical pre-shock observations are decomposed with
STL on the log scale to estimate month-of-year seasonal multipliers, with the base
forecast as a fallback when historical seasonality is unavailable. The linear curve
links the initial and terminal trend anchors. The quadratic curve is fitted to
de-seasonalized trend history plus a weighted terminal trend point. The logistic curve
is fitted to critical trend points, including the initial point and configured future
base-forecast anchors. The full forecasts are recovered on the original scale as:

```text
full forecast = recovery curve trend component * seasonal component
```

The fitted `RecoveryCurveForecast` stores `recovery_curve` / `trend_values`,
`seasonal_components`, and the recovered original-scale `values`.

Plotly visualization helpers are available for forecast matrices and recovery
curve outputs:

```python
from riseforecast import plot_forecast, plot_recovery_curve

fig = plot_forecast(
    forecast,
    observed=dataset.observed_target(),
    entities=("canada", "mexico"),
    show_interval=True,
    interval_level=0.8,
)

curve_fig = plot_recovery_curve(
    pipeline.state.recovery_curve_forecast,
    observed=dataset.observed_target(),
    entities=("canada",),
    show_interval=True,
    interval_level=1 - pipeline.config.interval.alpha,
)
```

When a `ForecastFrame` or `RecoveryCurveForecast` has `lower` and `upper`
matrices, the helpers draw a shaded interval band around the point forecast.
Use `show_interval=False` to hide it or `interval_opacity=` to adjust the band.
Use `interval_level=0.8` for an `80% interval` legend label, or
`interval_label="95% PI"` when the level comes from external forecast bounds.

Plotly is an optional dependency; install it with `pip install -e .[plot]` when
using these helpers.

To run the current tests:

```bash
source ~/.virtualenvs/py3.12-forecasting/bin/activate
python -m pytest
```

The tourism competition artifacts have also been converted to the compact
three-file recovery forecasting format:

```text
examples/tourism_competition/data/series.csv
examples/tourism_competition/data/panel.csv
examples/tourism_competition/data/config.yaml
```

Regenerate them with:

```bash
python examples/tourism_competition/convert_legacy_data.py
```

Run migrated examples against the converted data:

```bash
python examples/tourism_competition/run_forecast.py
python examples/tourism_competition/terminal_forecast.py
python examples/tourism_competition/initial_forecast.py
python examples/tourism_competition/reference_forecast.py
python examples/tourism_competition/recovery_curve_forecast.py
python examples/tourism_competition/evaluate_migration.py
```

The migration evaluation script uses `utilsforecast.evaluation.evaluate` and
`utilsforecast.losses` for MAE, RMSE, MAPE, SMAPE, bias, MASE, and RMSSE checks.

## Data Format

The compact data format uses three files: `series.csv`, `panel.csv`, and
`config.yaml`. See [DATA.md](DATA.md) for the full schema, configuration
reference, hierarchy conventions, interval fields, and tourism competition
mapping.

## Tourism Paper Legacy Implementation

For reproducing or checking the paper's original implementation, use the legacy
workflow under:

```text
examples/tourism_competition/legacypapercode/
```

The legacy workflow is a mixture of R scripts, Python notebooks, Excel files, and
generated artifacts. The original run order is documented in:

```text
examples/tourism_competition/legacypapercode/readme.txt
```

In short, the legacy implementation runs:

```text
retrieve_baidu_index.ipynb
generate_composite_search_index.ipynb
estimate_reference_series_via_search_data.R
estimate_reference_series_via_flight_data.ipynb
baseline_forecast_method_2018_1.R
baseline_forecast_method_2018_2.R
baseline_forecast_method_2020_1.R
baseline_forecast_method_2020_2.R
baseline_forecast_hierarchical.R
baseline_forecast_ensemble.ipynb
make_adjustment_forecasts.ipynb
```

The paper itself is available at:

```text
docs/paper.pdf
```

## Citation

If you use this code or build upon it, please cite:

> Feng Li & Taozhu Ruan (2026). RISE: Recovery-Informed Forecasting Strategy
> Enhancement. *Annals of Tourism Research*.
> [DOI](https://doi.org/10.1016/j.annals.2026.104164)
> [Preprint](https://arxiv.org/abs/2603.01085)
