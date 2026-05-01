# RISE: Recovery-Informed Forecasting Strategy Enhancement

This repository contains the code for **RISE** (Recovery-Informed Strategy
Enhancement), a three-stage forecasting framework developed for forecasting the
recovery of **Chinese outbound tourism** after COVID-19.

The framework was awarded **1st place in point forecasting** and **3rd place in
interval forecasting** in the **Tourism Forecasting Competition II (2023)**.

## Repository Status

This repository is currently in a **migration process**.

The original paper implementation is preserved in:

```text
legacypapercode/
```

The new Python package is being developed in:

```text
riseforecast/
```

Use the **legacy implementation** if you want to check, reproduce, or compare
against the implementation used for the paper. The Python package is the ongoing
migration toward a standard, reusable recovery forecasting library.

Because the migration replaces parts of the original R/notebook workflow with
standard Python package APIs, package-native forecasts can differ from the legacy
paper outputs. Small numerical differences can come from library implementations,
optimizer behavior, missing-value handling, date alignment, and floating point
rounding. Larger differences are also expected where the Python package uses
generalized model choices instead of the paper's tourism-specific scripts. Use
`legacypapercode/` as the source of truth for exact paper reproduction, and use
`examples/tourism_competition/validate_reproduction.py` to quantify stage-by-stage
differences.

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

## Python Migration

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
regressors. Prophet remains an optional dependency; install it with
`pip install -e .[prophet]` when that reference case is used.

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

The migration uses a compact, general recovery forecasting data structure with
three files:

```text
series.csv
panel.csv
config.yaml
```

The format is entity-neutral. A `series_id` can be a country, product, store,
route, market, sector, or any other quantity of interest recovering after a
shock.

### `series.csv`

`series.csv` stores one row per quantity of interest. It contains static
metadata, grouping information, recovery scores, and optional intervention
coefficients.

Example columns:

```text
series_id,series_name,target_name,unit,parent_id,group,subgroup,policy,distance,recovery,coefficient
canada,Canada,outbound_tourists,count,america,America,,3,1,2,0.70
```

`parent_id` is optional. When present and enabled in `config.yaml`, it describes
an explicit hierarchy as an edge list. Bottom-level series are inferred as
series that never appear as a `parent_id`; no `is_bottom` column is used. Parent
nodes should also be rows in `series.csv`:

```text
series_id,series_name,target_name,unit,parent_id
total,Total,outbound_tourists,count,
america,America,outbound_tourists,count,total
canada,Canada,outbound_tourists,count,america
mexico,Mexico,outbound_tourists,count,america
```

### `panel.csv`

`panel.csv` stores all time-varying values in one long table:

```text
date,series_id,kind,name,value,lower,upper
```

The `kind` column describes the role of each row:

```text
observed
signal
exogenous
base_forecast
reference_forecast
terminal_forecast
recovery_forecast
```

The `name` column identifies the target, exogenous variable, model, or curve:

```text
target
search_index
flight_capacity
legacy_ensemble
legacy_average
intervention_adjusted
legacy_final
```

For example:

```text
2023-02-01,canada,signal,search_index,7512,,
2023-02-01,canada,exogenous,flight_capacity,120,,
2024-07-01,canada,base_forecast,legacy_ensemble,94848.9,,
2024-07-01,canada,terminal_forecast,intervention_adjusted,66394.2,,
```

### `config.yaml`

`config.yaml` stores global recovery-forecasting settings:

```yaml
frequency: MS
shock:
  start: "2020-01"
  end: "2022-12"
dates:
  observed_until: "2023-01"
  initial_date: "2023-06"
  forecast_start: "2023-08"
  terminal_date: "2024-07"
  forecast_end: "2024-07"
base:
  train_end: "2019-12"
  validation_start: "2018-01"
  validation_end: "2019-12"
  horizon: 60
  models:
    - seasonal_naive
    - random_walk_drift
    - arima
    - ets
    - holt
    - holt_winters
  ensemble: mean
  selection_fraction: 0.8
  validation_metric: mape
  stacking_alpha: 1.0
reference:
  start: "2023-01"
  end: "2023-06"
  train_end: "2023-01"
  x:
    - name: search_arimax
      variables: [search_index]
      method: arimax
      lag: 1
    - name: search_prophet
      variables: [search_index]
      method: prophet
      lag: 1
    - name: search_ratio
      variables: [search_index]
      method: ratio
      lag: 1
      ratio_window: 36
    - name: flight_growth
      variables: [flight_capacity]
      method: growth_rate
recovery:
  method: regression
  coefficient_column: coefficient
  score_columns: [policy, distance, recovery]
  weights:
    policy: 1.0
    distance: 1.0
    recovery: 1.0
  anchors:
    canada: 0.65
    mexico: 1.0
    hong_kong: 0.85
  min_coefficient: 0.0
  max_coefficient: 1.0
  score_min: 1.0
  score_max: 5.0
  fit_intercept: true
  preserve_anchors: true
curve:
  curves: [linear, quadratic, logistic]
  seasonal_period: 12
  trend_history_start: "2022-01"
  trend_history_end: "2023-06"
  quadratic_terminal_weight: 18
  logistic_anchor_dates: ["2023-12", "2024-07", "2024-12"]
interval:
  enabled: true
  alpha: 0.2
  method: residual_quantile
hierarchy:
  enabled: false
  method: bottom_up
  parent_column: parent_id
  apply_to: [recovery]
```

`RecoveryForecastingPipeline.from_dataset(dataset)` consumes these settings and
runs the implemented stages from the compact dataset. The tourism example now
uses package-native base models configured under `base:`. If no package-native
base model configuration is supplied, the pipeline falls back to the converted
`base_forecast / legacy_ensemble` rows as the terminal-stage baseline.

The package-native path is not intended to be numerically identical to every
legacy paper artifact by default. It uses Python implementations of the general
RISE stages, so outputs may differ slightly from the original R scripts and
notebooks even when the same conceptual model is used. Direct equality should be
expected only for converted legacy artifacts or for a pipeline configuration that
explicitly consumes those artifacts as inputs.

The `recovery:` block controls the terminal intervention coefficient. Use
`method: direct` to consume a coefficient column as-is, `method: weighted_score`
to map weighted factor scores directly into the coefficient range, or
`method: regression` to follow the paper's calibration idea with anchor
coefficients.

The `interval:` block controls prediction intervals. If `base_forecast` or
`reference_forecast` rows contain `lower` and `upper` values, those bounds are
propagated through the terminal intervention and recovery-curve stages. When
package-native base models are fitted with a validation window, the pipeline
uses validation residual quantiles to calibrate base forecast intervals and then
recovers final lower/upper paths from trend-scale recovery curves and seasonal
components.

The `hierarchy:` block is optional. With `enabled: true`, the pipeline forecasts
the inferred bottom-level series and then applies bottom-up reconciliation to
the final recovery forecast. Aggregate columns are produced by summing their
bottom descendants.

With `hierarchy.enabled: true`, hierarchical base candidates can be listed under
`base.models` so TopDown, WLS, and MinT forecasts participate in the same
validation and ensemble selection as ARIMA, ETS, and other base models.

Additional hierarchy methods are available through Nixtla's
`hierarchicalforecast` package. Supported method names include `top_down`,
`top_down_forecast_proportions`, `top_down_average_proportions`,
`top_down_proportion_averages`, `ols`, `wls_struct`, `wls_var`, `mint`,
`mint_shrink`, and `mint_cov`. Methods such as `wls_var` and `mint_shrink`
require insample actual and fitted values when used through the lower-level
`reconcile_forecasts` API.

For the tourism competition, the legacy artifacts map into this structure as:

```text
data.xlsx          -> observed / target
composite index    -> signal / search_index
flight.xlsx        -> signal / flight_capacity
baseline.xlsx      -> base_forecast / legacy_ensemble
reference.xlsx     -> reference_forecast / legacy_average
point_forecast.xlsx -> recovery_forecast / legacy_final
```

In Python, load this format with:

```python
from riseforecast import RecoveryDataset

dataset = RecoveryDataset.from_directory("examples/tourism_competition/data")
baseline = dataset.matrix(kind="base_forecast", name="legacy_ensemble")
```

To audit the migration against original legacy files, run:

```bash
python examples/tourism_competition/validate_reproduction.py
```

The `converted_vs_legacy` checks verify that compact CSV data matches the
original Excel/CSV artifacts. The `package_vs_legacy` checks compare the current
Python package outputs to the paper artifacts and should be interpreted as a
diagnostic report, not as a required exact-match test.

## Legacy Paper Implementation

For reproducing or checking the paper's original implementation, use the legacy
workflow under:

```text
legacypapercode/
```

The legacy workflow is a mixture of R scripts, Python notebooks, Excel files, and
generated artifacts. The original run order is documented in:

```text
legacypapercode/readme.txt
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
