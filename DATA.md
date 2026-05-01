# Recovery Forecasting Data Format

`riseforecast` uses a compact, general recovery forecasting data structure with
three files:

```text
series.csv
panel.csv
config.yaml
```

The format is entity-neutral. A `series_id` can be a country, product, store,
route, market, sector, or any other quantity of interest recovering after a
shock.

## `series.csv`

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

## `panel.csv`

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

## `config.yaml`

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
runs the implemented stages from the compact dataset. If no package-native base
model configuration is supplied, the pipeline falls back to the converted
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

## Tourism Competition Mapping

For the tourism competition, the legacy artifacts map into this structure as:

```text
data.xlsx           -> observed / target
composite index     -> signal / search_index
flight.xlsx         -> signal / flight_capacity
baseline.xlsx       -> base_forecast / legacy_ensemble
reference.xlsx      -> reference_forecast / legacy_average
point_forecast.xlsx -> recovery_forecast / legacy_final
```

In Python, load this format with:

```python
from riseforecast import RecoveryDataset

dataset = RecoveryDataset.from_directory("examples/tourism_competition/data")
baseline = dataset.matrix(kind="base_forecast", name="legacy_ensemble")
```

For package-native diagnostics of the migrated tourism workflow, run:

```bash
python examples/tourism_competition/diagnose_package_forecast.py
```

This reports selected base models, validation errors, recovery coefficients,
terminal forecasts, final forecast summaries, and point/interval evaluation when
actual observations overlap the forecast period. This is the recommended
validation path for the generalized Python package.

For an optional audit against original legacy files in
`examples/tourism_competition/legacypapercode/`, run:

```bash
python examples/tourism_competition/validate_reproduction.py
```

The `converted_vs_legacy` checks verify that compact CSV data matches the
original Excel/CSV artifacts. The `package_vs_legacy` checks compare the current
Python package outputs to the paper artifacts and should be interpreted as a
diagnostic report, not as a required exact-match test. The generated
`examples/tourism_competition/reproduction_validation.csv` report is ignored by
git.
