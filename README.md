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

Implemented stages now include model-based initial forecasts, intervention-adjusted
terminal forecasts, and recovery curve forecasts:

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
from riseforecast import RecoveryDataset

dataset = RecoveryDataset.from_directory("examples/tourism_competition/data")

observed = dataset.observed_target()
baseline = dataset.base_forecast()
reference = dataset.reference_forecast()
coefficients = dataset.coefficients()
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

The paper base models now use direct StatsForecast implementations where
available, including `AutoARIMA`, `AutoETS`, `Holt`, `HoltWinters`,
`SeasonalNaive`, `RandomWalkWithDrift`, `MSTL`, and `AutoTBATS`. The `nnetar`
wrapper remains a scikit-learn autoregression approximation.

This applies:

```text
terminal forecast = no-shock baseline forecast at terminal date * intervention coefficient
```

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
python examples/tourism_competition/terminal_forecast.py
python examples/tourism_competition/initial_forecast.py
python examples/tourism_competition/recovery_curve_forecast.py
```

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
series_id,series_name,target_name,unit,group,subgroup,policy,distance,recovery,coefficient
canada,Canada,outbound_tourists,count,America,,3,1,2,0.70
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
base_forecast
reference_forecast
terminal_forecast
recovery_forecast
```

The `name` column identifies the target, signal, model, or curve:

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
```

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
