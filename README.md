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
code and supplementary materials/
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
  config.py
  data.py
  intervention.py
  recovery.py
  curves.py
  ensembles.py
  reference.py
  hierarchy.py
  metrics.py
  pipeline.py
```

The first implemented functional stage is the intervention-adjusted terminal
forecast:

```python
from riseforecast import intervention_terminal_forecast

terminal = intervention_terminal_forecast(
    base_forecast=baseline_df,
    coefficients=recovery_coefficients,
    terminal_date="2024-07",
)
```

This applies:

```text
terminal forecast = no-shock baseline forecast at terminal date * intervention coefficient
```

To run the current tests:

```bash
source ~/.virtualenvs/py3.12-forecasting/bin/activate
python -m pytest
```

## Legacy Paper Implementation

For reproducing or checking the paper's original implementation, use the legacy
workflow under:

```text
code and supplementary materials/
```

The legacy workflow is a mixture of R scripts, Python notebooks, Excel files, and
generated artifacts. The original run order is documented in:

```text
code and supplementary materials/readme.txt
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
