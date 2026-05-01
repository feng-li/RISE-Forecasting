# Tourism Competition Example

This directory contains the tourism competition example for the reusable
`riseforecast` recovery forecasting package.

The exact paper implementation is preserved here:

```text
examples/tourism_competition/legacypapercode/
```

The migration should keep tourism-specific files here:

- competition data loading and column mapping
- Baidu search keyword configuration
- flight data preparation
- destination hierarchy
- paper-specific recovery score table
- optional reproduction checks against legacy Excel outputs

The terminal forecast stage can already be run against the legacy baseline
artifact converted to the compact dataset:

```bash
python examples/tourism_competition/terminal_forecast.py
```

The full migrated workflow is config-driven from
`examples/tourism_competition/data/config.yaml`:

```bash
python examples/tourism_competition/run_forecast.py
```

Model-based initial forecasts can be generated from the observed target data:

```bash
python examples/tourism_competition/initial_forecast.py
```

Reference forecasts can be generated from arbitrary exogenous variables `X`.
In this example, Baidu search and flights are tourism-specific X variables, and
the migrated reference stage supports named X cases such as `search_arimax`,
`search_prophet`, `search_ratio`, and `flight_growth`.

```bash
python examples/tourism_competition/reference_forecast.py
```

The migrated terminal stage can also be chained into the recovery curve stage:

```bash
python examples/tourism_competition/recovery_curve_forecast.py
```

Forecast outputs can be visualized with Plotly through
`riseforecast.plot_forecast` or `riseforecast.plot_recovery_curve`. These helpers
accept optional `entities=` filters so the tourism example can plot selected
destinations without overcrowding the figure.

```bash
python examples/tourism_competition/plot_forecast.py --entities canada mexico hong_kong
```

The full runner uses the configured package-native base models for the no-shock
baseline. It now validates the base models on the configured pre-shock validation
window, imputes internal missing values with Kalman smoothing, keeps the best
configured fraction, and combines the selected forecasts with the configured
ensemble method. When a `parent_id` hierarchy is present, base candidates such
as `top_down_arima`, `top_down_ets`, `wls_struct`, and `mint_shrink` can be
listed in `base.models` and selected by the same validation procedure. The
configured X-based reference forecasts
are then used as the initial anchor. The terminal stage estimates recovery
coefficients from the three paper score factors
(`policy`, `distance`, and `recovery`) using the configured regression anchors.
The recovery curve stage estimates STL seasonal multipliers from historical pre-shock
observations, fits the three paper-style trend curves, and then recovers the full
forecast from the trend recovery curve and seasonal components. The returned
forecast object keeps these pieces separately.
Optional hierarchy reconciliation is configured through `parent_id` metadata in
`series.csv`; bottom nodes are inferred from `parent_id`, without an `is_bottom`
column. The package supports bottom-up reconciliation directly and can use
`hierarchicalforecast` methods such as top-down and `wls_struct`/MinTrace when
the required all-node or insample inputs are available.
Removing the `base:` section from `config.yaml` makes the pipeline fall back to
the converted legacy baseline artifact.

Migration checks use `utilsforecast` metrics to compare migrated outputs against
the converted legacy artifacts by stage: base, reference, terminal, and final
recovery forecast.

```bash
python examples/tourism_competition/evaluate_migration.py
```

Package-native diagnostics summarize the configured Python workflow without
treating exact legacy reproduction as the target:

```bash
python examples/tourism_competition/diagnose_package_forecast.py
```

This reports selected base models, validation errors, recovery coefficients,
terminal forecasts, final forecast summaries, and point/interval evaluation when
actual observations overlap the forecast period. Use `--output-dir` to save the
diagnostic tables as CSV files.

For an optional direct reproduction audit against the original
`examples/tourism_competition/legacypapercode/` Excel and CSV artifacts, run:

```bash
python examples/tourism_competition/validate_reproduction.py
```

This is a legacy audit, not the package-native validation target. It writes
`examples/tourism_competition/reproduction_validation.csv` and prints an overall
stage summary. The `converted_vs_legacy` rows should pass up to small floating
point differences; `package_vs_legacy` rows quantify where the generalized
package implementation differs from the original paper scripts. These
differences are expected unless the package is configured to consume the same
legacy stage artifacts. The Python implementation uses standard package APIs and
current dependency behavior, so it can produce slightly different results from
the original R/notebook workflow even for closely related models.

The legacy artifacts under `examples/tourism_competition/legacypapercode/` can be
converted to the compact recovery forecasting data format:

```bash
python examples/tourism_competition/convert_legacy_data.py
```
