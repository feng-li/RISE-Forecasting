# Tourism Competition Example

This directory will host the paper reproduction workflow on top of the reusable
`riseforecast` package.

The migration should keep tourism-specific files here:

- competition data loading and column mapping
- Baidu search keyword configuration
- flight data preparation
- destination hierarchy
- paper-specific recovery score table
- reproduction checks against legacy Excel outputs

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
the migrated reference stage defines three named X cases: `search_arimax`,
`search_ratio`, and `flight_growth`.

```bash
python examples/tourism_competition/reference_forecast.py
```

The migrated terminal stage can also be chained into the recovery curve stage:

```bash
python examples/tourism_competition/recovery_curve_forecast.py
```

The full runner uses the configured X-based reference forecasts as the initial
anchor and the converted legacy baseline as the terminal-stage baseline unless a
package-native base configuration is added to `config.yaml`.

Migration checks use `utilsforecast` metrics to compare migrated outputs against
the converted legacy artifacts:

```bash
python examples/tourism_competition/evaluate_migration.py
```

The legacy artifacts can be converted to the compact recovery forecasting data
format:

```bash
python examples/tourism_competition/convert_legacy_data.py
```
