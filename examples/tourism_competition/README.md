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

Model-based initial forecasts can be generated from the observed target data:

```bash
python examples/tourism_competition/initial_forecast.py
```

The migrated terminal stage can also be chained into the recovery curve stage:

```bash
python examples/tourism_competition/recovery_curve_forecast.py
```

Use `--initial-source base_models` to replace the converted legacy reference
forecast with the model-based initial forecast.

The legacy artifacts can be converted to the compact recovery forecasting data
format:

```bash
python examples/tourism_competition/convert_legacy_data.py
```
