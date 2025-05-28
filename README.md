# RISE: Recovery-Informed Strategy Enhancement in Post-COVID-19 Chinese Outbound Tourism Forecasting

This repository contains the implementation of **RISE** (Recovery-Informed Strategy Enhancement), a three-stage forecasting framework developed for predicting the recovery of **Chinese outbound tourism** in the post-COVID-19 era. The framework was awarded **1st place in point forecasting** and **3rd place in interval forecasting** in the **Tourism Forecasting Competition II (2023)**.

---

## 🔍 Overview

RISE addresses the challenges of recovery forecasting following a structural disruption (e.g., the COVID-19 pandemic) by combining:

1. **Base forecasts** from a diverse pool of 13 time series models.
2. **Reference forecasts** incorporating real-time indicators (Baidu search index, international flight data).
3. **Recovery curve adjustments** using linear, quadratic, and logistic models, tailored to each destination.

---

## 🧠 Methodology

The RISE framework proceeds in **three stages**:

### 1. Base Forecast
- Uses 13 univariate time series models (e.g., ARIMA, ETS, TBATS, NNAR).
- Applies three forecast combination methods: simple average, error-weighted, and stacking (Lasso/Ridge).
- Incorporates **hierarchical forecast reconciliation** (MinT, WLS) for destination grouping consistency.

### 2. Reference Forecast
- Enriches short-term forecasts using:
  - **Baidu search index** (lagged correlation to tourism).
  - **Flight frequency data** from VariFlight.
- Two estimation strategies: multivariate time series (ARIMAX, Prophet) and search/tourism ratio modeling.

### 3. Recovery Curve Forecast
- Connects initial and terminal points via **recovery coefficients** and **three curve types**:
  - Linear
  - Quadratic
  - Logistic
- Seasonal components extracted using **STL decomposition** (multiplicative form).

---


---

## 🚀 Getting Started

### Prerequisites
- R ≥ 4.0
- Recommended packages:
  - `forecast`, `hts`, `prophet`, `imputeTS`, `nnet`, `glmnet`, `dplyr`, `ggplot2`

### Running the Pipeline
```r
# Install required packages
install.packages(c("forecast", "hts", "prophet", "imputeTS", "nnet", "glmnet", "dplyr", "ggplot2"))

# Run the main script
source("main.R")
```
## Citation

If you use this code or build upon it, please cite:

Feng Li & Taozhu Ruan (2025). RISE: Recovery-Informed Strategy Enhancement in Post-COVID-19 Chinese Outbound Tourism Forecasting. *Manuscript*. [DOI]

