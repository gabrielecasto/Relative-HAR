# Decomposing Realised Volatility Forecasts: A Relative HAR Approach

This repository contains the code for an empirical project on realised volatility forecasting.  
The project proposes and evaluates a **Relative HAR** framework for forecasting daily stock realised variance, comparing it against standard HAR-type benchmark models.

The main idea is that stock-level realised variance should not be treated only as an isolated autoregressive process. Instead, stock variance can be decomposed into different risk components: a market-wide component, a sector-specific component, and a residual stock-specific component. These components are forecast separately and then recombined to obtain the final stock-level volatility forecast.

## Project Overview

The project focuses on forecasting daily log realised variance for a broad cross-section of S&P 500 stocks using high-frequency intraday data from 2019 to 2023.

The workflow includes:

1. collecting and cleaning intraday stock price data;
2. computing daily realised variance from intraday returns;
3. selecting an appropriate sampling frequency using volatility signature plots;
4. estimating and comparing three forecasting models:
   - standard HAR;
   - HAR-X with market and sector variables;
   - Relative HAR;
5. evaluating out-of-sample forecasting performance;
6. investigating the sources of the Relative HAR model’s improvement.

## Data

The analysis uses minute-by-minute intraday prices for S&P 500 constituents.  
The stock universe is filtered to retain only liquid stocks with sufficient trading history and consistent data availability.

The market component is proxied using **SPY**, while sector-level volatility is proxied using sector ETFs corresponding to the GICS sector classification.

Realised variance is computed using **30-minute intraday returns**, selected after inspecting volatility signature plots. The final modelling target is the logarithm of daily realised variance.

## Models

### HAR Model

The baseline model is the standard HAR(1,5,22) specification. It forecasts next-day log realised variance using:

- the previous daily log realised variance;
- the weekly average over the previous 5 trading days;
- the monthly average over the previous 22 trading days.

### HAR-X Market-Sector Model

The HAR-X model extends the baseline HAR model by adding market and sector information directly as additional regressors. It includes daily, weekly, and monthly HAR components for:

- the individual stock;
- the market proxy;
- the corresponding sector ETF.

This model is used as a benchmark to check whether improvements come simply from adding market and sector information.

### Relative HAR Model

The Relative HAR model is the main contribution of the project. It decomposes stock-level log realised variance into:

1. a market component;
2. an orthogonal sector component, obtained after removing the market effect from sector volatility;
3. a residual stock-specific relative component.

Each component is forecast separately using a HAR(1,5,22) structure. The final forecast is then reconstructed using the estimated stock-specific loadings.

## Forecasting Protocol

All models are estimated using a rolling-window forecasting framework with daily re-estimation.

For each stock, model, training-window length, and forecast date, the code stores:

- the realised log variance;
- the one-step-ahead forecast;
- the forecast error;
- squared error;
- absolute error;
- QLIKE loss.

The models are evaluated on the same ticker-date panel to ensure that performance differences are due to model structure rather than data availability.

## Evaluation Metrics

Forecasting performance is evaluated using three loss functions:

- **MSE**: Mean Squared Error;
- **MAE**: Mean Absolute Error;
- **QLIKE**: Quasi-Likelihood loss.

All losses are computed on log realised variance forecasts.

Statistical significance is assessed using pairwise Diebold-Mariano-West tests with Newey-West standard errors. The tests compare Relative HAR against both HAR and HAR-X.

## Main Results

The Relative HAR model generally delivers lower out-of-sample losses than the benchmark models.

The strongest improvements are observed for:

- MSE;
- MAE.

For QLIKE, the results are more nuanced, especially for shorter training windows, but the Relative HAR model remains competitive and often performs close to or better than the alternatives.

Across training windows, Relative HAR produces statistically significant improvements for a substantial share of stocks, while statistically significant underperformance is rare.

At the sector level, the Relative HAR model also tends to improve MSE and MAE across most sectors.

## Additional Diagnostics

The project also investigates why the Relative HAR model improves forecasting performance.

The component forecastability diagnostic shows that market volatility is the most predictable component, followed by raw sector volatility and stock-level volatility. The orthogonal sector and residual relative components are noisier, but still contain useful predictive information.

The error decomposition analysis shows that most of the final forecast error comes from the residual stock-specific component. However, the recombination of component forecasts often generates error-offsetting effects, meaning that component forecast errors can partially compensate each other.

## Conclusion

The results suggest that decomposing stock realised variance into economically meaningful components can improve volatility forecasting.

Rather than simply adding market and sector variables as extra regressors, the Relative HAR model separates systematic, sector-specific, and stock-specific volatility dynamics before forecasting and recombining them. This structure allows the model to exploit both individual time-series predictability and cross-risk relationships.

Overall, the Relative HAR framework appears to be a promising extension of the standard HAR model for individual stock realised variance forecasting.

## Notes

This repository contains the code used to implement the empirical analysis.  
The raw intraday data and the full academic report are not included.
