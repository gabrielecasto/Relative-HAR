


# In this section we can run the whole script, using functions from other
# files in the same working directory


#________________________________PREPARATION____________________________________

# Clean the environment
#rm(list = ls()); gc()

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")

# Connect to the scripts of the same working directory
sources <- c(
  "setup.R",
  "import.R",
  "cleaning.R",
  "sectors.R",
  "signature.R",
  "signature_plots.R",
  "HAR.R",
  "HAR_X_market_sector.R",
  "relative_HAR.R",
  "forecast_output_checks.R",
  "model_comparison_training_windows.R",
  "model_comparison_significance.R",
  "forecastability_diagnostic.R",
  "relative_HAR_error_decomposition.R"
)

stopifnot(all(file.exists(sources)))
invisible(lapply(sources,source))



#_______________________________IMPORT_DATA_____________________________________

# In this section we import the tickers that compose the SP500 index and the
# minute-level closing prices for each stock. We also import sector ETFs
# and SPY. Sector ETFs can be defined in "Sectors.R" in the function
# GetSectorSTFMap().

# Use API key
# Polygon API key from local environment variable
readRenviron(".Renviron")
POLYGON_KEY <- Sys.getenv("POLYGON_API_KEY")

if (!nzchar(POLYGON_KEY)) {
  stop("Missing POLYGON_API_KEY. Please create a local .Renviron file.", call.
       = FALSE)
}

# Import SP500 tickers
STOCK_TICKERS <- c(GetSp500Tickers())

# Add sectors and relative sector ETF
TICKER_SECTOR_TABLE <- GetSp500SectorsWikipedia(tickers = STOCK_TICKERS)
TICKER_SECTOR_TABLE <- AddSectorEtf(ticker_sector_table = TICKER_SECTOR_TABLE,
                                    sector_etf_map = GetSectorETFMap())

# Find ETFs to download
ETFS <- unique(stats::na.omit(TICKER_SECTOR_TABLE$sector_etf))

# Set parallel plan for import
future::plan(future::multisession, workers=max(parallel::detectCores() - 1, 1))
cat("Workers before import:", future::nbrOfWorkers(), "\n")

# Import minute level data for SP500 tickers, SPY and sector ETFs by importing
# individual years and binding rows
IMPORT_YEARS <- 2019:2023

INTRADAY_YEAR_LIST <- vector("list", length(IMPORT_YEARS))
names(INTRADAY_YEAR_LIST) <- as.character(IMPORT_YEARS)

for (year in IMPORT_YEARS) {
  
  message("\n==============================")
  message("Importing year: ", year)
  message("==============================\n")
  
  from_year <- as.Date(paste0(year, "-01-01"))
  to_year <- as.Date(paste0(year, "-12-31"))
  
  INTRADAY_YEAR_LIST[[as.character(year)]] <- BuildWideIntradayDf(
    tickers = c(STOCK_TICKERS, "SPY", ETFS),
    from_date = from_year,
    to_date = to_year,
    multiplier = 1,
    timespan = "minute",
    sleep_sec = 0.3,
    verbose = TRUE,
    NA_Share_Threshold = 0.9
  )
  
  invisible(gc())
}

# Combine all yearly datasets into one full intraday panel
INTRADAY_WIDE_DF <- data.table::rbindlist(INTRADAY_YEAR_LIST, fill = TRUE)

# Deduplicate and sort by datetime
INTRADAY_WIDE_DF <- unique(INTRADAY_WIDE_DF, by = "datetime")
data.table::setorder(INTRADAY_WIDE_DF, datetime)

# Clean temporary objects
rm(INTRADAY_YEAR_LIST, IMPORT_YEARS,
   from_year, to_year, year)
invisible(gc())

# Reset parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers before cleaning:", future::nbrOfWorkers(), "\n")



#_______________________________CLEANING_DATA___________________________________

# In this section we clean the data. Start by building a master grid composed of
# all the expected minutes for each day that we are considering.
# Output is master grid and trading days.

out <- MasterGridCompleteData()
MASTER_GRID <- out$MASTER_GRID
TRADING_DAYS <- out$TRADING_DAYS
rm(out); invisible(gc())

# We perform the first stage of cleaning in which we drop all the tickers that
# have less than x% of expected observations

PRICES <- FilterA(Coverage_Threshold = 0.9)

# Check if ETFS are in PIRCES
suppressWarnings(ETFS[!(ETFS %in% names(PRICES))])

# We perform the second stage of cleaning in which we drop all the tickers that
# i) A day is classified as having a large gap if the maximum number of
# consecutive missing minutes is greater than or equal to Minutes_Big_Gap.
# ii) A ticker is removed if the fraction of days with a large gap exceeds
#     Maximum_N_Big_Gaps.
# iii) A ticker is removed if the maximum intraday gap observed on any single
#      day exceeds Max_Gap_Allowed.

PRICES <- FilterB(Minutes_Big_Gap = 10,
                  Maximum_N_Big_Gaps = 0.10,
                  Max_Gap_Allowed = 22)

# Check if ETFS and SPY are in PIRCES
suppressWarnings(ETFS[!(ETFS %in% names(PRICES))])
suppressWarnings("SPY"[!("SPY" %in% names(PRICES))])

# Fill missing prices per ticker by carrying the last observation forward (LOCF)
# and then backward (NOCB) to eliminate internal and edge NAs; store the filled
# panel as PRICES_FILLED and run basic sanity checks for NA/Inf/zero values.

PRICES_FILLED <- FillMissingPrices()
rm(PRICES, envir = .GlobalEnv); invisible(gc())

# In this section we remove the rows corresponding to minutes in which
# at least x% of returns across all stocks is 0. This arises from locf and nocb
# filling minutes with no data. In early close days this leads to a 0 return
# for minutes in which market is closed early.

out1 <- EarlyClose(ZeroShareThreshold = 0.9)
DT <- out1$DT
tickers <- out1$tickers
rm(out1, PRICES_FILLED);invisible(gc())

# Check if ETFS and SPY are in DT
suppressWarnings(ETFS[!(ETFS %in% names(DT))])
suppressWarnings("SPY"[!("SPY" %in% names(DT))])

# Drop INTRADAY_WIDE_DF
rm(INTRADAY_WIDE_DF); invisible(gc())



#____________________________VOLATILITY_SIGNATURE_______________________________

# In this section we compute the volatility signature for each selected stock.
# Starting from one-minute intraday log returns, we aggregate returns over
# different time intervals, compute daily realized variance for each interval,
# and then summarize the average realized variance across trading days.

# Set parallel plan for signature plot
future::plan(future::multisession, workers = 4)
cat("Workers before signature:", future::nbrOfWorkers(), "\n")

# Stocks selected to perform signature
STOCK_TICKERS_SIGNATURE <- intersect(
  as.character(unlist(STOCK_TICKERS, use.names = FALSE)),
  colnames(DT)
)

# Option for parallel work
options(future.globals.maxSize = 8 * 1024^3)

# Signature statistics for each stock
SIGNATURE_BY_STOCK <- BuildVolatilitySignature(
  DF = DT,
  tickers = STOCK_TICKERS_SIGNATURE,
  intervals = 1:70,
  date_col_name = "datetime",
  include_partial_last_block = TRUE,
  minimum_days_required = 50,
  show_progress = TRUE
)

rm(STOCK_TICKERS);invisible(gc())



#_____________________________SIGNATURE_PLOTS___________________________________

# In this section we prepare and plot the volatility signature results. Starting
# from the stock-level realized variance estimates computed at different
# sampling intervals, we normalize each stock by its own stable plateau level.
# Then, we build cross-sectional and sector-level summaries and produce plots
# that show how realized variance changes across sampling frequencies.

# Prepare the plot-ready volatility signature dataframe
SIGNATURE_PLOT_DF <- PrepareSignaturePlotData(
  signature_by_stock = SIGNATURE_BY_STOCK,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  plateau_intervals = 40:70,
  minimum_days_required = 50,
  drop_invalid_plateau = TRUE
)

# Build the cross-sectional summary across sampling intervals
SIGNATURE_INTERVAL_SUMMARY <- BuildSignatureIntervalSummary(
  signature_plot_df = SIGNATURE_PLOT_DF,
  ratio_col = "rv_ratio"
)

# Build the sector-level summary of the normalized signature
SIGNATURE_SECTOR_SUMMARY <- BuildSignatureSectorSummary(
  signature_plot_df = SIGNATURE_PLOT_DF,
  sector_col = "sector",
  ratio_col = "rv_ratio",
  minimum_stocks_per_sector = 10,
  drop_missing_sector = TRUE
)

# Plot the normalized volatility signature across all stocks
P_SIGNATURE_SPAGHETTI <- PlotSignatureSpaghetti(
  signature_plot_df = SIGNATURE_PLOT_DF,
  signature_interval_summary = SIGNATURE_INTERVAL_SUMMARY,
  ratio_col = "rv_ratio",
  lower_band_col = "p10_ratio",
  upper_band_col = "p90_ratio",
  median_col = "median_ratio",
  reference_interval = 30,
  x_breaks = c(1, 5, 10, 15, 20, 30, 40, 50, 60, 70),
  y_limits = NULL,
  title = "Volatility Signature Plot",
  subtitle = "Normalized realized volatility across stocks",
  output_path = "figures/signature_spaghetti.pdf"
)

print(P_SIGNATURE_SPAGHETTI)

# Plot the normalized volatility signature separately by sector
P_SIGNATURE_SECTOR <- PlotSignatureBySector(
  signature_sector_summary = SIGNATURE_SECTOR_SUMMARY,
  sector_col = "sector",
  lower_band_col = "p25_ratio",
  upper_band_col = "p75_ratio",
  median_col = "median_ratio",
  reference_interval = 30,
  x_breaks = c(1, 10, 20, 30, 40, 50, 60, 70),
  y_limits = NULL,
  title = "Sector-Level Volatility Signature Plot",
  subtitle = "Normalized realized volatility by sector",
  facet_ncol = 3,
  output_path = "figures/signature_by_sector.pdf"
)

print(P_SIGNATURE_SECTOR)

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")

rm(P_SIGNATURE_SECTOR, P_SIGNATURE_SPAGHETTI,
   SIGNATURE_SECTOR_SUMMARY, SIGNATURE_PLOT_DF, SIGNATURE_INTERVAL_SUMMARY)
invisible(gc())



#____________________________________HAR________________________________________

# In this section we build and estimate rolling HAR models for realized
# volatility forecasting. Starting from one-minute intraday log returns, we
# first construct daily log realized variance series for each stock. Then, we
# build HAR regressors using daily, weekly and monthly variance components,
# estimate rolling one-step-ahead HAR forecasts, and store the corresponding
# forecast errors. Lags to determine the three regressors are 1,5,22 by default
# but can be modified.

# Set parallel plan for HAR model
future::plan(future::multisession, workers = 4)
cat("Workers before HAR:", future::nbrOfWorkers(), "\n")

# Select tickers for HAR estimation
STOCK_TICKERS_HAR <- c(TICKER_SECTOR_TABLE$ticker, "SPY",
                       unique(TICKER_SECTOR_TABLE$sector_etf))

# Build 30-minutes daily Realized Variance panel
DAILY_RV_30MIN <- BuildDailyRVPanel(
  DF = DT,
  tickers = STOCK_TICKERS_HAR,
  interval_minutes = 30,
  date_col_name = "datetime",
  include_partial_last_block = FALSE,
  minimum_blocks_required = 10
)

# Clean daily RV panel
# Remove dates where SPY or any sector ETF has non-finite daily log RV.
# These dates cannot be used by the relative HAR model, so they are removed
# before estimating both HAR and relative HAR.

BENCHMARK_TICKERS <- unique(c("SPY", TICKER_SECTOR_TABLE$sector_etf))

BENCHMARK_TICKERS <- BENCHMARK_TICKERS[
  BENCHMARK_TICKERS %in% names(DAILY_RV_30MIN)
]

bad_benchmark_rows <- Reduce(
  `|`,
  lapply(BENCHMARK_TICKERS, function(ticker) {
    !is.finite(DAILY_RV_30MIN[[ticker]])
  })
)

BAD_BENCHMARK_DATES <- DAILY_RV_30MIN$date[bad_benchmark_rows]

cat("Dropped benchmark bad dates:\n")
print(BAD_BENCHMARK_DATES)

DAILY_RV_30MIN <- DAILY_RV_30MIN[!bad_benchmark_rows, ]
rownames(DAILY_RV_30MIN) <- NULL


# Remove DOW because it has ticker-specific missing daily log RV observations
if ("DOW" %in% names(DAILY_RV_30MIN)) {
  DAILY_RV_30MIN$DOW <- NULL
}

# Prepare the relative HAR ticker universe after cleaning the daily RV panel
RELATIVE_HAR_TICKERS <- PrepareRelativeHARTickers(
  daily_log_rv_wide = DAILY_RV_30MIN,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  market_ticker = "SPY",
  date_col_name = "date"
)

rownames(RELATIVE_HAR_TICKERS) <- NULL

# Use the same stock universe for standard HAR and relative HAR
STOCK_TICKERS_HAR <- RELATIVE_HAR_TICKERS$ticker

# Estimate rolling HAR and report prediction errors out-of-sample (1-step ahead)
HAR_FORECAST_ERRORS <- RollingHARForecastPanel(
  daily_log_rv_wide = DAILY_RV_30MIN,
  tickers = STOCK_TICKERS_HAR,
  training_window = 750,
  first_lag = 1,
  second_lag = 5,
  third_lag = 22
)

# Remove non used objects
#rm(DT)
invisible(gc())

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")



#____________________________X_HAR_MARKET_SECTOR________________________________

# In this section we build and estimate rolling HAR-X market-sector models for
# realized volatility forecasting. The model extends the standard HAR benchmark
# by adding market and sector information directly as additional regressors.
# Starting from daily log realized variance series, we construct daily, weekly
# and monthly HAR components for each stock, for the market benchmark, and for
# the corresponding sector ETF. Then, we estimate rolling one-step-ahead OLS
# forecasts and store the corresponding forecast errors.

# Set parallel plan for HAR model
future::plan(future::multisession, workers = 4)
cat("Workers before signature:", future::nbrOfWorkers(), "\n")

# Estimate rolling HAR-X market-sector forecasts out-of-sample (1-step ahead)
HAR_X_MARKET_SECTOR_FORECAST_ERRORS <- RollingHARXMarketSectorForecastPanel(
  daily_log_rv_wide = DAILY_RV_30MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  training_window = 750,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22
)

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")



#_______________________________RELATIVE_HAR____________________________________

# In this section we build and estimate relative HAR models for realized
# volatility forecasting. Starting from daily log realized variance series,
# we decompose each stock volatility into market, sector-specific and relative
# components. Then, we forecast each component separately and reconstruct the
# one-step-ahead stock log realized variance forecast.

# Set parallel plan for relative HAR model
future::plan(future::multisession, workers=4)
cat("Workers before relative HAR:", future::nbrOfWorkers(), "\n")

RELATIVE_HAR_FORECAST_ERRORS <- RollingRelativeHARForecastPanel(
  daily_log_rv_wide = DAILY_RV_30MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  training_window = 750,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22,
  minimum_observations = 30
)

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")



#___________________________FORECAST_OUTPUT_CHECKS______________________________

# In this section we check the integrity and alignment of the forecast-error
# outputs produced by the different realized volatility forecasting models.
# The checks are performed before any model performance comparison. First, each
# model output is inspected separately to verify that the required columns are
# available, dates and tickers are correctly formatted, forecast errors are
# internally consistent, and no duplicated ticker-date observations are present.
# Then, the outputs of all models are compared to ensure that they refer to the
# same ticker universe, target dates, ticker-date pairs, realized actual values
# and forecast origins. Finally, sector-level metadata are checked for
# consistency across models and, when available, against the reference
# ticker-sector table.

MODEL_OUTPUTS <- list(
  HAR = HAR_FORECAST_ERRORS,
  HAR_X_MARKET_SECTOR = HAR_X_MARKET_SECTOR_FORECAST_ERRORS,
  RELATIVE_HAR = RELATIVE_HAR_FORECAST_ERRORS
)

MODEL_OUTPUT_ALIGNMENT <- CheckForecastOutputAlignment(
  model_outputs = MODEL_OUTPUTS,
  reference_model = "HAR",
  tolerance = 1e-10,
  verbose = FALSE
)

MODEL_METADATA_CHECKS <- CheckModelMetadataConsistency(
  alignment_checks = MODEL_OUTPUT_ALIGNMENT,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  market_ticker = "SPY",
  verbose = FALSE
)

ALL_MODEL_CHECKS_PASSED <- isTRUE(MODEL_OUTPUT_ALIGNMENT$passed) &&
  isTRUE(MODEL_METADATA_CHECKS$passed)

if (ALL_MODEL_CHECKS_PASSED) {
  cat("\nAll model integrity checks passed.\n")
} else {
  cat("\nSome model integrity checks failed.\n")
}

# Remove non used objects
rm(HAR_FORECAST_ERRORS, HAR_X_MARKET_SECTOR_FORECAST_ERRORS,
  RELATIVE_HAR_FORECAST_ERRORS, MODEL_OUTPUTS, MODEL_OUTPUT_ALIGNMENT,
  MODEL_METADATA_CHECKS, ALL_MODEL_CHECKS_PASSED, STOCK_TICKERS_HAR)
invisible(gc())



#____________________________MODEL_COMPARISON___________________________________

# In this section we estimate HAR, HAR-X market-sector and relative HAR models
# across different training windows. We store aggregate loss measures (across
# all stocks and across by sector) and plot them.

# Set parallel plan for model comparison
future::plan(future::multisession, workers = 4)
cat("Workers before model comparison:", future::nbrOfWorkers(), "\n")

# Define training windows
TRAINING_WINDOWS <- seq(50, 800, by = 50)

MODEL_COMPARISON_RESULTS <- RunModelComparisonAcrossTrainingWindows(
  daily_log_rv_wide = DAILY_RV_30MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  training_windows = TRAINING_WINDOWS,
  rv_interval_minutes = 30,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22,
  minimum_observations = 30,
  tolerance = 1e-10
)

LOSS_PANEL <- MODEL_COMPARISON_RESULTS$loss_panel
TICKER_MODEL_LOSSES <- MODEL_COMPARISON_RESULTS$ticker_losses
OVERALL_MODEL_LOSSES <- MODEL_COMPARISON_RESULTS$overall_losses
SECTOR_MODEL_LOSSES <- MODEL_COMPARISON_RESULTS$sector_losses

# Plot the results, aggregate and by sector
MODEL_COMPARISON_PLOTS <- PlotModelLossCurvesAcrossTrainingWindows(
  overall_losses = MODEL_COMPARISON_RESULTS$overall_losses,
  sector_losses = MODEL_COMPARISON_RESULTS$sector_losses,
  output_dir = "figures/model_comparison",
  save_plots = TRUE,
  overall_x_breaks = TRAINING_WINDOWS,
  sector_x_breaks = c(50, 200, 400, 600, 800)
)

# Aggregate results
P_OVERALL_MSE <- MODEL_COMPARISON_PLOTS$overall_plots$mse
P_OVERALL_MAE <- MODEL_COMPARISON_PLOTS$overall_plots$mae
P_OVERALL_QLIKE <- MODEL_COMPARISON_PLOTS$overall_plots$qlike

print(P_OVERALL_MSE)
print(P_OVERALL_MAE)
print(P_OVERALL_QLIKE)

# Results by sector
P_SECTOR_MSE <- MODEL_COMPARISON_PLOTS$sector_plots$mse
P_SECTOR_MAE <- MODEL_COMPARISON_PLOTS$sector_plots$mae
P_SECTOR_QLIKE <- MODEL_COMPARISON_PLOTS$sector_plots$qlike

print(P_SECTOR_MSE)
print(P_SECTOR_MAE)
print(P_SECTOR_QLIKE)

# Build a sector-level table of percentage loss reductions relative to HAR
# for selected training windows. Positive values mean lower losses than HAR.
SELECTED_TRAINING_WINDOWS <- c(100, 400, 800)

SECTOR_PERCENT_REDUCTION_VS_HAR_TABLE <- SECTOR_MODEL_LOSSES %>%
  dplyr::filter(training_window %in% SELECTED_TRAINING_WINDOWS) %>%
  dplyr::group_by(training_window, rv_interval_minutes, sector) %>%
  dplyr::mutate(har_mse = mse[model == "HAR"][1],
    har_mae = mae[model == "HAR"][1], har_qlike = qlike[model == "HAR"][1],
    mse_reduction_vs_har_pct = 100 * (har_mse - mse) / har_mse,
    mae_reduction_vs_har_pct = 100 * (har_mae - mae) / har_mae,
    qlike_reduction_vs_har_pct = 100 * (har_qlike - qlike) / har_qlike) %>%
  dplyr::ungroup() %>%
  
  # HAR is the benchmark, so it is excluded from the final table
  dplyr::filter(model != "HAR") %>%
  dplyr::select(sector, model, training_window, mse_reduction_vs_har_pct,
    mae_reduction_vs_har_pct, qlike_reduction_vs_har_pct) %>%
  tidyr::pivot_longer(cols = c(mse_reduction_vs_har_pct,
      mae_reduction_vs_har_pct, qlike_reduction_vs_har_pct),
    names_to = "metric", values_to = "loss_reduction_vs_har_pct") %>%
  dplyr::mutate(
    metric = dplyr::case_when(metric == "mse_reduction_vs_har_pct" ~ "MSE",
      metric == "mae_reduction_vs_har_pct" ~ "MAE",
      metric == "qlike_reduction_vs_har_pct" ~ "QLIKE", TRUE ~ metric),
    column_name = paste0(metric, " training = ", training_window),
    loss_reduction_vs_har_pct = round(loss_reduction_vs_har_pct, 2)) %>%
  dplyr::select(sector, model, column_name,loss_reduction_vs_har_pct) %>%
  tidyr::pivot_wider(
    names_from = column_name, values_from = loss_reduction_vs_har_pct) %>%
  dplyr::arrange(sector, model)

# Remove non used objects
rm(MODEL_COMPARISON_RESULTS, MODEL_COMPARISON_PLOTS, TRAINING_WINDOWS,
   TICKER_SECTOR_TABLE, P_OVERALL_MSE, P_OVERALL_MAE,
   P_OVERALL_QLIKE, P_SECTOR_MSE, P_SECTOR_MAE, P_SECTOR_QLIKE,
   OVERALL_MODEL_LOSSES, SECTOR_MODEL_LOSSES)
invisible(gc())

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")



#_______________________RELATIVE_HAR_SIGNIFICANCE_TESTS_________________________

# In this section we test whether the relative HAR model provides statistically
# significant forecasting improvements relative to the benchmark models.
# Starting from the long LOSS_PANEL produced by the model-comparison section,
# we compare relative HAR losses with HAR and HAR-X market-sector losses for
# each stock, training window and loss metric. The loss differences are tested
# using a Diebold-Mariano-West statistic with Newey-West long-run variance.
# Finally, we summarize the percentage of stocks showing significant
# improvement or underperformance and plot these results across training
# windows.

# Test whether relative HAR significantly improves or underperforms against
# HAR and HAR-X market-sector for each training window and loss metric
RELATIVE_HAR_SIGNIFICANCE <- RunRelativeHARSignificanceTests(
  loss_panel = LOSS_PANEL,
  relative_model = "RELATIVE_HAR",
  benchmark_models = c("HAR", "HAR_X_MARKET_SECTOR"),
  metrics = c("squared_error", "absolute_error", "qlike"),
  alpha = 0.05,
  nw_lag = NULL
)

# Plot percentage of stocks with significant relative HAR improvements
P_SIGNIFICANT_IMPROVEMENT <- PlotRelativeHARSignificance(
  summary_by_window = RELATIVE_HAR_SIGNIFICANCE$summary_by_window,
  test_type = "improvement",
  output_path = "figures/model_comparison/significant_improvement.pdf"
)

# Plot percentage of stocks with significant relative HAR underperformance
P_SIGNIFICANT_UNDERPERFORMANCE <- PlotRelativeHARSignificance(
  summary_by_window = RELATIVE_HAR_SIGNIFICANCE$summary_by_window,
  test_type = "underperformance",
  output_path = "figures/model_comparison/significant_underperformance.pdf"
)

print(P_SIGNIFICANT_IMPROVEMENT)
print(P_SIGNIFICANT_UNDERPERFORMANCE)



#________________________FORECASTABILITY_DIAGNOSTICS____________________________

# In this section we test whether the relative HAR components are more
# forecastable than the direct stock log realized variance. We do this for one
# selected training window by comparing HAR forecasts against a rolling-mean
# benchmark and computing out-of-sample R-squared values.

# Set parallel plan for forecastability diagnostics
future::plan(future::multisession, workers = 4)
cat("Workers before forecastability diagnostics:",
    future::nbrOfWorkers(), "\n")

# Build the component-level forecastability panel
FORECASTABILITY_PANEL <- BuildForecastabilityPanel(
  daily_log_rv_wide = DAILY_RV_30MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  training_window = 800,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22,
  minimum_observations = 30
)

# Summarize out-of-sample R-squared values
FORECASTABILITY_RESULTS <- SummarizeForecastabilityR2(
  forecastability_panel = FORECASTABILITY_PANEL
)

FORECASTABILITY_COMPONENT_SUMMARY <- FORECASTABILITY_RESULTS$component_summary
FORECASTABILITY_SECTOR_COMPARISON_TABLE <- 
  FORECASTABILITY_RESULTS$sector_comparison_table

# Print compact results
cat("\nForecastability component summary:\n")
print(FORECASTABILITY_COMPONENT_SUMMARY)

cat("\nForecastability comparison table:\n")
print(FORECASTABILITY_SECTOR_COMPARISON_TABLE)

# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")

P_FORECASTABILITY_R2_BY_SECTOR <- PlotForecastabilityR2BoxplotBySector(
  r2_table = FORECASTABILITY_RESULTS$r2_table,
  output_path = "figures/model_comparison/forecastability_r2_by_sector_800.pdf"
)

print(P_FORECASTABILITY_R2_BY_SECTOR)

# Remove non used objects
rm(FORECASTABILITY_PANEL, FORECASTABILITY_RESULTS)
invisible(gc())



#_______________________RELATIVE_HAR_ERROR_DECOMPOSITION________________________

# In this section we analyze the forecast-error structure of the relative HAR
# model. Starting from the relative HAR forecast-error output and the daily log
# realized variance panel, we reconstruct the realized market, sector-perp and
# relative q components at each target date. We then decompose the final
# forecast error into weighted component errors, summarize their contribution
# to the final MSFE by sector, and plot the squared components together with
# the cross-term contribution.

# Set parallel plan for relative HAR model
future::plan(future::multisession, workers=4)
cat("Workers before relative HAR:", future::nbrOfWorkers(), "\n")

RELATIVE_HAR_DECOMPOSITION_FORECAST_ERRORS <- RollingRelativeHARForecastPanel(
  daily_log_rv_wide = DAILY_RV_30MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  training_window = 800,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22,
  minimum_observations = 30
)

# Build the relative HAR error decomposition panel
RELATIVE_HAR_ERROR_DECOMPOSITION_PANEL <- 
  BuildRelativeHARErrorDecompositionPanel(
    relative_har_forecast_errors = RELATIVE_HAR_DECOMPOSITION_FORECAST_ERRORS,
    daily_log_rv_wide = DAILY_RV_30MIN,
    tolerance = 1e-8
  )

RELATIVE_HAR_ERROR_DECOMPOSITION_SUMMARY <- 
  SummarizeRelativeHARErrorDecomposition(
    decomposition_panel = RELATIVE_HAR_ERROR_DECOMPOSITION_PANEL
  )


# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")

# Plot results
P_RELATIVE_HAR_ERROR_DECOMPOSITION <- 
  PlotRelativeHARErrorDecompositionDiverging(
  error_decomposition_summary = RELATIVE_HAR_ERROR_DECOMPOSITION_SUMMARY,
  output_path = "figures/model_comparison/relative_har_error_decomposition_800.pdf"
)

print(P_RELATIVE_HAR_ERROR_DECOMPOSITION)



#_____________________________END_OF_THE_SCRIPT_________________________________