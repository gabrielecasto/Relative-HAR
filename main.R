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
  "relative_HAR.R",
  "model_comparison_plots.R"
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
IMPORT_YEARS <- 2019:2022

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



#______________________________CLEANING_DATA____________________________________

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
                  Max_Gap_Allowed = 20)

# Check if ETFS are in PIRCES
suppressWarnings(ETFS[!(ETFS %in% names(PRICES))])

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

# Check if ETFS are in DT
suppressWarnings(setdiff(ETFS, colnames(DT)))



#____________________________VOLATILITY_SIGNATURE_______________________________

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
  intervals = 1:30,
  date_col_name = "datetime",
  include_partial_last_block = TRUE,
  minimum_days_required = 1,
  show_progress = TRUE
)

rm(STOCK_TICKERS);invisible(gc())



#_____________________________SIGNATURE_PLOTS___________________________________


SIGNATURE_PLOT_DF <- PrepareSignaturePlotData(
  signature_by_stock = SIGNATURE_BY_STOCK,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  plateau_intervals = 10:30,
  noise_intervals = 1:2,
  reference_interval = 10,
  minimum_days_required = 50,
  drop_invalid_plateau = TRUE
)

SIGNATURE_INTERVAL_SUMMARY <- BuildSignatureIntervalSummary(
  signature_plot_df = SIGNATURE_PLOT_DF,
  ratio_col = "rv_ratio"
)

SIGNATURE_SECTOR_SUMMARY <- BuildSignatureSectorSummary(
  signature_plot_df = SIGNATURE_PLOT_DF,
  sector_col = "sector",
  ratio_col = "rv_ratio",
  minimum_stocks_per_sector = 10,
  drop_missing_sector = TRUE
)


SIGNATURE_DECISION_TABLE <- BuildSignatureDecisionTable(
  signature_plot_df = SIGNATURE_PLOT_DF,
  ratio_col = "rv_ratio"
)

print(SIGNATURE_DECISION_TABLE)


P_SIGNATURE_SPAGHETTI <- PlotSignatureSpaghetti(
  signature_plot_df = SIGNATURE_PLOT_DF,
  signature_interval_summary = SIGNATURE_INTERVAL_SUMMARY,
  ratio_col = "rv_ratio",
  lower_band_col = "p10_ratio",
  upper_band_col = "p90_ratio",
  median_col = "median_ratio",
  reference_interval = 10,
  y_limits = NULL,
  output_path = "figures/signature_spaghetti.pdf"
)

print(P_SIGNATURE_SPAGHETTI)

P_SIGNATURE_HEATMAP <- PlotSignatureHeatmap(
  signature_plot_df = SIGNATURE_PLOT_DF,
  fill_col = "log_rv_ratio",
  order_col = "noise_ratio",
  reference_interval = 10,
  cap_quantile = 0.98,
  show_ticker_labels = FALSE,
  output_path = "figures/signature_heatmap.pdf"
)

print(P_SIGNATURE_HEATMAP)


P_SIGNATURE_SECTOR <- PlotSignatureBySector(
  signature_sector_summary = SIGNATURE_SECTOR_SUMMARY,
  sector_col = "sector",
  lower_band_col = "p25_ratio",
  upper_band_col = "p75_ratio",
  median_col = "median_ratio",
  reference_interval = 10,
  y_limits = NULL,
  facet_ncol = 3,
  output_path = "figures/signature_by_sector.pdf"
)

print(P_SIGNATURE_SECTOR)


# Reset the parallel plan
future::plan(future::sequential)
invisible(gc())
cat("Workers after reset:", future::nbrOfWorkers(), "\n")

rm(P_SIGNATURE_HEATMAP, P_SIGNATURE_SECTOR, P_SIGNATURE_SPAGHETTI,
   SIGNATURE_AGGREGATE, SIGNATURE_SECTOR_SUMMARY, signature_plot,
   SIGNATURE_PLOT_DF, SIGNATURE_BY_STOCK, SIGNATURE_DECISION_TABLE,
   SIGNATURE_INTERVAL_SUMMARY)
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
cat("Workers before signature:", future::nbrOfWorkers(), "\n")

# Select tickers for HAR estimation
STOCK_TICKERS_HAR <- c(TICKER_SECTOR_TABLE$ticker, "SPY",
                       unique(TICKER_SECTOR_TABLE$sector_etf))

# Build 10-minutes daily Realized Variance panel
DAILY_RV_10MIN <- BuildDailyRVPanel(
  DF = DT,
  tickers = STOCK_TICKERS_HAR,
  interval_minutes = 10,
  date_col_name = "datetime",
  include_partial_last_block = FALSE,
  minimum_blocks_required = 1
)

# Estimate rolling HAR and report prediction errors out-of-sample (1-step ahead)
HAR_FORECAST_ERRORS <- RollingHARForecastPanel(
  daily_log_rv_wide = DAILY_RV_10MIN,
  tickers = STOCK_TICKERS_HAR,
  training_window = 500,
  first_lag = 1,
  second_lag = 5,
  third_lag = 22
)



#_______________________________RELATIVE_HAR____________________________________

# Set parallel plan for relative HAR model
future::plan(future::multisession, workers=4)
cat("Workers before import:", future::nbrOfWorkers(), "\n")

RELATIVE_HAR_TICKERS <- PrepareRelativeHARTickers(
  daily_log_rv_wide = DAILY_RV_10MIN,
  ticker_sector_table = TICKER_SECTOR_TABLE,
  market_ticker = "SPY",
  date_col_name = "date"
)

RELATIVE_HAR_FORECAST_ERRORS <- RollingRelativeHARForecastPanel(
  daily_log_rv_wide = DAILY_RV_10MIN,
  relative_har_tickers = RELATIVE_HAR_TICKERS,
  training_window = 500,
  market_ticker = "SPY",
  first_lag = 1,
  second_lag = 5,
  third_lag = 22,
  minimum_observations = 30
)



#___________________________MODEL_COMPARISON_PLOTS______________________________

# In this section we compare the forecasting performance of the standard HAR
# model and the relative HAR model. Forecasts are matched by ticker and target
# date, then percentage errors are computed on the log realized variance scale.
# The script produces sector-level time-series plots, overlapping error
# histograms, and a summary table comparing mean, median and tail forecast
# errors across models.

# Build a panel that allows for a graphical and numerical model comparison
MODEL_COMPARISON <- BuildModelComparisonPanel(
  har_forecast_errors = HAR_FORECAST_ERRORS,
  relative_har_forecast_errors = RELATIVE_HAR_FORECAST_ERRORS
)

# Plot the percentage out of sample estimation errors for HAR and relative_HAR
# over time, by sector
P_PERCENTAGE_ERRORS_OVER_TIME_BY_SECTOR <-
  PlotPercentageErrorsOverTimeBySector(model_comparison = MODEL_COMPARISON,
  facet_ncol = 3,
  output_path = "figures/percentage_errors_over_time_by_sector.pdf"
)

print(P_PERCENTAGE_ERRORS_OVER_TIME_BY_SECTOR)

# Plot the distribution of the percentage out of sample estimation errors
# for HAR and relative_HAR by sector
P_PERCENTAGE_ERROR_HISTOGRAMS_BY_SECTOR <- 
  PlotPercentageErrorHistogramsBySector(model_comparison = MODEL_COMPARISON,
  bins = 45,
  cap_quantile = 0.99,
  facet_ncol = 3,
  output_path = "figures/percentage_error_histograms_by_sector.pdf"
)

print(P_PERCENTAGE_ERROR_HISTOGRAMS_BY_SECTOR)

SECTOR_ERROR_SUMMARY <- BuildSectorErrorSummary(
  model_comparison = MODEL_COMPARISON
)

print(SECTOR_ERROR_SUMMARY)













