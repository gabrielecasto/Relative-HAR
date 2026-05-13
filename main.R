
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
  "analysis.R",
  "signature.R"
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
future::plan(future::multisession, workers =
               max(parallel::detectCores() - 1, 1))
cat("Workers before import:", future::nbrOfWorkers(), "\n")

# Import minute level data for SP500 tickers, SPY and sector ETFs
INTRADAY_WIDE_DF <- BuildWideIntradayDf(
  tickers = c(STOCK_TICKERS, "SPY", ETFS),
  from_date = as.Date("2022-01-01"),
  to_date = as.Date("2023-01-31"),
  multiplier = 1,
  timespan = "minute",
  sleep_sec = 0.3,
  verbose = TRUE,
  NA_Share_Threshold = 0.9
)

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
rm(out1);invisible(gc())

# Check if ETFS are in DT
suppressWarnings(setdiff(ETFS, colnames(DT)))



#_______________________________SIGNATURE_PLOT__________________________________



# Set parallel plan for signature plot
future::plan(future::multisession, workers = 
               max(parallel::detectCores() - 1, 1))
cat("Workers before signature:", future::nbrOfWorkers(), "\n")


STOCK_TICKERS_SIGNATURE <- intersect(
  as.character(unlist(STOCK_TICKERS, use.names = FALSE)),
  colnames(DT)
)

progressr::handlers(global = TRUE)
progressr::handlers("txtprogressbar")

options(future.globals.maxSize = 8 * 1024^3)

SIGNATURE_BY_STOCK <- BuildVolatilitySignature(
  DT = DT,
  tickers = STOCK_TICKERS_SIGNATURE,
  intervals = 1:40,
  date_col_name = "datetime",
  include_partial_last_block = FALSE,
  minimum_days_required = 1,
  show_progress = TRUE
)

SIGNATURE_AGGREGATE <- AggregateVolatilitySignature(
  signature_by_stock = SIGNATURE_BY_STOCK
)

signature_plot <- ggplot2::ggplot(
  SIGNATURE_AGGREGATE,
  ggplot2::aes(x = interval_minutes)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = p25_mean_rv,
      ymax = p75_mean_rv
    ),
    fill = "grey80",
    alpha = 0.7
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = median_mean_rv),
    color = "red",
    linewidth = 1
  ) +
  ggplot2::labs(
    title = "Volatility Signature Plot",
    x = "Sampling interval in minutes",
    y = "Sample average realized volatility"
  ) +
  ggplot2::theme_minimal()

print(signature_plot)
