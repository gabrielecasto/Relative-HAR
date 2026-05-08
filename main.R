
# In this section we can run the whole script, using functions from other
# files in the same working directory


#________________________________PREPARATION____________________________________

# Clean the environment
rm(list = ls()); gc()

# Connect to the scripts of the same working directory
sources <- c(
  "setup.R",
  "config.R",
  "import.R",
  "cleaning.R"
)

stopifnot(all(file.exists(sources)))
invisible(lapply(sources,source))



#_______________________________IMPORT_DATA_____________________________________

# In this section we import the tickers that compose the SP500 index and the
# minute-level closing prices for each stock.

# Use API key
# Polygon API key from local environment variable
readRenviron(".Renviron")
POLYGON_KEY <- Sys.getenv("POLYGON_API_KEY")

if (!nzchar(POLYGON_KEY)) {
  stop("Missing POLYGON_API_KEY. Please create a local .Renviron file.", call.
       = FALSE)
}

# Import SP500 tickers
TICKERS <- c(GetSp500Tickers(), "SPY")

# Import minute level data
INTRADAY_WIDE_DF <- BuildWideIntradayDf(
  tickers = TICKERS,
  from_date = as.Date("2020-02-01"),
  to_date = as.Date("2021-01-31"),
  multiplier = 1,
  timespan = "minute",
  sleep_sec = 0.1,
  verbose = TRUE
)



#______________________________CLEANING_DATA____________________________________

# In this section we clean the data. Start by building a master grid composed of
# all the expected minutes for each day that we are considering.
# Output is master grid and trading days.

out <- MasterGridCompleteData()
MASTER_GRID <- out$MASTER_GRID
TRADING_DAYS <- out$TRADING_DAYS

rm(out);invisible(gc())

# We perform the first stage of cleaning in which we drop all the tickers that
# have less than x% of expected observations

PRICES <- FilterA(Coverage_Threshold = 0.9)

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

# Fill missing prices per ticker by carrying the last observation forward (LOCF)
# and then backward (NOCB) to eliminate internal and edge NAs; store the filled
# panel as PRICES_FILLED and run basic sanity checks for NA/Inf/zero values.

PRICES_FILLED <- FillMissingPrices()

# In this section we remove the rows corresponding to minutes in which
# at least x% of returns across all stocks is 0. This arises from locf and nocb
# filling minutes with no data. In early close days this leads to a 0 return
# for minutes in which market is closed early.

out1 <- EarlyClose(ZeroShareThreshold = 0.9)
DT <- out1$DT
tickers <- out1$tickers
rm(out1);invisible(gc())









