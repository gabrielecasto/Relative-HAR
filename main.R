
# In this section we can run the whole script, using functions from other
# files in the same working directory


#________________________________PREPARATION____________________________________

# Clean the environment
#rm(list = ls()); gc()

# Connect to the scripts of the same working directory
sources <- c(
  "import.R",
  "cleaning.R",
  "setup.R"
)

stopifnot(all(file.exists(sources)))
invisible(lapply(sources,source))



#_______________________________IMPORT_DATA_____________________________________

# In this section we import the tickers that compose the SP500 index and the
# minute-level closing prices for each stock.

# Import SP500 tickers
TICKERS <- c(GetSp500Tickers(), "SPY")

# Import minute level data
INTRADAY_WIDE_DF <- BuildWideIntradayDf(
  tickers = TICKERS,
  from_date = as.Date("2019-08-01"),
  to_date = as.Date("2021-01-31"),
  multiplier = 1,
  timespan = "minute",
  sleep_sec = 0.1,
  verbose = TRUE
)













