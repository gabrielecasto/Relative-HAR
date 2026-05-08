


# In this section we will clean the data dropping some tickers with later
# specified criteria, filling eventual missing data and producing an output
# suitable for the following part of the process.



#______________________________CLEANING_DATA____________________________________

# This section creates the master grid. Define all trading days from starting
# date to end date by considering the days in which SP500 index is traded.
# For each trading day, we create a grid with all minutes from 9:30 to 15:59.

MasterGridCompleteData <- function() {
  # (Assumes INTRADAY_WIDE_DF exists)
  
  # Infer sample bounds directly from the data
  DATA_FROM <- as.Date(min(INTRADAY_WIDE_DF$datetime, na.rm = TRUE))
  DATA_TO <- as.Date(max(INTRADAY_WIDE_DF$datetime, na.rm = TRUE))
  
  # Compute list of trading dates
  TRADING_DAYS <- tidyquant::tq_get(
    "^GSPC", from = DATA_FROM, to = DATA_TO, get = "stock.prices"
  ) %>%
    dplyr::distinct(date) %>%
    dplyr::arrange(date) %>%
    dplyr::pull(date)
  
  # Build one minute level grid for trading day (09:30–15:59 -> 390 minutes)
  ONE_DAY_GRID <- tibble::tibble(
    minute_of_day = format(seq.POSIXt(
      from = as.POSIXct("1970-01-01 09:30:00", tz = "UTC"),
      to   = as.POSIXct("1970-01-01 15:59:00", tz = "UTC"),
      by   = "1 min"
    ), "%H:%M:%S")
  )
  
  # Combine grids day-minute
  MASTER_GRID <- tidyr::crossing(date = TRADING_DAYS, ONE_DAY_GRID) %>%
    dplyr::mutate(
      datetime = as.POSIXct(paste(date, minute_of_day), tz = "America/New_York")
    ) %>%
    dplyr::select(datetime) %>%
    dplyr::arrange(datetime)
  
  # Sanity check
  stopifnot(nrow(MASTER_GRID) == length(TRADING_DAYS) * 390)
  
  cat("Trading days:", length(TRADING_DAYS), "\n")
  cat("Master grid rows:", nrow(MASTER_GRID), "\n")
  cat("Expected rows:", length(TRADING_DAYS) * 390, "\n")
  
  return(list(MASTER_GRID = MASTER_GRID, TRADING_DAYS = TRADING_DAYS))
}



#________________________________FILTER_A_______________________________________

# Filter A drops all the tickers that have a coverage treshold lower than a
# specified percentage defined in file config e.g. Coverage_Threshold = 70%,
# will drop all tickers that have less than 70% of all
# the expected observations.

FilterA <- function(Coverage_Threshold) {
  
  # Align to MASTER_GRID (adds missing timestamps as rows)
  data.table::setDT(MASTER_GRID)
  data.table::setDT(INTRADAY_WIDE_DF)
  
  data.table::setkey(MASTER_GRID, datetime)
  data.table::setkey(INTRADAY_WIDE_DF, datetime)
  
  PRICES <- INTRADAY_WIDE_DF[MASTER_GRID]
  
  rm(INTRADAY_WIDE_DF, envir = .GlobalEnv); gc()
  
  stopifnot(nrow(PRICES) == nrow(MASTER_GRID))
  stopifnot(identical(PRICES$datetime, MASTER_GRID$datetime))
  
  # Alignment check
  stopifnot(nrow(PRICES) == nrow(MASTER_GRID))
  rm(MASTER_GRID, envir = .GlobalEnv);gc()
  
  ticker_cols <- setdiff(names(PRICES), "datetime")
  X_cov <- as.matrix(PRICES[, .SD, .SDcols = ticker_cols])
  
  n_total <- nrow(X_cov)
  n_obs <- colSums(!is.na(X_cov))
  coverage <- n_obs / n_total
  
  COVERAGE_TABLE <- tibble::tibble(
    ticker = ticker_cols,
    n_total = n_total,
    n_obs = as.integer(n_obs),
    coverage = as.numeric(coverage)
  ) %>%
    dplyr::arrange(coverage)
  
  TICKERS_PASS_A <- COVERAGE_TABLE$ticker[COVERAGE_TABLE$coverage >=
                                            Coverage_Threshold]
  TICKERS_DROP_A <- COVERAGE_TABLE$ticker[COVERAGE_TABLE$coverage <
                                            Coverage_Threshold]
  
  cat("Tickers total:", length(ticker_cols), "\n")
  cat("Pass A:", length(TICKERS_PASS_A), "\n")
  cat("Drop A:", length(TICKERS_DROP_A), "\n")
  
  # Keep only tickers that pass A (overwrite PRICES to avoid extra big objects)
  PRICES <- PRICES[, c("datetime", TICKERS_PASS_A), with = FALSE]
  
  rm(X_cov)
  
  return(PRICES)
}



#________________________________FILTER_B_______________________________________

# This filter removes tickers with insufficient intraday data quality based on
# the pattern of missing observations within trading days.
# i)  A day is classified as having a large gap if the maximum number of
# consecutive missing minutes is greater than or equal to Minutes_Big_Gap.
# ii) A ticker is removed if the fraction of days with a large gap exceeds
# Maximum_N_Big_Gaps.
# iii) A ticker is removed if the maximum intraday gap observed on any single
# day exceeds Max_Gap_Allowed.

FilterB <- function(Minutes_Big_Gap, Maximum_N_Big_Gaps, Max_Gap_Allowed) {
  
  ticker_cols_A <- setdiff(names(PRICES), "datetime")
  n_days <- length(TRADING_DAYS)
  
  DAY_IDXS <- matrix(seq_len(n_days * 390L), nrow = 390L, ncol = n_days)
  stopifnot(nrow(PRICES) == n_days * 390)
  
  # Convert to matrix once (fast + low overhead)
  X <- as.matrix(PRICES[, ..ticker_cols_A])
  OBS <- !is.na(X)
  
  future::plan(future::sequential)
  
  # Compute ticker-level gap stats
  gap_rows <- lapply(seq_along(ticker_cols_A), function(j) {
    o <- OBS[, j]
    
    max_gaps <- vapply(seq_len(n_days), function(d) {
      idx <- DAY_IDXS[, d]
      obs <- which(o[idx])
      
      if (length(obs) <= 1L) Inf else max(diff(obs)) - 1L
    }, numeric(1))
    
    n_big <- sum(max_gaps >= Minutes_Big_Gap, na.rm = TRUE)
    ratio_big <- n_big / n_days
    
    max_any <- suppressWarnings(max(max_gaps[is.finite(max_gaps)],
                                    na.rm = TRUE))
    if (!is.finite(max_any)) max_any <- Inf
    
    data.frame(
      ticker = ticker_cols_A[j],
      n_days = n_days,
      n_days_big_gap = n_big,
      ratio_days_big_gap = ratio_big,
      max_gap_any_day = max_any,
      stringsAsFactors = FALSE
    )
  })
  
  GAP_TICKER_STATS <- dplyr::bind_rows(gap_rows) %>%
    dplyr::as_tibble() %>%
    dplyr::arrange(desc(max_gap_any_day))
  
  rm(gap_rows)
  
  TICKERS_PASS_B <- GAP_TICKER_STATS$ticker[
    GAP_TICKER_STATS$ratio_days_big_gap <= Maximum_N_Big_Gaps &
      GAP_TICKER_STATS$max_gap_any_day < Max_Gap_Allowed
  ]
  
  TICKERS_DROP_B <- setdiff(ticker_cols_A, TICKERS_PASS_B)
  
  # Keep only tickers that pass B (overwrite PRICES)
  PRICES <- PRICES[, c("datetime", TICKERS_PASS_B), with = FALSE]
  
  cat("Tickers after Filter A:", length(ticker_cols_A), "\n")
  cat("Pass Filter B:", length(TICKERS_PASS_B), "\n")
  cat("Dropped by Filter B:", length(TICKERS_DROP_B), "\n")
  
  return(PRICES)
}



#____________________________FILL_MISSING_PRICES________________________________

# Fill missing prices per ticker by carrying the last observation forward (LOCF)
# and then backward (NOCB) to eliminate internal and edge NAs; store the filled
# panel as PRICES_FILLED and run basic sanity checks for NA/Inf/zero values.

FillMissingPrices <- function() {
  
  ticker_cols_B <- setdiff(names(PRICES), "datetime")
  
  data.table::setDT(PRICES)
  
  PRICES[, (ticker_cols_B) := lapply(.SD, data.table::nafill, type = "locf"),
         .SDcols = ticker_cols_B]
  PRICES[, (ticker_cols_B) := lapply(.SD, data.table::nafill, type = "nocb"),
         .SDcols = ticker_cols_B]
  
  # Final output in compact form
  PRICES_FILLED <- PRICES
  
  rm(PRICES, envir = .GlobalEnv);gc()
  
  # Check for NAs, Inf, 0
  DTp <- as.data.table(PRICES_FILLED)
  price_cols <- setdiff(names(DTp), "datetime")
  X <- as.matrix(DTp[, ..price_cols])
  
  number_NA <- sum(is.na(X))
  number_inf <- sum(is.infinite(X))
  number_of_0 <- sum(X == 0, na.rm = TRUE)
  
  cat("Number of NAs: ", number_NA, "\n", sep = "")
  cat("Number of Inf: ", number_inf, "\n", sep = "")
  cat("Number of 0s: ", number_of_0, "\n", sep = "")
  
  return(PRICES_FILLED)
}



#________________________________EARLY_CLOSE____________________________________

# In this section we remove the rows corresponding to minutes in which
# at least x% of returns across all stocks is 0. This arises from locf and nocb
# filling minutes with no data. In early close days this leads to a 0 return
# for minutes in which market is closed early.

EarlyClose <- function(ZeroShareThreshold) {
  
  # NA at the first observation of each day
  dates <- PRICES_FILLED$datetime
  
  LOG_RETURNS <- data.frame(
    datetime = dates,
    sapply(PRICES_FILLED[, -1], function(p) {
      r <- c(NA, diff(log(p)))
      
      day_change <- as.Date(dates)[-1] != as.Date(dates)[-length(dates)]
      day_change <- c(FALSE, day_change)
      
      r[day_change] <- NA
      r
    })
  )
  
  rm(PRICES_FILLED, envir = .GlobalEnv);gc()
  
  DT_lr <- as.data.table(LOG_RETURNS)
  ret_cols <- setdiff(names(DT_lr), "datetime")
  
  # Share of EXACT zeros among non-NA returns (per row)
  DT_lr[, zero_share := rowSums(.SD == 0, na.rm = TRUE) / rowSums(!is.na(.SD)),
        .SDcols = ret_cols]
  
  # Drop rows where > 90% of tickers have return == 0
  DT_lr <- DT_lr[is.na(zero_share) | zero_share <= ZeroShareThreshold]
  
  # Remove helper col and overwrite LOG_RETURNS 
  # (keep same object name for downstream code)
  DT_lr[, zero_share := NULL]
  LOG_RETURNS <- as.data.frame(DT_lr)
  
  DT <- as.data.table(LOG_RETURNS)
  tickers <- setdiff(names(DT), "datetime")
  
  DT[, date := as.Date(datetime)]
  DT[, m := seq_len(.N), by = date]
  
  return(list(DT = DT, tickers = tickers))
}



#_____________________________END_OF_THE_SCRIPT_________________________________