


# In this section we compute the volatility signature for each selected stock.
# Starting from one-minute intraday log returns, we aggregate returns over
# different time intervals, compute daily realized variance for each interval,
# and then summarize the average realized variance across trading days.



#____________________________VOLATILITY_SIGNATURE_______________________________

# This function converts one-minute intraday log returns into
# interval-level log returns.
# Example: interval_minutes = 5, r_5min = r_1 + r_2 + r_3 + r_4 + r_5

BuildIntervalReturnsForTickerDay <- function(returns_1min, interval_minutes,
                                             include_partial_last_block=TRUE) {
  
  if (interval_minutes < 1) {
    stop("interval_minutes must be greater than or equal to 1.", call. = FALSE)
  }
  
  # Remove missing returns, for example the first return of each day
  returns_1min <- returns_1min[is.finite(returns_1min)]
  if (length(returns_1min) == 0) {return(numeric(0))}
  
  # If requested, keep only complete blocks
  if (!include_partial_last_block) {
    
    n_complete <- floor(length(returns_1min) / interval_minutes) *
      interval_minutes
    
    if (n_complete == 0) {
      return(numeric(0))
    }
    
    returns_1min <- returns_1min[seq_len(n_complete)]
    
    # Fast complete-block aggregation.
    # Each column of the matrix corresponds to one intraday return block.
    interval_returns <- colSums(
      matrix(returns_1min, nrow = interval_minutes)
    )
    
  } else {
    
    # Create block identifiers: 1, 1, ..., 2, 2, ..., etc.
    block_id <- ((seq_along(returns_1min) - 1L) %/% interval_minutes) + 1L
    
    # Fast aggregation that also keeps the last partial block, if present.
    interval_returns <- as.numeric(
      rowsum(
        matrix(returns_1min, ncol = 1),
        group = block_id,
        reorder = FALSE
      )
    )
  }
  
  return(as.numeric(interval_returns))
}

# This function computes daily realized volatility for one stock, using
# interval-level log returns.
# Example: interval_minutes = 5, RV_5min = sum(r_5min^2)
# Important: the function does not take logs and does not annualize the RV.

ComputeRVForInterval <- function(returns_1min, interval_minutes,
                                 include_partial_last_block = TRUE) {
  
  interval_returns <- BuildIntervalReturnsForTickerDay(
    returns_1min = returns_1min,
    interval_minutes = interval_minutes,
    include_partial_last_block = include_partial_last_block
  )
  
  if (length(interval_returns) == 0) {
    return(data.frame(
      daily_rv = NA_real_,
      n_blocks = 0L
    ))
  }
  
  daily_rv <- sum(interval_returns^2)
  
  return(data.frame(
    daily_rv = daily_rv,
    n_blocks = length(interval_returns)
  ))
}

# This function computes the volatility signature for one ticker.
# For each interval: 1) computes daily RV within each trading day; 2) averages
# daily RV across days; 3) returns one compact row per interval.
# Important: daily blocks are always reset at the beginning of each trading day.

ComputeSignatureForTicker <- function(DF, ticker, intervals,
                                      date_col_name,
                                      include_partial_last_block,
                                      minimum_days_required) {
  
  if (!ticker %in% names(DF)) {
    stop(paste("Ticker", ticker, "not found in DF."), call. = FALSE)
  }
  
  if (!date_col_name %in% names(DF)) {
    stop(paste("DF must contain the column", date_col_name), call. = FALSE)
  }
  
  # Keep only valid dates and selected ticker returns
  date_vector <- as.Date(DF[[date_col_name]])
  return_vector <- DF[[ticker]]
  
  # Match the behavior of split(): rows with missing dates are not used
  valid_date_rows <- !is.na(date_vector)
  date_vector <- date_vector[valid_date_rows]
  return_vector <- return_vector[valid_date_rows]
  
  # Split one-minute returns by trading day only once
  returns_by_day <- split(return_vector, date_vector)
  
  signature_list <- lapply(intervals, function(interval_minutes) {
    
    # Pre-allocate daily outputs to avoid creating one small dataframe per day
    daily_rv <- rep(NA_real_, length(returns_by_day))
    n_blocks <- rep(0L, length(returns_by_day))
    
    for (day_index in seq_along(returns_by_day)) {
      
      interval_returns <- BuildIntervalReturnsForTickerDay(
        returns_1min = returns_by_day[[day_index]],
        interval_minutes = interval_minutes,
        include_partial_last_block = include_partial_last_block
      )
      
      n_blocks[day_index] <- length(interval_returns)
      
      if (length(interval_returns) > 0) {
        daily_rv[day_index] <- sum(interval_returns^2)
      }
    }
    
    # Keep only valid daily RV values
    valid_rv <- daily_rv[is.finite(daily_rv)]
    valid_blocks <- n_blocks[n_blocks > 0]
    
    if (length(valid_rv) < minimum_days_required) {
      return(data.frame(
        ticker = ticker,
        interval_minutes = interval_minutes,
        mean_rv = NA_real_,
        median_daily_rv = NA_real_,
        p10_daily_rv = NA_real_,
        p25_daily_rv = NA_real_,
        p75_daily_rv = NA_real_,
        p90_daily_rv = NA_real_,
        n_days = length(valid_rv),
        avg_n_blocks = NA_real_
      ))
    }
    
    return(data.frame(
      ticker = ticker,
      interval_minutes = interval_minutes,
      mean_rv = mean(valid_rv),
      median_daily_rv = median(valid_rv),
      p10_daily_rv = as.numeric(quantile(valid_rv, 0.10, names = FALSE)),
      p25_daily_rv = as.numeric(quantile(valid_rv, 0.25, names = FALSE)),
      p75_daily_rv = as.numeric(quantile(valid_rv, 0.75, names = FALSE)),
      p90_daily_rv = as.numeric(quantile(valid_rv, 0.90, names = FALSE)),
      n_days = length(valid_rv),
      avg_n_blocks = mean(valid_blocks)
    ))
  })
  
  signature_table <- do.call(rbind, signature_list)
  rownames(signature_table) <- NULL
  
  return(signature_table)
}



# This function computes the volatility signature for all selected tickers.
# Output is one compact table with one row for each ticker and interval.
# Important: daily RV values are computed internally but are not saved.

BuildVolatilitySignature <- function(DF, tickers, intervals, date_col_name,
                                     include_partial_last_block,
                                     minimum_days_required,
                                     show_progress) {
  
  DF <- as.data.frame(DF)
  date_col_name <- as.character(date_col_name)[1]
  
  tickers <- as.character(unlist(tickers, use.names = FALSE))
  tickers <- tickers[tickers %in% names(DF)]
  
  if (length(tickers) == 0) {
    stop("No valid tickers found in DF.", call. = FALSE)
  }
  
  # Progress bar
  signature_list <- progressr::with_progress({
    p <- progressr::progressor(steps = length(tickers))
      
    # Work in parallel
    future.apply::future_lapply(tickers, function(ticker) {
      
      out <- ComputeSignatureForTicker(
        DF = DF,
        ticker = ticker,
        intervals = intervals,
        include_partial_last_block = include_partial_last_block,
        minimum_days_required = minimum_days_required,
        date_col_name = date_col_name
        )
      
      p(sprintf("ticker %s", ticker))
      
      return(out)
      })
    })
  
  signature_by_stock <- do.call(rbind, signature_list)
  rownames(signature_by_stock) <- NULL
  
  return(signature_by_stock)
}



#_____________________________END_OF_THE_SCRIPT_________________________________