


# In this section we build the functions to ...



#_______________________________SIGNATURE_PLOT__________________________________

# This function converts one-minute intraday log returns into
# interval-level log returns.
# Example: interval_minutes = 5, r_5min = r_1 + r_2 + r_3 + r_4 + r_5

BuildIntervalReturnsForTickerDay <- function(returns_1min,
                                             interval_minutes,
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
  }
  
  # Create block identifiers: 1, 1, ..., 2, 2, ..., etc.
  block_id <- ceiling(seq_along(returns_1min) / interval_minutes)
  
  # Sum one-minute log returns within each block
  interval_returns <- tapply(
    returns_1min,
    block_id,
    sum
  )
  
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

ComputeSignatureForTicker <- function(DT,
                                      ticker,
                                      intervals = 1:150,
                                      date_col_name = "date",
                                      include_partial_last_block = TRUE,
                                      minimum_days_required = 1) {
  
  if (!ticker %in% names(DT)) {
    stop(paste("Ticker", ticker, "not found in DT."), call. = FALSE)
  }
  
  if (!date_col_name %in% names(DT)) {
    stop(paste("DT must contain the column", date_col_name), call. = FALSE)
  }
  
  # Keep only date and ticker returns
  stock_data <- data.frame(date = as.Date(DT[[date_col_name]]),
                           return = DT[[ticker]])
  
  # Split one-minute returns by trading day
  returns_by_day <- split(stock_data$return, stock_data$date)
  
  signature_list <- lapply(intervals, function(interval_minutes) {
    
    daily_results <- lapply(returns_by_day, function(day_returns) {
      ComputeRVForInterval(
        returns_1min = day_returns,
        interval_minutes = interval_minutes,
        include_partial_last_block = include_partial_last_block
      )
    })
    
    daily_results <- do.call(rbind, daily_results)
    
    # Keep only valid daily RV values
    valid_rv <- daily_results$daily_rv[is.finite(daily_results$daily_rv)]
    valid_blocks <- daily_results$n_blocks[daily_results$n_blocks > 0]
    
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
  
  return(signature_table)
}




# This function computes the volatility signature for all selected tickers.
# Output is one compact table with one row for each ticker and interval.
# Important: daily RV values are computed internally but are not saved.

BuildVolatilitySignature <- function(DT,
                                     tickers,
                                     intervals = 1:150,
                                     date_col_name = "date",
                                     include_partial_last_block = TRUE,
                                     minimum_days_required = 1,
                                     show_progress = TRUE) {
  
  DT <- as.data.frame(DT)
  date_col_name <- as.character(date_col_name)[1]
  
  if (missing(tickers) || is.null(tickers)) {
    tickers <- setdiff(names(DT), c("datetime", "date", "m"))
  }
  
  tickers <- as.character(unlist(tickers, use.names = FALSE))
  tickers <- tickers[tickers %in% names(DT)]
  
  if (length(tickers) == 0) {
    stop("No valid tickers found in DT.", call. = FALSE)
  }
  
  if (show_progress) {
    
    signature_list <- progressr::with_progress({
      
      p <- progressr::progressor(steps = length(tickers))
      
      future.apply::future_lapply(tickers, function(ticker) {
        
        out <- ComputeSignatureForTicker(
          DT = DT,
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
    
  } else {
    
    signature_list <- future.apply::future_lapply(tickers, function(ticker) {
      
      ComputeSignatureForTicker(
        DT = DT,
        ticker = ticker,
        intervals = intervals,
        include_partial_last_block = include_partial_last_block,
        minimum_days_required = minimum_days_required,
        date_col_name = date_col_name
      )
    })
  }
  
  signature_by_stock <- do.call(rbind, signature_list)
  rownames(signature_by_stock) <- NULL
  
  return(signature_by_stock)
}



# This function aggregates the volatility signature across stocks.
# Input is one row for each ticker and interval. Output is one row for each
# interval, with cross-sectional median and dispersion.

AggregateVolatilitySignature <- function(signature_by_stock) {
  
  required_cols <- c("interval_minutes", "mean_rv")
  
  if (!all(required_cols %in% names(signature_by_stock))) {
    stop("signature_by_stock must contain 'interval_minutes' and 'mean_rv'.",
         call. = FALSE)
  }
  
  intervals <- sort(unique(signature_by_stock$interval_minutes))
  
  aggregate_list <- lapply(intervals, function(interval_minutes) {
    
    rv_values <- signature_by_stock$mean_rv[
      signature_by_stock$interval_minutes == interval_minutes
    ]
    
    rv_values <- rv_values[is.finite(rv_values)]
    
    if (length(rv_values) == 0) {
      return(data.frame(
        interval_minutes = interval_minutes,
        median_mean_rv = NA_real_,
        p10_mean_rv = NA_real_,
        p25_mean_rv = NA_real_,
        p75_mean_rv = NA_real_,
        p90_mean_rv = NA_real_,
        mean_mean_rv = NA_real_,
        n_stocks = 0L
      ))
    }
    
    return(data.frame(
      interval_minutes = interval_minutes,
      median_mean_rv = median(rv_values),
      p10_mean_rv = as.numeric(quantile(rv_values, 0.10, names = FALSE)),
      p25_mean_rv = as.numeric(quantile(rv_values, 0.25, names = FALSE)),
      p75_mean_rv = as.numeric(quantile(rv_values, 0.75, names = FALSE)),
      p90_mean_rv = as.numeric(quantile(rv_values, 0.90, names = FALSE)),
      mean_mean_rv = mean(rv_values),
      n_stocks = length(rv_values)
    ))
  })
  
  signature_aggregate <- do.call(rbind, aggregate_list)
  rownames(signature_aggregate) <- NULL
  
  return(signature_aggregate)
}