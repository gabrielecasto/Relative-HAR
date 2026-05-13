# This function builds a wide daily log realized volatility dataframe.
# Starting from one-minute intraday log returns, it aggregates returns over a
# chosen interval and computes daily realized variance as the sum of squared
# interval-level log returns. The final output has dates as rows and tickers
# as columns, with log daily realized variance in the cells.

BuildDailyRVPanel <- function(DF, tickers, interval_minutes, date_col_name,
                              include_partial_last_block,
                              minimum_blocks_required) {
  
  DF <- as.data.frame(DF)
  
  # Check that the date column exists
  if (!date_col_name %in% names(DF)) {
    stop(paste("DF must contain the column", date_col_name), call. = FALSE)
  }
  
  # Keep only valid tickers available in the dataframe
  tickers <- as.character(unlist(tickers, use.names = FALSE))
  tickers <- tickers[tickers %in% names(DF)]
  
  if (length(tickers) == 0) {
    stop("No valid tickers found in DF.", call. = FALSE)
  }
  
  # Convert the selected date column into Date format
  date_vector <- as.Date(DF[[date_col_name]])
  
  # Compute daily log realized variance separately for each ticker
  rv_list <- lapply(tickers, function(ticker) {
    
    # Keep only date and one-minute returns for the selected ticker
    stock_data <- data.frame(date = date_vector, return = DF[[ticker]])
    
    # Split one-minute returns by trading day
    returns_by_day <- split(stock_data$return, stock_data$date)
    
    # Compute daily realized variance for each trading day
    ticker_rv_list <- lapply(names(returns_by_day), function(day) {
      
      rv_out <- ComputeRVForInterval(
        returns_1min = returns_by_day[[day]],
        interval_minutes = interval_minutes,
        include_partial_last_block = include_partial_last_block
      )
      
      daily_rv <- rv_out$daily_rv
      n_blocks <- rv_out$n_blocks
      
      # Return NA when the daily RV observation is not valid
      if (!is.finite(daily_rv) ||
          daily_rv <= 0 ||
          n_blocks < minimum_blocks_required) {
        log_daily_rv <- NA_real_
      } else {
        log_daily_rv <- log(daily_rv)
      }
      
      data.frame(
        date = as.Date(day),
        ticker = ticker,
        log_daily_rv = log_daily_rv,
        stringsAsFactors = FALSE
      )
    })
    
    ticker_rv_panel <- do.call(rbind, ticker_rv_list)
    
    return(ticker_rv_panel)
  })
  
  # Combine all ticker-level daily log RV panels
  daily_rv_long <- do.call(rbind, rv_list)
  rownames(daily_rv_long) <- NULL
  
  # Reshape the dataframe from long to wide format
  daily_rv_wide <- stats::reshape(
    daily_rv_long,
    idvar = "date",
    timevar = "ticker",
    direction = "wide"
  )
  
  # Clean column names
  names(daily_rv_wide) <- gsub("log_daily_rv\\.", "", names(daily_rv_wide))
  
  # Order rows by date
  daily_rv_wide <- daily_rv_wide[order(daily_rv_wide$date), ]
  rownames(daily_rv_wide) <- NULL
  
  return(daily_rv_wide)
}