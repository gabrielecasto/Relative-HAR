








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
  rv_list <- future.apply::future_lapply(tickers, function(ticker) {
    
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






# This function builds the HAR regression dataframe for one selected ticker.
# Starting from a wide daily log realized volatility dataframe, it constructs
# the daily, weekly and monthly HAR components by default (variations are
# allowed). The target variable is the next-day log realized volatility. The
# output is ready to be used in an OLS regression of target on daily, weekly
# and monthly components.

BuildHARDataForTicker <- function(daily_log_rv_wide, ticker, first_lag = 1,
                                  second_lag = 5, third_lag = 22) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.", call. = FALSE)
  }
  
  # Check that the selected ticker exists
  if (!ticker %in% names(daily_log_rv_wide)) {
    stop(paste("Ticker", ticker, "not found in daily_log_rv_wide."),
         call. = FALSE)
  }
  
  # Order observations by date
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Extract dates and the selected log daily RV series
  dates <- daily_log_rv_wide$date
  log_rv <- as.numeric(daily_log_rv_wide[[ticker]])
  
  # Check that there are enough observations to build the HAR variables
  if (length(log_rv) < (third_lag + 1)) {
    stop("Not enough observations to build HAR data.",
         call. = FALSE)
  }
  
  # Define the valid time indices
  # The first usable origin is day third_lag. The last usable origin is the day
  # before the last observation because the target is one day ahead.
  first_origin_index <- third_lag
  last_origin_index <- length(log_rv) - 1
  
  # Build HAR regressors for each valid forecasting origin
  har_list <- lapply(first_origin_index:last_origin_index,
                     function(time_index) {
    
    # Build the second component using the current day and the previous
    # (first_lag - 1) days
    daily_window <- log_rv[(time_index - (first_lag-1)):time_index]
    
    if (all(is.finite(daily_window))) {
      daily_component <- mean(daily_window)
    } else {
      daily_component <- NA_real_
    }
    
    # Build the second component using the current day and the previous
    # (second_lag - 1) days
    weekly_window <- log_rv[(time_index - (second_lag-1)):time_index]
    
    if (all(is.finite(weekly_window))) {
      weekly_component <- mean(weekly_window)
    } else {
      weekly_component <- NA_real_
    }
    
    # Build the weekly component using the current day and the previous
    # (third_lag - 1) days
    monthly_window <- log_rv[(time_index - (third_lag-1)):time_index]
    
    if (all(is.finite(monthly_window))) {
      monthly_component <- mean(monthly_window)
    } else {
      monthly_component <- NA_real_
    }
    
    # Define the one-day-ahead target
    target_value <- log_rv[time_index + 1]
    
    data.frame(
      ticker = ticker,
      origin_date = dates[time_index],
      target_date = dates[time_index + 1],
      target = target_value,
      daily = daily_component,
      weekly = weekly_component,
      monthly = monthly_component,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all HAR rows
  har_data <- do.call(rbind, har_list)
  rownames(har_data) <- NULL
  
  # Keep only complete and finite HAR observations
  har_data <- har_data[
    is.finite(har_data$target) &
      is.finite(har_data$daily) &
      is.finite(har_data$weekly) &
      is.finite(har_data$monthly),
  ]
  
  rownames(har_data) <- NULL
  
  return(har_data)
}








# This function estimates a rolling HAR model for one selected ticker.
# Starting from a ticker-level HAR dataframe, it uses a fixed rolling window
# to estimate the model target ~ daily + weekly + monthly. At each step, it
# produces a one-day-ahead forecast and records the corresponding forecast
# error. The output has one row for each out-of-sample forecast date.

RollingHARForecastByTicker <- function(har_data, training_window = 100) {
  
  har_data <- as.data.frame(har_data)
  
  # Check that the required columns exist
  required_cols <- c(
    "ticker",
    "origin_date",
    "target_date",
    "target",
    "daily",
    "weekly",
    "monthly"
  )
  
  if (!all(required_cols %in% names(har_data))) {
    stop(paste0("har_data must contain ticker, origin_date, target_date, ",
                "target, daily, weekly and monthly."), call. = FALSE)
  }
  
  # Order observations by origin date
  har_data$origin_date <- as.Date(har_data$origin_date)
  har_data$target_date <- as.Date(har_data$target_date)
  har_data <- har_data[order(har_data$origin_date), ]
  rownames(har_data) <- NULL
  
  # Keep only complete and finite observations
  har_data <- har_data[
    is.finite(har_data$target) &
      is.finite(har_data$daily) &
      is.finite(har_data$weekly) &
      is.finite(har_data$monthly),
  ]
  
  rownames(har_data) <- NULL
  
  # Check that there are enough observations for rolling estimation
  if (nrow(har_data) <= training_window) {
    stop("Not enough HAR observations for the selected training window.",
         call. = FALSE)
  }
  
  # Define the ticker name
  ticker <- unique(har_data$ticker)
  
  if (length(ticker) != 1) {
    stop("har_data must refer to one ticker only.", call. = FALSE)
  }
  
  # Define the out-of-sample test indices
  test_indices <- (training_window + 1):nrow(har_data)
  
  # Estimate the rolling HAR model and store one-step-ahead forecast errors
  forecast_list <- lapply(test_indices, function(test_index) {
    
    # Select the rolling training window
    train_start <- test_index - training_window
    train_end <- test_index - 1
    
    train_data <- har_data[train_start:train_end, ]
    test_data <- har_data[test_index, ]
    
    # Fit the HAR model on the rolling training window
    har_model <- stats::lm(
      target ~ daily + weekly + monthly,
      data = train_data
    )
    
    # Produce the one-step-ahead forecast
    forecast_value <- as.numeric(stats::predict(
      har_model,
      newdata = test_data
    ))
    
    # Extract the realized value
    actual_value <- test_data$target
    
    # Compute forecast errors
    error_value <- actual_value - forecast_value
    squared_error <- error_value^2
    absolute_error <- abs(error_value)
    
    # Compute QLIKE using log realized variance values
    qlike <- exp(actual_value - forecast_value) -
      (actual_value - forecast_value) - 1
    
    data.frame(
      ticker = ticker,
      forecast_origin_date = test_data$origin_date,
      target_date = test_data$target_date,
      actual = actual_value,
      forecast = forecast_value,
      error = error_value,
      squared_error = squared_error,
      absolute_error = absolute_error,
      qlike = qlike,
      stringsAsFactors = FALSE
    )
  })
  
  # Combine all forecast-error rows
  forecast_errors <- do.call(rbind, forecast_list)
  rownames(forecast_errors) <- NULL
  
  return(forecast_errors)
}




# This function estimates rolling HAR models for all selected tickers.
# Starting from a wide daily log realized volatility dataframe, it builds the
# ticker-level HAR regression data, estimates a rolling HAR model for each
# ticker, and stores the one-day-ahead forecast errors. The final output has
# one row for each ticker and out-of-sample forecast date.

RollingHARForecastPanel <- function(daily_log_rv_wide,
                                    tickers,
                                    training_window = 100,
                                    first_lag = 1,
                                    second_lag = 5,
                                    third_lag = 22) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.", call. = FALSE)
  }
  
  # Keep only valid tickers available in the dataframe
  tickers <- as.character(unlist(tickers, use.names = FALSE))
  tickers <- tickers[tickers %in% names(daily_log_rv_wide)]
  
  if (length(tickers) == 0) {
    stop("No valid tickers found in daily_log_rv_wide.", call. = FALSE)
  }
  
  # Estimate rolling HAR forecasts separately for each ticker
  forecast_list <- future.apply::future_lapply(tickers, function(ticker) {
    
    # Build the HAR regression dataframe for the selected ticker
    har_data <- BuildHARDataForTicker(
      daily_log_rv_wide = daily_log_rv_wide,
      ticker = ticker,
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag
    )
    
    # Skip tickers with too few observations for the selected training window
    if (nrow(har_data) <= training_window) {
      warning(paste("Skipping", ticker, "- not enough HAR observations."))
      return(NULL)
    }
    
    # Estimate rolling HAR forecasts for the selected ticker
    forecast_errors <- RollingHARForecastByTicker(
      har_data = har_data,
      training_window = training_window
    )
    
    return(forecast_errors)
  })
  
  # Remove skipped tickers
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    stop("No ticker produced valid rolling HAR forecasts.", call. = FALSE)
  }
  
  # Combine all ticker-level forecast-error dataframes
  forecast_errors_panel <- do.call(rbind, forecast_list)
  rownames(forecast_errors_panel) <- NULL
  
  return(forecast_errors_panel)
}