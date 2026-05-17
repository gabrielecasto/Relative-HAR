


# In this section we build and estimate rolling HAR models for realized
# volatility forecasting. Starting from one-minute intraday log returns, we
# first construct daily log realized variance series for each stock. Then, we
# build HAR regressors using daily, weekly and monthly variance components,
# estimate rolling one-step-ahead HAR forecasts, and store the corresponding
# forecast errors. Lags to determine the three regressors are 1,5,22 by default
# but can be modified.



#______________________________DAILY_RV_PANEL___________________________________

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
  
  # Check that interval_minutes is a valid positive integer
  if (!is.finite(interval_minutes) ||
      interval_minutes < 1 ||
      interval_minutes != floor(interval_minutes)) {
    stop("interval_minutes must be a positive integer.", call. = FALSE)
  }
  
  # Check that minimum_blocks_required is valid
  if (!is.finite(minimum_blocks_required) ||
      minimum_blocks_required < 1 ||
      minimum_blocks_required != floor(minimum_blocks_required)) {
    stop("minimum_blocks_required must be a positive integer.",
         call. = FALSE)
  }
  
  interval_minutes <- as.integer(interval_minutes)
  minimum_blocks_required <- as.integer(minimum_blocks_required)
  
  # Convert the selected date column into Date format
  date_vector <- as.Date(DF[[date_col_name]])
  
  # Match the behavior of split(): rows with missing dates are not used
  valid_date_rows <- !is.na(date_vector)
  date_vector <- date_vector[valid_date_rows]
  
  # Extract all selected return columns once
  returns_matrix <- as.matrix(DF[valid_date_rows, tickers, drop = FALSE])
  storage.mode(returns_matrix) <- "double"
  
  # Split row indices by trading day only once
  day_index_list <- split(seq_along(date_vector), date_vector)
  day_dates <- as.Date(names(day_index_list))
  
  # Prepare output matrix: one row per day, one column per ticker
  log_rv_matrix <- matrix(
    NA_real_,
    nrow = length(day_index_list),
    ncol = length(tickers)
  )
  
  colnames(log_rv_matrix) <- tickers
  
  # Helper used only when missing patterns differ across tickers
  ComputeLogRVOneTicker <- function(returns_1min) {
    
    returns_1min <- returns_1min[is.finite(returns_1min)]
    
    if (length(returns_1min) == 0) {
      return(NA_real_)
    }
    
    if (!include_partial_last_block) {
      
      n_complete <- floor(length(returns_1min) / interval_minutes) *
        interval_minutes
      
      if (n_complete == 0) {
        return(NA_real_)
      }
      
      returns_1min <- returns_1min[seq_len(n_complete)]
      n_blocks <- n_complete / interval_minutes
      
      interval_returns <- colSums(
        matrix(returns_1min, nrow = interval_minutes)
      )
      
    } else {
      
      block_id <- ((seq_along(returns_1min) - 1L) %/% interval_minutes) + 1L
      
      interval_returns <- as.numeric(
        rowsum(
          matrix(returns_1min, ncol = 1),
          group = block_id,
          reorder = FALSE
        )
      )
      
      n_blocks <- length(interval_returns)
    }
    
    daily_rv <- sum(interval_returns^2)
    
    if (!is.finite(daily_rv) ||
        daily_rv <= 0 ||
        n_blocks < minimum_blocks_required) {
      return(NA_real_)
    }
    
    return(log(daily_rv))
  }
  
  # Compute daily log realized variance day by day
  for (day_position in seq_along(day_index_list)) {
    
    row_index <- day_index_list[[day_position]]
    day_matrix <- returns_matrix[row_index, , drop = FALSE]
    
    finite_matrix <- is.finite(day_matrix)
    reference_finite <- finite_matrix[, 1]
    
    # Fast path: all tickers have the same finite/missing intraday pattern.
    # This should usually hold after your cleaning, except for unusual cases.
    common_missing_pattern <- all(finite_matrix == reference_finite)
    
    if (common_missing_pattern) {
      
      clean_day_matrix <- day_matrix[reference_finite, , drop = FALSE]
      n_obs <- nrow(clean_day_matrix)
      
      if (n_obs == 0) {
        next
      }
      
      if (!include_partial_last_block) {
        
        n_complete <- floor(n_obs / interval_minutes) * interval_minutes
        
        if (n_complete == 0) {
          next
        }
        
        clean_day_matrix <- clean_day_matrix[seq_len(n_complete), ,
                                             drop = FALSE]
        n_blocks <- n_complete / interval_minutes
        
        block_id <- rep(seq_len(n_blocks), each = interval_minutes)
        
      } else {
        
        n_blocks <- ceiling(n_obs / interval_minutes)
        block_id <- ((seq_len(n_obs) - 1L) %/% interval_minutes) + 1L
      }
      
      block_returns <- rowsum(
        clean_day_matrix,
        group = block_id,
        reorder = FALSE
      )
      
      daily_rv <- colSums(block_returns^2)
      
      valid_rv <- is.finite(daily_rv) &
        daily_rv > 0 &
        n_blocks >= minimum_blocks_required
      
      log_rv_matrix[day_position, valid_rv] <- log(daily_rv[valid_rv])
      
    } else {
      
      # Exact fallback: if tickers have different missing patterns, compute
      # each ticker separately using the same logic as the original function.
      for (ticker_position in seq_along(tickers)) {
        
        log_rv_matrix[day_position, ticker_position] <-
          ComputeLogRVOneTicker(day_matrix[, ticker_position])
      }
    }
  }
  
  # Build the final wide dataframe directly
  daily_rv_wide <- data.frame(
    date = day_dates,
    as.data.frame(log_rv_matrix, check.names = FALSE),
    check.names = FALSE
  )
  
  # Order rows by date
  daily_rv_wide <- daily_rv_wide[order(daily_rv_wide$date), ]
  rownames(daily_rv_wide) <- NULL
  
  return(daily_rv_wide)
}



#__________________BUILD_HAR_REGRESSORS_DATAFRAME_ONE_TICKER____________________

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
  
  # Check that lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  # Define the maximum lag needed to build all HAR components
  max_lag <- max(lags)
  
  # Check that there are enough observations to build the HAR variables
  if (length(log_rv) < (max_lag + 1)) {
    stop("Not enough observations to build HAR data.",
         call. = FALSE)
  }
  
  # Define the valid time indices
  # The first usable origin is the maximum lag. The last usable origin is the
  # day before the last observation because the target is one day ahead.
  first_origin_index <- max_lag
  last_origin_index <- length(log_rv) - 1
  
  # Build HAR regressors for each valid forecasting origin
  har_list <- lapply(first_origin_index:last_origin_index,
                     function(time_index) {
    
    # Build the first component using the current day and the previous
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
    
    # Build the third component using the current day and the previous
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



#________________________ROLLING_HAR_MODEL_ONE_TICKER___________________________

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
  
  # Pre-build the HAR design matrix once.
  x_matrix <- cbind(
    intercept = 1,
    daily = har_data$daily,
    weekly = har_data$weekly,
    monthly = har_data$monthly
  )
  
  y_vector <- har_data$target
  
  # Define the out-of-sample test indices
  test_indices <- (training_window + 1):nrow(har_data)
  
  # Estimate the rolling HAR model and store one-step-ahead forecast errors
  forecast_list <- lapply(test_indices, function(test_index) {
    
    # Select the rolling training window
    train_start <- test_index - training_window
    train_end <- test_index - 1
    
    train_rows <- train_start:train_end
    
    # Fit the HAR model on the rolling training window
    har_model <- stats::lm.fit(
      x = x_matrix[train_rows, , drop = FALSE],
      y = y_vector[train_rows]
    )
    
    # Use the estimated coefficients to produce the one-step-ahead forecast
    rank <- har_model$rank
    pivot <- har_model$qr$pivot[seq_len(rank)]
    
    forecast_value <- as.numeric(sum(
      x_matrix[test_index, pivot] * har_model$coefficients[pivot]
    ))
    
    # Extract the realized value
    actual_value <- y_vector[test_index]
    
    # Compute forecast errors
    error_value <- actual_value - forecast_value
    squared_error <- error_value^2
    absolute_error <- abs(error_value)
    
    # Compute QLIKE using log realized variance values
    qlike <- exp(actual_value - forecast_value) -
      (actual_value - forecast_value) - 1
    
    data.frame(
      ticker = ticker,
      forecast_origin_date = har_data$origin_date[test_index],
      target_date = har_data$target_date[test_index],
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
  forecast_errors <- data.table::rbindlist(
    forecast_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecast_errors <- as.data.frame(forecast_errors)
  rownames(forecast_errors) <- NULL
  
  return(forecast_errors)
}



#________________________ROLLING_HAR_MODEL_ALL_TICKERS__________________________

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
  
  # Estimate rolling HAR forecasts separately for each ticker.
  # Progress is shown by default using progressr, as defined in setup.R.
  forecast_list <- progressr::with_progress({
    
    p <- progressr::progressor(steps = length(tickers))
    
    future.apply::future_lapply(tickers, function(ticker) {
      
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
        p(sprintf("ticker %s", ticker))
        return(NULL)
      }
      
      # Estimate rolling HAR forecasts for the selected ticker
      forecast_errors <- RollingHARForecastByTicker(
        har_data = har_data,
        training_window = training_window
      )
      
      p(sprintf("ticker %s", ticker))
      
      return(forecast_errors)
    })
  })
  
  # Remove skipped tickers
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    stop("No ticker produced valid rolling HAR forecasts.", call. = FALSE)
  }
  
  # Combine all ticker-level forecast-error dataframes
  forecast_errors_panel <- data.table::rbindlist(
    forecast_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecast_errors_panel <- as.data.frame(forecast_errors_panel)
  rownames(forecast_errors_panel) <- NULL
  
  return(forecast_errors_panel)
}



#_____________________________END_OF_THE_SCRIPT_________________________________