


# In this section we build and estimate relative HAR models for realized
# volatility forecasting. Starting from daily log realized variance series,
# we decompose each stock volatility into market, sector-specific and relative
# components. Then, we forecast each component separately and reconstruct the
# one-step-ahead stock log realized variance forecast.



#______________________PREPARE_RELATIVE_HAR_TICKERS_____________________________

# This function prepares the list of stock tickers that can be used in the
# relative HAR model. A stock is kept only if its own daily log RV series, the
# market benchmark SPY, and its corresponding sector ETF are all available in
# the daily log RV panel.

PrepareRelativeHARTickers <- function(daily_log_rv_wide, date_col_name,
                                      ticker_sector_table, market_ticker) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  ticker_sector_table <- as.data.frame(ticker_sector_table)
  
  # Check that date column is in the data frame
  if (!date_col_name %in% names(daily_log_rv_wide)) {
    stop(paste("Data frame must contain the column", date_col_name),
         call. = FALSE)
    }
  
  # Check that columns names "ticker", "sector", "sector_etf" are in
  # ticker_sector_table
  required_cols <- c("ticker", "sector", "sector_etf")
  
  if (!all(required_cols %in% names(ticker_sector_table))) {
    stop("ticker_sector_table must contain ticker, sector and sector_etf.",
         call. = FALSE)
  }
  
  # Check that market ticker is in daily_log_rv_wide
  if (!market_ticker %in% names(daily_log_rv_wide)) {
    stop(paste("Market ticker", market_ticker,
               "not found in daily_log_rv_wide."), call. = FALSE)
    }
  
  
  available_columns <- names(daily_log_rv_wide)
  
  eligible_table <- ticker_sector_table[
    ticker_sector_table$ticker %in% available_columns &
      ticker_sector_table$sector_etf %in% available_columns &
      !is.na(ticker_sector_table$sector_etf),
  ]
  
  eligible_table$market_ticker <- market_ticker
  
  rownames(eligible_table) <- NULL
  
  return(eligible_table)
}



#_____________________FIT_RELATIVE_DECOMPOSITION_WINDOW_________________________

# This function estimates the relative volatility decomposition inside one
# rolling window. Starting from stock, market and sector log realized variance
# series, it first removes the market component from sector volatility. Then, it
# decomposes stock volatility into market volatility, orthogonal sector
# volatility and a relative stock-specific component.

FitRelativeDecompositionWindow <- function(window_data, minimum_observations) {
  
  window_data <- as.data.frame(window_data)
  
  # Check that the required columns exist
  required_cols <- c("date", "y_stock", "y_market", "y_sector")
  
  if (!all(required_cols %in% names(window_data))) {
    stop("window_data must contain date, y_stock, y_market and y_sector.",
         call. = FALSE)
  }
  
  # Order observations by date
  window_data$date <- as.Date(window_data$date)
  window_data <- window_data[order(window_data$date), ]
  rownames(window_data) <- NULL
  
  # Keep only complete and finite observations
  model_data <- window_data[
    is.finite(window_data$y_stock) &
      is.finite(window_data$y_market) &
      is.finite(window_data$y_sector),
  ]
  
  rownames(model_data) <- NULL
  
  # Skip windows with too few valid observations
  if (nrow(model_data) < minimum_observations) {
    return(NULL)
  }
  
  # First regression: remove the market component from sector volatility
  sector_model <- stats::lm(
    y_sector ~ y_market,
    data = model_data
  )
  
  # Orthogonal sector component
  model_data$s_perp <- as.numeric(stats::residuals(sector_model))
  
  # Second regression: decompose stock volatility into market, sector and
  # relative components
  stock_model <- stats::lm(
    y_stock ~ y_market + s_perp,
    data = model_data
  )
  
  # Extract fitted systematic component and relative component
  model_data$y_systematic <- as.numeric(stats::fitted(stock_model))
  model_data$q <- as.numeric(stats::residuals(stock_model))
  
  # Extract decomposition coefficients
  sector_coefs <- stats::coef(sector_model)
  stock_coefs <- stats::coef(stock_model)
  
  required_stock_coefs <- c("(Intercept)", "y_market", "s_perp")
  required_sector_coefs <- c("(Intercept)", "y_market")
  
  if (!all(required_stock_coefs %in% names(stock_coefs)) ||
      !all(required_sector_coefs %in% names(sector_coefs))) {
    return(NULL)
  }
  
  alpha_hat <- unname(stock_coefs["(Intercept)"])
  beta_market_hat <- unname(stock_coefs["y_market"])
  beta_sector_hat <- unname(stock_coefs["s_perp"])
  
  sector_intercept_hat <- unname(sector_coefs["(Intercept)"])
  sector_beta_market_hat <- unname(sector_coefs["y_market"])
  
  coefficient_values <- c(
    alpha_hat,
    beta_market_hat,
    beta_sector_hat,
    sector_intercept_hat,
    sector_beta_market_hat
  )
  
  if (!all(is.finite(coefficient_values))) {
    return(NULL)
  }
  
  coefficients <- data.frame(
    alpha_hat = alpha_hat,
    beta_market_hat = beta_market_hat,
    beta_sector_hat = beta_sector_hat,
    sector_intercept_hat = sector_intercept_hat,
    sector_beta_market_hat = sector_beta_market_hat,
    n_obs = nrow(model_data),
    stringsAsFactors = FALSE
  )
  
  result <- list(
    component_data = model_data,
    coefficients = coefficients
  )
  
  return(result)
}



#_______________________FORECAST_HAR_ONE_STEP_FROM_SERIES_______________________

# This function estimates a HAR model on one generic time series and produces
# one one-step-ahead forecast. It reuses BuildHARDataForTicker() from HAR.R by
# temporarily storing the input series as a dataframe column named "component".

ForecastHAROneStepFromSeries <- function(dates, series, component_name,
                                         first_lag = 1, second_lag = 5,
                                         third_lag = 22,
                                         minimum_har_observations) {
  
  dates <- as.Date(dates)
  series <- as.numeric(series)
  
  # Check that dates and series have the same length
  if (length(dates) != length(series)) {
    stop("dates and series must have the same length.", call. = FALSE)
  }
  
  # Check that lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  max_lag <- max(lags)
  
  # Create a temporary dataframe compatible with BuildHARDataForTicker()
  temp_data <- data.frame(
    date = dates,
    component = series,
    stringsAsFactors = FALSE
  )
  
  temp_data <- temp_data[order(temp_data$date), ]
  rownames(temp_data) <- NULL
  
  # Check that there are enough raw observations
  if (nrow(temp_data) < (max_lag + 2)) {
    return(NULL)
  }
  
  # Build HAR regression data using the existing HAR.R function
  har_data <- tryCatch(
    BuildHARDataForTicker(
      daily_log_rv_wide = temp_data,
      ticker = "component",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag
    ),
    error = function(e) NULL
  )
  
  if (is.null(har_data) || nrow(har_data) < minimum_har_observations) {
    return(NULL)
  }
  
  # Estimate the HAR model
  har_model <- stats::lm(
    target ~ daily + weekly + monthly,
    data = har_data
  )
  
  # Build current HAR predictors using the last available observation
  last_index <- nrow(temp_data)
  
  daily_current <- mean(
    temp_data$component[(last_index - (first_lag - 1)):last_index],
    na.rm = FALSE
  )
  
  weekly_current <- mean(
    temp_data$component[(last_index - (second_lag - 1)):last_index],
    na.rm = FALSE
  )
  
  monthly_current <- mean(
    temp_data$component[(last_index - (third_lag - 1)):last_index],
    na.rm = FALSE
  )
  
  current_values <- c(daily_current, weekly_current, monthly_current)
  
  if (!all(is.finite(current_values))) {
    return(NULL)
  }
  
  new_data <- data.frame(
    daily = daily_current,
    weekly = weekly_current,
    monthly = monthly_current
  )
  
  # Produce the one-step-ahead forecast
  forecast_value <- tryCatch(
    as.numeric(stats::predict(har_model, newdata = new_data)),
    error = function(e) NA_real_
  )
  
  if (!is.finite(forecast_value)) {
    return(NULL)
  }
  
  # Extract HAR coefficients
  har_coefs <- stats::coef(har_model)
  
  get_coef <- function(name) {
    if (name %in% names(har_coefs) && is.finite(har_coefs[name])) {
      return(unname(har_coefs[name]))
    } else {
      return(NA_real_)
    }
  }
  
  result <- list(
    component_name = component_name,
    forecast = forecast_value,
    intercept = get_coef("(Intercept)"),
    beta_daily = get_coef("daily"),
    beta_weekly = get_coef("weekly"),
    beta_monthly = get_coef("monthly"),
    n_obs = nrow(har_data)
  )
  
  return(result)
}



#____________________ROLLING_RELATIVE_HAR_ONE_TICKER____________________________

# This function estimates the relative HAR model for one selected stock.
# For each rolling window, it decomposes stock log realized variance into
# market, orthogonal sector and relative components. Then, it forecasts each
# component with a HAR model and reconstructs the one-step-ahead stock log
# realized variance forecast.

RollingRelativeHARForecastByTicker <- function(daily_log_rv_wide, ticker,
                                               sector, sector_etf,
                                               market_ticker, training_window,
                                               first_lag = 1, second_lag = 5,
                                               third_lag = 22,
                                               minimum_observations) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.", call. = FALSE)
  }
  
  # Check that the required tickers exist
  required_tickers <- c(ticker, market_ticker, sector_etf)
  
  if (!all(required_tickers %in% names(daily_log_rv_wide))) {
    warning(paste("Skipping", ticker, "- missing stock, market or sector data."))
    return(NULL)
  }
  
  # Order observations by date
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Build the dataframe used by the relative HAR model
  model_panel <- data.frame(
    date = daily_log_rv_wide$date,
    y_stock = as.numeric(daily_log_rv_wide[[ticker]]),
    y_market = as.numeric(daily_log_rv_wide[[market_ticker]]),
    y_sector = as.numeric(daily_log_rv_wide[[sector_etf]]),
    stringsAsFactors = FALSE
  )
  
  # Define the raw rolling window length.
  # With HAR lags 1, 5, 22, a window of training_window HAR rows requires
  # training_window + third_lag raw daily observations.
  raw_window_length <- training_window + third_lag
  
  if (nrow(model_panel) <= raw_window_length) {
    warning(paste("Skipping", ticker, "- not enough observations."))
    return(NULL)
  }
  
  # Define rolling forecast origins
  forecast_origin_indices <- raw_window_length:(nrow(model_panel) - 1)
  
  forecast_list <- lapply(forecast_origin_indices, function(origin_index) {
    
    # Select rolling window ending at the forecast origin
    window_start <- origin_index - raw_window_length + 1
    window_end <- origin_index
    
    window_data <- model_panel[window_start:window_end, ]
    
    # Actual next-day stock log realized variance
    actual_value <- model_panel$y_stock[origin_index + 1]
    
    if (!is.finite(actual_value)) {
      return(NULL)
    }
    
    # Estimate the relative decomposition inside the rolling window
    decomposition <- FitRelativeDecompositionWindow(
      window_data = window_data,
      minimum_observations = minimum_observations
    )
    
    if (is.null(decomposition)) {
      return(NULL)
    }
    
    component_data <- decomposition$component_data
    coefficients <- decomposition$coefficients
    
    # Forecast market log RV
    market_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$y_market,
      component_name = "market",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast orthogonal sector component
    sector_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$s_perp,
      component_name = "sector_perp",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast relative stock-specific component
    relative_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$q,
      component_name = "relative_q",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    if (is.null(market_har) || is.null(sector_har) || is.null(relative_har)) {
      return(NULL)
    }
    
    # Reconstruct the stock log realized variance forecast
    forecast_value <- coefficients$alpha_hat +
      coefficients$beta_market_hat * market_har$forecast +
      coefficients$beta_sector_hat * sector_har$forecast +
      relative_har$forecast
    
    if (!is.finite(forecast_value)) {
      return(NULL)
    }
    
    # Compute forecast errors
    error_value <- actual_value - forecast_value
    squared_error <- error_value^2
    absolute_error <- abs(error_value)
    
    # Compute QLIKE using log realized variance values
    qlike <- exp(actual_value - forecast_value) -
      (actual_value - forecast_value) - 1
    
    data.frame(
      ticker = ticker,
      sector = sector,
      sector_etf = sector_etf,
      market_ticker = market_ticker,
      forecast_origin_date = model_panel$date[origin_index],
      target_date = model_panel$date[origin_index + 1],
      actual = actual_value,
      forecast = forecast_value,
      error = error_value,
      squared_error = squared_error,
      absolute_error = absolute_error,
      qlike = qlike,
      market_forecast = market_har$forecast,
      sector_perp_forecast = sector_har$forecast,
      q_forecast = relative_har$forecast,
      alpha_hat = coefficients$alpha_hat,
      beta_market_hat = coefficients$beta_market_hat,
      beta_sector_hat = coefficients$beta_sector_hat,
      sector_intercept_hat = coefficients$sector_intercept_hat,
      sector_beta_market_hat = coefficients$sector_beta_market_hat,
      n_decomposition_obs = coefficients$n_obs,
      n_market_har_obs = market_har$n_obs,
      n_sector_har_obs = sector_har$n_obs,
      n_relative_har_obs = relative_har$n_obs,
      stringsAsFactors = FALSE
    )
  })
  
  # Remove skipped forecast origins
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    warning(paste("Skipping", ticker, "- no valid relative HAR forecasts."))
    return(NULL)
  }
  
  # Combine all forecast-error rows
  forecast_errors <- do.call(rbind, forecast_list)
  rownames(forecast_errors) <- NULL
  
  return(forecast_errors)
}



#____________________ROLLING_RELATIVE_HAR_FORECAST_PANEL________________________

# This function estimates rolling relative HAR forecasts for all eligible
# tickers. It applies RollingRelativeHARForecastByTicker() stock by stock and
# combines the ticker-level forecast-error dataframes into one panel.

RollingRelativeHARForecastPanel <- function(daily_log_rv_wide,
                                            relative_har_tickers,
                                            training_window, market_ticker,
                                            first_lag = 1, second_lag = 5,
                                            third_lag = 22,
                                            minimum_observations = 30) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  relative_har_tickers <- as.data.frame(relative_har_tickers)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.", call. = FALSE)
  }
  
  # Check that the relative HAR ticker table has the required columns
  required_cols <- c("ticker", "sector", "sector_etf")
  
  if (!all(required_cols %in% names(relative_har_tickers))) {
    stop("relative_har_tickers must contain ticker, sector and sector_etf.",
         call. = FALSE)
  }
  
  # Check that the market ticker exists
  if (!market_ticker %in% names(daily_log_rv_wide)) {
    stop(paste("Market ticker", market_ticker,
               "not found in daily_log_rv_wide."),
         call. = FALSE)
  }
  
  # Convert relevant columns to character
  relative_har_tickers$ticker <- as.character(relative_har_tickers$ticker)
  relative_har_tickers$sector <- as.character(relative_har_tickers$sector)
  relative_har_tickers$sector_etf <- 
    as.character(relative_har_tickers$sector_etf)
  
  # Keep only rows with stock and sector ETF available in the daily RV panel
  available_columns <- names(daily_log_rv_wide)
  
  valid_rows <- relative_har_tickers$ticker %in% available_columns &
    relative_har_tickers$sector_etf %in% available_columns &
    !is.na(relative_har_tickers$ticker) &
    !is.na(relative_har_tickers$sector_etf) &
    nzchar(relative_har_tickers$ticker) &
    nzchar(relative_har_tickers$sector_etf)
  
  relative_har_tickers <- relative_har_tickers[valid_rows, ]
  rownames(relative_har_tickers) <- NULL
  
  if (nrow(relative_har_tickers) == 0) {
    stop("No valid ticker-sector ETF pair found for the relative HAR model.",
         call. = FALSE)
  }
  
  # Remove duplicated ticker-sector ETF pairs, if any
  relative_har_tickers <- relative_har_tickers[
    !duplicated(relative_har_tickers[, c("ticker", "sector_etf")]),
  ]
  
  rownames(relative_har_tickers) <- NULL
  
  # Estimate rolling relative HAR forecasts separately for each ticker
  forecast_list <- future.apply::future_lapply(
    seq_len(nrow(relative_har_tickers)),
    function(row_index) {
      
      ticker_info <- relative_har_tickers[row_index, ]
      
      forecast_errors <- RollingRelativeHARForecastByTicker(
        daily_log_rv_wide = daily_log_rv_wide,
        ticker = ticker_info$ticker,
        sector = ticker_info$sector,
        sector_etf = ticker_info$sector_etf,
        market_ticker = market_ticker,
        training_window = training_window,
        first_lag = first_lag,
        second_lag = second_lag,
        third_lag = third_lag,
        minimum_observations = minimum_observations
      )
      
      return(forecast_errors)
    }
  )
  
  # Remove skipped tickers
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    stop("No ticker produced valid rolling relative HAR forecasts.",
         call. = FALSE)
  }
  
  # Combine all ticker-level forecast-error dataframes
  forecast_errors_panel <- do.call(rbind, forecast_list)
  rownames(forecast_errors_panel) <- NULL
  
  # Order final output by ticker and target date
  forecast_errors_panel <- forecast_errors_panel[
    order(forecast_errors_panel$ticker, forecast_errors_panel$target_date),
  ]
  
  rownames(forecast_errors_panel) <- NULL
  
  return(forecast_errors_panel)
}



#_____________________________END_OF_THE_SCRIPT_________________________________