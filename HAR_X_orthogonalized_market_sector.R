



#___________BUILD_ORTHOGONALIZED_HAR_X_DESIGN_FROM_WINDOW_______________________

# This helper function builds the HAR-X regression design for one orthogonalized
# rolling window. Starting from the component dataframe produced by the relative
# decomposition, it constructs daily, weekly and monthly HAR components for the
# market component, the orthogonal sector component and the stock-specific
# relative component. The function does not estimate the model. It only returns
# the training dataframe and the current predictor row needed for a one-step
# ahead forecast.

BuildOrthogonalizedHARXDesignFromWindow <- function(component_data,
                                                    first_lag = 1,
                                                    second_lag = 5,
                                                    third_lag = 22,
                                                    minimum_har_observations) {
  
  component_data <- as.data.frame(component_data)
  
  # Check that the required columns exist
  required_cols <- c("date", "y_stock", "y_market", "s_perp", "q")
  
  if (!all(required_cols %in% names(component_data))) {
    stop(
      paste0(
        "component_data must contain date, y_stock, y_market, ",
        "s_perp and q."
      ),
      call. = FALSE
    )
  }
  
  # Check that the rolling mean helper function is available
  if (!exists("FastRollingMeanAllFinite")) {
    stop(
      paste0(
        "FastRollingMeanAllFinite() is not available. ",
        "Please source relative_HAR.R before calling this function."
      ),
      call. = FALSE
    )
  }
  
  # Check that lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  # Order observations by date
  component_data$date <- as.Date(component_data$date)
  component_data <- component_data[order(component_data$date), ]
  rownames(component_data) <- NULL
  
  # Extract dates and orthogonalized component series
  dates <- component_data$date
  y_stock <- as.numeric(component_data$y_stock)
  y_market <- as.numeric(component_data$y_market)
  s_perp <- as.numeric(component_data$s_perp)
  q <- as.numeric(component_data$q)
  
  # Define the maximum lag needed to build all HAR components
  max_lag <- max(lags)
  n <- length(dates)
  
  # Check that there are enough observations to build HAR-X variables and a
  # current one-step-ahead predictor row
  if (n < (max_lag + 1)) {
    return(NULL)
  }
  
  # Build market HAR components
  market_daily <- FastRollingMeanAllFinite(
    series = y_market,
    window_length = first_lag
  )
  
  market_weekly <- FastRollingMeanAllFinite(
    series = y_market,
    window_length = second_lag
  )
  
  market_monthly <- FastRollingMeanAllFinite(
    series = y_market,
    window_length = third_lag
  )
  
  # Build orthogonal sector HAR components
  sector_perp_daily <- FastRollingMeanAllFinite(
    series = s_perp,
    window_length = first_lag
  )
  
  sector_perp_weekly <- FastRollingMeanAllFinite(
    series = s_perp,
    window_length = second_lag
  )
  
  sector_perp_monthly <- FastRollingMeanAllFinite(
    series = s_perp,
    window_length = third_lag
  )
  
  # Build stock-specific relative HAR components
  q_daily <- FastRollingMeanAllFinite(
    series = q,
    window_length = first_lag
  )
  
  q_weekly <- FastRollingMeanAllFinite(
    series = q,
    window_length = second_lag
  )
  
  q_monthly <- FastRollingMeanAllFinite(
    series = q,
    window_length = third_lag
  )
  
  # The training origins stop at n - 1 because their targets must be observed
  # inside the rolling window. The current origin is n and is used only to
  # produce the next-day forecast outside the window.
  origin_indices <- max_lag:(n - 1)
  
  # Build the training dataframe used to estimate the orthogonalized HAR-X model
  training_data <- data.frame(
    origin_date = dates[origin_indices],
    target_date = dates[origin_indices + 1],
    target = y_stock[origin_indices + 1],
    market_daily = market_daily[origin_indices],
    market_weekly = market_weekly[origin_indices],
    market_monthly = market_monthly[origin_indices],
    sector_perp_daily = sector_perp_daily[origin_indices],
    sector_perp_weekly = sector_perp_weekly[origin_indices],
    sector_perp_monthly = sector_perp_monthly[origin_indices],
    q_daily = q_daily[origin_indices],
    q_weekly = q_weekly[origin_indices],
    q_monthly = q_monthly[origin_indices],
    stringsAsFactors = FALSE
  )
  
  # Keep only complete and finite training observations
  model_cols <- c(
    "target",
    "market_daily",
    "market_weekly",
    "market_monthly",
    "sector_perp_daily",
    "sector_perp_weekly",
    "sector_perp_monthly",
    "q_daily",
    "q_weekly",
    "q_monthly"
  )
  
  complete_rows <- Reduce(
    `&`,
    lapply(model_cols, function(column_name) {
      is.finite(training_data[[column_name]])
    })
  )
  
  training_data <- training_data[complete_rows, ]
  rownames(training_data) <- NULL
  
  # Skip windows with too few HAR-X observations
  if (nrow(training_data) < minimum_har_observations) {
    return(NULL)
  }
  
  # Build the current predictor row used for the one-step-ahead forecast
  current_predictors <- c(
    intercept = 1,
    market_daily = market_daily[n],
    market_weekly = market_weekly[n],
    market_monthly = market_monthly[n],
    sector_perp_daily = sector_perp_daily[n],
    sector_perp_weekly = sector_perp_weekly[n],
    sector_perp_monthly = sector_perp_monthly[n],
    q_daily = q_daily[n],
    q_weekly = q_weekly[n],
    q_monthly = q_monthly[n]
  )
  
  if (!all(is.finite(current_predictors))) {
    return(NULL)
  }
  
  result <- list(
    training_data = training_data,
    current_predictors = current_predictors,
    current_origin_date = dates[n],
    n_har_x_obs = nrow(training_data)
  )
  
  return(result)
}



#_______________ROLLING_ORTHOGONALIZED_HAR_X_MODEL_ONE_TICKER___________________

# This function estimates a rolling orthogonalized HAR-X market-sector model
# for one selected ticker. For each rolling window, it decomposes stock log
# realized variance into market, orthogonal sector and stock-specific relative
# components. Then, it builds daily, weekly and monthly HAR components for
# these three orthogonalized series and estimates one multivariate OLS model.
# At each step, it produces a one-day-ahead forecast and records the
# corresponding forecast error.

RollingOrthogonalizedHARXForecastByTicker <- function(daily_log_rv_wide,
                                                      ticker,
                                                      sector,
                                                      sector_etf,
                                                      market_ticker,
                                                      training_window,
                                                      first_lag = 1,
                                                      second_lag = 5,
                                                      third_lag = 22,
                                                      minimum_observations = 30)
  {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.", call. = FALSE)
  }
  
  # Check that ticker names are valid strings
  ticker <- as.character(ticker)[1]
  sector <- as.character(sector)[1]
  sector_etf <- as.character(sector_etf)[1]
  market_ticker <- as.character(market_ticker)[1]
  
  if (is.na(ticker) || !nzchar(ticker)) {
    stop("ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  if (is.na(sector_etf) || !nzchar(sector_etf)) {
    stop("sector_etf must be a valid non-empty string.", call. = FALSE)
  }
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  if (is.na(sector) || !nzchar(sector)) {
    sector <- NA_character_
  }
  
  # Check that the required tickers exist
  required_tickers <- c(ticker, market_ticker, sector_etf)
  
  if (!all(required_tickers %in% names(daily_log_rv_wide))) {
    warning(paste("Skipping", ticker,
                  "- missing stock, market or sector data."))
    return(NULL)
  }
  
  # Check that the required helper functions are available
  if (!exists("FitRelativeDecompositionWindow")) {
    stop(
      paste0(
        "FitRelativeDecompositionWindow() is not available. ",
        "Please source relative_HAR.R before calling this function."
      ),
      call. = FALSE
    )
  }
  
  if (!exists("BuildOrthogonalizedHARXDesignFromWindow")) {
    stop(
      paste0(
        "BuildOrthogonalizedHARXDesignFromWindow() is not available. ",
        "Please source HAR_X_orthogonalized_market_sector.R correctly."
      ),
      call. = FALSE
    )
  }
  
  # Check that lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  # Check that the training window is valid
  if (!is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a positive integer.", call. = FALSE)
  }
  
  # Check that the minimum number of observations is valid
  if (!is.finite(minimum_observations) ||
      minimum_observations < 1 ||
      minimum_observations != floor(minimum_observations)) {
    stop("minimum_observations must be a positive integer.", call. = FALSE)
  }
  
  # Order observations by date
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Build the dataframe used by the orthogonalized HAR-X model
  model_panel <- data.frame(
    date = daily_log_rv_wide$date,
    y_stock = as.numeric(daily_log_rv_wide[[ticker]]),
    y_market = as.numeric(daily_log_rv_wide[[market_ticker]]),
    y_sector = as.numeric(daily_log_rv_wide[[sector_etf]]),
    stringsAsFactors = FALSE
  )
  
  # Keep only complete and finite observations before defining rolling windows
  model_panel <- model_panel[
    is.finite(model_panel$y_stock) &
      is.finite(model_panel$y_market) &
      is.finite(model_panel$y_sector),
  ]
  
  rownames(model_panel) <- NULL
  
  # Define the raw rolling window length.
  # With default HAR lags 1, 5 and 22, a window of training_window HAR rows
  # requires training_window + 22 raw daily observations.
  max_lag <- max(lags)
  raw_window_length <- training_window + max_lag
  
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
    decomposition_coeffs <- decomposition$coefficients
    
    # Build the orthogonalized HAR-X training data and current predictors
    orthogonalized_design <- BuildOrthogonalizedHARXDesignFromWindow(
      component_data = component_data,
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    if (is.null(orthogonalized_design)) {
      return(NULL)
    }
    
    training_data <- orthogonalized_design$training_data
    
    # Pre-build the orthogonalized HAR-X design matrix
    x_matrix <- cbind(
      intercept = 1,
      market_daily = training_data$market_daily,
      market_weekly = training_data$market_weekly,
      market_monthly = training_data$market_monthly,
      sector_perp_daily = training_data$sector_perp_daily,
      sector_perp_weekly = training_data$sector_perp_weekly,
      sector_perp_monthly = training_data$sector_perp_monthly,
      q_daily = training_data$q_daily,
      q_weekly = training_data$q_weekly,
      q_monthly = training_data$q_monthly
    )
    
    y_vector <- training_data$target
    
    # Fit the orthogonalized HAR-X model on the rolling training window
    orthogonalized_har_x_model <- tryCatch(
      stats::lm.fit(
        x = x_matrix,
        y = y_vector
      ),
      error = function(e) NULL
    )
    
    if (is.null(orthogonalized_har_x_model) ||
        is.null(orthogonalized_har_x_model$coefficients)) {
      return(NULL)
    }
    
    # Use the estimated coefficients to produce the one-step-ahead forecast.
    # The pivot is used for safety in case the design matrix is rank-deficient.
    rank <- orthogonalized_har_x_model$rank
    
    if (is.null(rank) || rank < 1) {
      return(NULL)
    }
    
    pivot <- orthogonalized_har_x_model$qr$pivot[seq_len(rank)]
    model_coefficients <- orthogonalized_har_x_model$coefficients[pivot]
    
    if (!all(is.finite(model_coefficients))) {
      return(NULL)
    }
    
    current_predictors <- orthogonalized_design$current_predictors
    
    if (!all(colnames(x_matrix) %in% names(current_predictors))) {
      return(NULL)
    }
    
    current_predictors <- current_predictors[colnames(x_matrix)]
    
    if (!all(is.finite(current_predictors))) {
      return(NULL)
    }
    
    forecast_value <- as.numeric(sum(
      current_predictors[pivot] * model_coefficients
    ))
    
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
      alpha_hat = decomposition_coeffs$alpha_hat,
      beta_market_hat = decomposition_coeffs$beta_market_hat,
      beta_sector_hat = decomposition_coeffs$beta_sector_hat,
      sector_intercept_hat = decomposition_coeffs$sector_intercept_hat,
      sector_beta_market_hat = decomposition_coeffs$sector_beta_market_hat,
      n_decomposition_obs = decomposition_coeffs$n_obs,
      n_har_x_obs = orthogonalized_design$n_har_x_obs,
      stringsAsFactors = FALSE
    )
  })
  
  # Remove skipped forecast origins
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    warning(paste("Skipping", ticker,
                  "- no valid orthogonalized HAR-X forecasts."))
    return(NULL)
  }
  
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



#_________________ROLLING_ORTHOGONALIZED_HAR_X_FORECAST_PANEL___________________

# This function estimates rolling orthogonalized HAR-X market-sector forecasts
# for all eligible tickers. It applies
# RollingOrthogonalizedHARXForecastByTicker() stock by stock and combines the
# ticker-level forecast-error dataframes into one panel.

RollingOrthogonalizedHARXForecastPanel <- function(daily_log_rv_wide,
                                                   relative_har_tickers,
                                                   training_window,
                                                   market_ticker,
                                                   first_lag = 1,
                                                   second_lag = 5,
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
  
  # Check that the ticker-level forecasting function is available
  if (!exists("RollingOrthogonalizedHARXForecastByTicker")) {
    stop(
      paste0(
        "RollingOrthogonalizedHARXForecastByTicker() is not available. ",
        "Please source HAR_X_orthogonalized_market_sector.R correctly."
      ),
      call. = FALSE
    )
  }
  
  # Check that the market ticker is valid
  market_ticker <- as.character(market_ticker)[1]
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  if (!market_ticker %in% names(daily_log_rv_wide)) {
    stop(paste("Market ticker", market_ticker,
               "not found in daily_log_rv_wide."),
         call. = FALSE)
  }
  
  # Check that lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  # Check that the training window is valid
  if (!is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a positive integer.", call. = FALSE)
  }
  
  # Check that the minimum number of observations is valid
  if (!is.finite(minimum_observations) ||
      minimum_observations < 1 ||
      minimum_observations != floor(minimum_observations)) {
    stop("minimum_observations must be a positive integer.", call. = FALSE)
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
    stop(
      paste0(
        "No valid ticker-sector ETF pair found for the orthogonalized ",
        "HAR-X market-sector model."
      ),
      call. = FALSE
    )
  }
  
  # Remove duplicated ticker-sector ETF pairs, if any
  relative_har_tickers <- relative_har_tickers[
    !duplicated(relative_har_tickers[, c("ticker", "sector_etf")]),
  ]
  
  rownames(relative_har_tickers) <- NULL
  
  # Estimate rolling orthogonalized HAR-X forecasts separately for each ticker.
  # Progress is shown by default using progressr, as defined in setup.R.
  forecast_list <- progressr::with_progress({
    
    p <- progressr::progressor(steps = nrow(relative_har_tickers))
    
    future.apply::future_lapply(
      seq_len(nrow(relative_har_tickers)),
      function(row_index) {
        
        ticker_info <- relative_har_tickers[row_index, ]
        
        ticker <- ticker_info$ticker
        sector <- ticker_info$sector
        sector_etf <- ticker_info$sector_etf
        
        # Build and estimate the orthogonalized HAR-X model for the selected
        # ticker. tryCatch avoids stopping the full panel estimation if one
        # ticker fails.
        forecast_errors <- tryCatch({
          
          RollingOrthogonalizedHARXForecastByTicker(
            daily_log_rv_wide = daily_log_rv_wide,
            ticker = ticker,
            sector = sector,
            sector_etf = sector_etf,
            market_ticker = market_ticker,
            training_window = training_window,
            first_lag = first_lag,
            second_lag = second_lag,
            third_lag = third_lag,
            minimum_observations = minimum_observations
          )
          
        }, error = function(e) {
          
          warning(paste("Skipping", ticker, "-", conditionMessage(e)))
          NULL
        })
        
        p(sprintf("ticker %s", ticker))
        
        return(forecast_errors)
      }
    )
  })
  
  # Remove skipped tickers
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    stop(
      paste0(
        "No ticker produced valid rolling orthogonalized HAR-X ",
        "market-sector forecasts."
      ),
      call. = FALSE
    )
  }
  
  # Combine all ticker-level forecast-error dataframes
  forecast_errors_panel <- data.table::rbindlist(
    forecast_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecast_errors_panel <- as.data.frame(forecast_errors_panel)
  
  # Order the final panel by ticker and target date
  forecast_errors_panel$forecast_origin_date <- as.Date(
    forecast_errors_panel$forecast_origin_date
  )
  
  forecast_errors_panel$target_date <- as.Date(
    forecast_errors_panel$target_date
  )
  
  forecast_errors_panel <- forecast_errors_panel[
    order(forecast_errors_panel$ticker, forecast_errors_panel$target_date),
  ]
  
  rownames(forecast_errors_panel) <- NULL
  
  return(forecast_errors_panel)
}



#_____________________________END_OF_THE_SCRIPT_________________________________