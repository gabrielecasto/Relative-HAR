


# This section studies whether the components used by the relative HAR
# decomposition are individually forecastable. For each eligible stock and
# rolling window, we decompose stock log RV into market, orthogonal sector and
# relative q components. We estimate separate one-step-ahead HAR forecasts for
# stock log RV, market log RV, raw sector ETF log RV, sector-perp and
# relative q. Each forecast is compared with a rolling-mean benchmark based only
# on past observations, and forecastability is measured through out-of-sample
# R-squared.



#________________________COMPUTE_ROLLING_MEAN_BASELINE__________________________

# This function computes a rolling-mean benchmark forecast for one ordered
# numeric time series. The forecast for observation t is computed using only the
# previous training_window observations, so the function does not introduce
# look-ahead bias. A benchmark forecast is returned only when all observations
# inside the previous rolling window are finite.

ComputeRollingMeanBaseline <- function(series, training_window) {
  
  # Convert the input series to numeric format
  series <- as.numeric(series)
  
  # Check that training_window is a valid positive integer
  if (length(training_window) != 1 ||
      !is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a single positive integer.", call. = FALSE)
  }
  
  training_window <- as.integer(training_window)
  
  # Return an empty vector if the input series is empty
  if (length(series) == 0) {
    return(numeric(0))
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
  
  # Compute rolling means ending at each observation.
  # rolling_mean[t] uses observations from t - training_window + 1 to t.
  rolling_mean <- FastRollingMeanAllFinite(
    series = series,
    window_length = training_window
  )
  
  # Shift the rolling mean by one period.
  # baseline_forecast[t] uses only observations before t.
  baseline_forecast <- c(NA_real_, rolling_mean[-length(rolling_mean)])
  
  return(baseline_forecast)
}



#__________________BUILD_FORECASTABILITY_PANEL_ONE_TICKER_______________________

# This function builds a component-level forecastability panel for one selected
# ticker. For each rolling window, it estimates the relative volatility
# decomposition using only in-sample observations, forecasts stock log RV,
# market log RV, raw sector ETF log RV, orthogonal sector volatility and the
# relative q component with HAR models, and stores actual values, HAR forecasts
# and rolling-mean benchmark forecasts for the next out-of-sample observation.

BuildForecastabilityPanelOneTicker <- function(daily_log_rv_wide,
                                               ticker,
                                               sector,
                                               sector_etf,
                                               market_ticker,
                                               training_window,
                                               first_lag = 1,
                                               second_lag = 5,
                                               third_lag = 22,
                                               minimum_observations = 30) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the required date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.",
         call. = FALSE)
  }
  
  # Standardize ticker identifiers
  ticker <- as.character(ticker)[1]
  sector <- as.character(sector)[1]
  sector_etf <- as.character(sector_etf)[1]
  market_ticker <- as.character(market_ticker)[1]
  
  # Check that ticker names are valid
  if (is.na(ticker) || !nzchar(ticker)) {
    stop("ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  if (is.na(sector_etf) || !nzchar(sector_etf)) {
    stop("sector_etf must be a valid non-empty string.", call. = FALSE)
  }
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  # Check that the required tickers are available
  required_tickers <- c(ticker, market_ticker, sector_etf)
  
  if (!all(required_tickers %in% names(daily_log_rv_wide))) {
    warning(paste("Skipping", ticker,
                  "- missing stock, market or sector ETF data."))
    return(NULL)
  }
  
  # Check that the required helper functions are available
  required_functions <- c(
    "FitRelativeDecompositionWindow",
    "ForecastHAROneStepFromSeries",
    "ComputeRollingMeanBaseline"
  )
  
  missing_functions <- required_functions[
    !sapply(required_functions, exists)
  ]
  
  if (length(missing_functions) > 0) {
    stop(
      paste0(
        "Missing required functions: ",
        paste(missing_functions, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Check that training_window and lags are valid positive integers
  lags <- c(first_lag, second_lag, third_lag)
  
  if (length(training_window) != 1 ||
      !is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a single positive integer.",
         call. = FALSE)
  }
  
  if (any(!is.finite(lags)) || any(lags < 1) || any(lags != floor(lags))) {
    stop("first_lag, second_lag and third_lag must be positive integers.",
         call. = FALSE)
  }
  
  training_window <- as.integer(training_window)
  max_lag <- max(lags)
  
  # Order observations by date
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Build the ticker-level dataframe used for the diagnostic
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
  # A HAR estimation window with training_window usable HAR rows requires
  # training_window + max_lag raw daily observations.
  raw_window_length <- training_window + max_lag
  
  if (nrow(model_panel) <= raw_window_length) {
    warning(paste("Skipping", ticker, "- not enough observations."))
    return(NULL)
  }
  
  # Internal helper: compute the out-of-sample rolling-mean benchmark forecast.
  # The target value is appended only to align the forecast index; because the
  # rolling mean is shifted by one period, the target is not used in the
  # benchmark forecast.
  GetBaselineForecast <- function(series_window, target_value) {
    
    baseline_series <- ComputeRollingMeanBaseline(
      series = c(series_window, target_value),
      training_window = training_window
    )
    
    baseline_value <- baseline_series[length(baseline_series)]
    
    return(as.numeric(baseline_value))
  }
  
  # Internal helper: build one component-level forecast row
  BuildComponentRow <- function(component_name, actual_value, forecast_value,
                                baseline_value, forecast_origin_date,
                                target_date) {
    
    values_to_check <- c(actual_value, forecast_value, baseline_value)
    
    if (!all(is.finite(values_to_check))) {
      return(NULL)
    }
    
    error_har <- actual_value - forecast_value
    error_baseline <- actual_value - baseline_value
    
    data.frame(
      training_window = training_window,
      component = component_name,
      ticker = ticker,
      sector = sector,
      sector_etf = sector_etf,
      market_ticker = market_ticker,
      forecast_origin_date = forecast_origin_date,
      target_date = target_date,
      actual = actual_value,
      forecast = forecast_value,
      baseline_forecast = baseline_value,
      squared_error_har = error_har^2,
      squared_error_baseline = error_baseline^2,
      stringsAsFactors = FALSE
    )
  }
  
  # Define rolling forecast origins
  forecast_origin_indices <- raw_window_length:(nrow(model_panel) - 1)
  
  forecast_list <- lapply(forecast_origin_indices, function(origin_index) {
    
    # Select rolling window ending at the forecast origin
    window_start <- origin_index - raw_window_length + 1
    window_end <- origin_index
    
    window_data <- model_panel[window_start:window_end, ]
    
    # Extract out-of-sample target values
    stock_actual <- model_panel$y_stock[origin_index + 1]
    market_actual <- model_panel$y_market[origin_index + 1]
    sector_actual_raw <- model_panel$y_sector[origin_index + 1]
    
    if (!all(is.finite(c(stock_actual, market_actual, sector_actual_raw)))) {
      return(NULL)
    }
    
    # Estimate the relative decomposition using in-sample observations
    decomposition <- FitRelativeDecompositionWindow(
      window_data = window_data,
      minimum_observations = minimum_observations
    )
    
    if (is.null(decomposition)) {
      return(NULL)
    }
    
    component_data <- decomposition$component_data
    coefficients <- decomposition$coefficients
    
    # Forecast stock log RV with a direct HAR model
    stock_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$y_stock,
      component_name = "stock_log_rv",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast market log RV with a HAR model
    market_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$y_market,
      component_name = "market_log_rv",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast raw sector ETF log RV with a HAR model
    sector_etf_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$y_sector,
      component_name = "sector_etf_log_rv",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast orthogonal sector component with a HAR model
    sector_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$s_perp,
      component_name = "sector_perp",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    # Forecast relative stock-specific component with a HAR model
    relative_har <- ForecastHAROneStepFromSeries(
      dates = component_data$date,
      series = component_data$q,
      component_name = "relative_q",
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_har_observations = training_window
    )
    
    if (is.null(stock_har) ||
        is.null(market_har) ||
        is.null(sector_etf_har) ||
        is.null(sector_har) ||
        is.null(relative_har)) {
      return(NULL)
    }
    
    # Compute out-of-sample actual sector-perp using only in-sample coefficients
    sector_perp_actual <- sector_actual_raw -
      (coefficients$sector_intercept_hat +
         coefficients$sector_beta_market_hat * market_actual)
    
    # Compute out-of-sample actual q using only in-sample coefficients
    q_actual <- stock_actual -
      (coefficients$alpha_hat +
         coefficients$beta_market_hat * market_actual +
         coefficients$beta_sector_hat * sector_perp_actual)
    
    if (!all(is.finite(c(sector_perp_actual, q_actual)))) {
      return(NULL)
    }
    
    # Compute rolling-mean benchmark forecasts for each component
    stock_baseline <- GetBaselineForecast(
      series_window = component_data$y_stock,
      target_value = stock_actual
    )
    
    market_baseline <- GetBaselineForecast(
      series_window = component_data$y_market,
      target_value = market_actual
    )
    
    sector_etf_baseline <- GetBaselineForecast(
      series_window = component_data$y_sector,
      target_value = sector_actual_raw
    )
    
    sector_baseline <- GetBaselineForecast(
      series_window = component_data$s_perp,
      target_value = sector_perp_actual
    )
    
    q_baseline <- GetBaselineForecast(
      series_window = component_data$q,
      target_value = q_actual
    )
    
    forecast_origin_date <- model_panel$date[origin_index]
    target_date <- model_panel$date[origin_index + 1]
    
    # Build one row for each forecasted component
    component_rows <- list(
      BuildComponentRow(
        component_name = "stock_log_rv",
        actual_value = stock_actual,
        forecast_value = stock_har$forecast,
        baseline_value = stock_baseline,
        forecast_origin_date = forecast_origin_date,
        target_date = target_date
      ),
      BuildComponentRow(
        component_name = "market_log_rv",
        actual_value = market_actual,
        forecast_value = market_har$forecast,
        baseline_value = market_baseline,
        forecast_origin_date = forecast_origin_date,
        target_date = target_date
      ),
      BuildComponentRow(
        component_name = "sector_etf_log_rv",
        actual_value = sector_actual_raw,
        forecast_value = sector_etf_har$forecast,
        baseline_value = sector_etf_baseline,
        forecast_origin_date = forecast_origin_date,
        target_date = target_date
      ),
      BuildComponentRow(
        component_name = "sector_perp",
        actual_value = sector_perp_actual,
        forecast_value = sector_har$forecast,
        baseline_value = sector_baseline,
        forecast_origin_date = forecast_origin_date,
        target_date = target_date
      ),
      BuildComponentRow(
        component_name = "relative_q",
        actual_value = q_actual,
        forecast_value = relative_har$forecast,
        baseline_value = q_baseline,
        forecast_origin_date = forecast_origin_date,
        target_date = target_date
      )
    )
    
    # Keep only complete component rows
    component_rows <- component_rows[!sapply(component_rows, is.null)]
    
    if (length(component_rows) != 5) {
      return(NULL)
    }
    
    return(data.table::rbindlist(
      component_rows,
      use.names = TRUE,
      fill = FALSE
    ))
  })
  
  # Remove skipped forecast origins
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    warning(paste("Skipping", ticker,
                  "- no valid forecastability observations."))
    return(NULL)
  }
  
  # Combine all component-level forecastability rows
  forecastability_panel <- data.table::rbindlist(
    forecast_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecastability_panel <- as.data.frame(forecastability_panel)
  rownames(forecastability_panel) <- NULL
  
  return(forecastability_panel)
}



#_______________________BUILD_FORECASTABILITY_PANEL_____________________________

# This function builds the component-level forecastability panel for all
# eligible tickers. It applies BuildForecastabilityPanelOneTicker() stock by
# stock and combines the ticker-level outputs into one long dataframe. The
# function produces diagnostic forecasts and benchmark errors for stock log RV,
# market log RV, raw sector ETF log RV, orthogonal sector component and the
# relative q component.

BuildForecastabilityPanel <- function(daily_log_rv_wide,
                                      relative_har_tickers,
                                      training_window,
                                      market_ticker = "SPY",
                                      first_lag = 1,
                                      second_lag = 5,
                                      third_lag = 22,
                                      minimum_observations = 30) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  relative_har_tickers <- as.data.frame(relative_har_tickers)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.",
         call. = FALSE)
  }
  
  # Check that the relative HAR ticker table has the required columns
  required_cols <- c("ticker", "sector", "sector_etf")
  
  if (!all(required_cols %in% names(relative_har_tickers))) {
    stop("relative_har_tickers must contain ticker, sector and sector_etf.",
         call. = FALSE)
  }
  
  # Check that the market ticker is valid and available
  market_ticker <- as.character(market_ticker)[1]
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.",
         call. = FALSE)
  }
  
  if (!market_ticker %in% names(daily_log_rv_wide)) {
    stop(paste("Market ticker", market_ticker,
               "not found in daily_log_rv_wide."),
         call. = FALSE)
  }
  
  # Check that the one-ticker forecastability function is available
  if (!exists("BuildForecastabilityPanelOneTicker")) {
    stop(
      paste0(
        "BuildForecastabilityPanelOneTicker() is not available. ",
        "Please source the file containing it before calling this function."
      ),
      call. = FALSE
    )
  }
  
  # Check that training_window is a valid positive integer
  if (length(training_window) != 1 ||
      !is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a single positive integer.",
         call. = FALSE)
  }
  
  training_window <- as.integer(training_window)
  
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
    stop("No valid ticker-sector ETF pair found.",
         call. = FALSE)
  }
  
  # Remove duplicated ticker-sector ETF pairs, if any
  relative_har_tickers <- relative_har_tickers[
    !duplicated(relative_har_tickers[, c("ticker", "sector_etf")]),
  ]
  
  rownames(relative_har_tickers) <- NULL
  
  # Build forecastability diagnostics separately for each ticker.
  # Progress is shown by default using progressr, as defined in setup.R.
  forecastability_list <- progressr::with_progress({
    
    p <- progressr::progressor(steps = nrow(relative_har_tickers))
    
    future.apply::future_lapply(
      seq_len(nrow(relative_har_tickers)),
      function(row_index) {
        
        ticker_info <- relative_har_tickers[row_index, ]
        
        ticker <- ticker_info$ticker
        sector <- ticker_info$sector
        sector_etf <- ticker_info$sector_etf
        
        # Build the component-level forecastability panel for one ticker.
        # tryCatch avoids stopping the full diagnostic if one ticker fails.
        ticker_panel <- tryCatch({
          
          BuildForecastabilityPanelOneTicker(
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
        
        return(ticker_panel)
      }
    )
  })
  
  # Remove skipped tickers
  forecastability_list <- forecastability_list[
    !sapply(forecastability_list, is.null)
  ]
  
  if (length(forecastability_list) == 0) {
    stop("No ticker produced valid forecastability diagnostics.",
         call. = FALSE)
  }
  
  # Combine all ticker-level forecastability dataframes
  forecastability_panel <- data.table::rbindlist(
    forecastability_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecastability_panel <- as.data.frame(forecastability_panel)
  
  # Order final output by ticker, component and target date
  forecastability_panel$target_date <- as.Date(
    forecastability_panel$target_date)
  
  forecastability_panel <- forecastability_panel[
    order(forecastability_panel$ticker,
          forecastability_panel$component,
          forecastability_panel$target_date),
  ]
  
  rownames(forecastability_panel) <- NULL
  
  return(forecastability_panel)
}



#_________________________SUMMARIZE_FORECASTABILITY_R2__________________________

# This function summarizes the component-level forecastability panel. Starting
# from out-of-sample HAR forecast errors and rolling-mean benchmark errors, it
# computes the out-of-sample R-squared for each ticker and component. It also
# builds a compact comparison between the relative q component and the direct
# stock log RV forecastability.

SummarizeForecastabilityR2 <- function(forecastability_panel) {
  
  forecastability_panel <- as.data.frame(forecastability_panel)
  
  # Check that the required columns are available
  required_cols <- c(
    "training_window",
    "component",
    "ticker",
    "sector",
    "target_date",
    "actual",
    "forecast",
    "baseline_forecast",
    "squared_error_har",
    "squared_error_baseline"
  )
  
  if (!all(required_cols %in% names(forecastability_panel))) {
    
    missing_cols <- setdiff(required_cols, names(forecastability_panel))
    
    stop(
      paste0(
        "forecastability_panel does not contain the required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Standardize key variables
  forecastability_panel$training_window <- 
    as.integer(forecastability_panel$training_window)
  forecastability_panel$component <- 
    as.character(forecastability_panel$component)
  forecastability_panel$ticker <- as.character(forecastability_panel$ticker)
  forecastability_panel$sector <- as.character(forecastability_panel$sector)
  forecastability_panel$target_date <- 
    as.Date(forecastability_panel$target_date)
  
  # Convert numerical variables to numeric format
  numeric_cols <- c(
    "actual",
    "forecast",
    "baseline_forecast",
    "squared_error_har",
    "squared_error_baseline"
  )
  
  for (column_name in numeric_cols) {
    forecastability_panel[[column_name]] <- suppressWarnings(
      as.numeric(as.character(forecastability_panel[[column_name]]))
    )
  }
  
  # Keep only valid out-of-sample forecast-error observations
  valid_rows <- is.finite(forecastability_panel$squared_error_har) &
    is.finite(forecastability_panel$squared_error_baseline) &
    forecastability_panel$squared_error_baseline >= 0
  
  forecastability_panel <- forecastability_panel[valid_rows, ]
  rownames(forecastability_panel) <- NULL
  
  if (nrow(forecastability_panel) == 0) {
    stop("No valid forecastability observations are available.",
         call. = FALSE)
  }
  
  # Compute ticker-component out-of-sample R-squared values
  r2_table <- forecastability_panel %>%
    dplyr::group_by(training_window, component, ticker, sector) %>%
    dplyr::summarise(
      n_forecasts = dplyr::n(),
      msfe_har = mean(squared_error_har, na.rm = TRUE),
      msfe_baseline = mean(squared_error_baseline, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      r2_oos = dplyr::if_else(
        is.finite(msfe_har) &
          is.finite(msfe_baseline) &
          msfe_baseline > 0,
        1 - msfe_har / msfe_baseline,
        NA_real_
      )
    )
  
  # Build a compact component-level summary
  component_summary <- r2_table %>%
    dplyr::filter(is.finite(r2_oos)) %>%
    dplyr::group_by(training_window, component) %>%
    dplyr::summarise(
      n_tickers = dplyr::n_distinct(ticker),
      mean_r2_oos = mean(r2_oos, na.rm = TRUE),
      median_r2_oos = stats::median(r2_oos, na.rm = TRUE),
      pct_positive_r2_oos = 100 * mean(r2_oos > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(training_window, component)
  
  # Check that the two components needed for the paired comparison are present
  required_comparison_components <- c("stock_log_rv", "relative_q")
  
  if (!all(required_comparison_components %in% unique(r2_table$component))) {
    stop(
      paste0(
        "r2_table must contain both stock_log_rv and relative_q ",
        "to build the paired comparison."
      ),
      call. = FALSE
    )
  }
  
  # Build ticker-level paired differences: relative q forecastability minus
  # direct stock log RV forecastability
  comparison_details <- r2_table %>%
    dplyr::filter(component %in% required_comparison_components) %>%
    dplyr::select(training_window, ticker, sector, component, r2_oos) %>%
    tidyr::pivot_wider(
      names_from = component,
      values_from = r2_oos
    ) %>%
    dplyr::mutate(
      delta_r2_q_vs_stock = relative_q - stock_log_rv
    ) %>%
    dplyr::filter(is.finite(delta_r2_q_vs_stock))
  
  # Build sector-level paired-comparison table
  sector_comparison_table <- comparison_details %>%
    dplyr::group_by(training_window, sector) %>%
    dplyr::summarise(
      n_tickers = dplyr::n_distinct(ticker),
      mean_stock_r2_oos = mean(stock_log_rv, na.rm = TRUE),
      mean_q_r2_oos = mean(relative_q, na.rm = TRUE),
      mean_delta_r2 = mean(delta_r2_q_vs_stock, na.rm = TRUE),
      median_delta_r2 = stats::median(delta_r2_q_vs_stock, na.rm = TRUE),
      pct_positive_delta = 100 * mean(delta_r2_q_vs_stock > 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(mean_delta_r2)
  
  return(list(
    r2_table = r2_table,
    component_summary = component_summary,
    sector_comparison_table = sector_comparison_table
  ))
}



#_________________PLOT_FORECASTABILITY_R2_BOXPLOT_BY_SECTOR_____________________

# This function plots the distribution of stock-level out-of-sample R-squared
# values by sector. For each sector, the boxplot shows the R2_OOS distribution
# of direct stock log RV forecasts. A blue point marks the sector-perp
# forecastability in that sector, a red dashed horizontal line marks the
# market log RV forecastability, and a green triangle marks raw sector ETF
# forecastability. This is a descriptive diagnostic plot.

PlotForecastabilityR2BoxplotBySector <- function(r2_table,
                                                 output_path = NULL,
                                                 width = 12,
                                                 height = 7) {
  
  r2_table <- as.data.frame(r2_table)
  
  # Check that the required columns are available
  required_cols <- c(
    "training_window",
    "component",
    "ticker",
    "sector",
    "r2_oos"
  )
  
  if (!all(required_cols %in% names(r2_table))) {
    
    missing_cols <- setdiff(required_cols, names(r2_table))
    
    stop(
      paste0(
        "r2_table does not contain the required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Standardize key variables
  r2_table$training_window <- as.integer(r2_table$training_window)
  r2_table$component <- as.character(r2_table$component)
  r2_table$ticker <- as.character(r2_table$ticker)
  r2_table$sector <- as.character(r2_table$sector)
  r2_table$r2_oos <- as.numeric(r2_table$r2_oos)
  
  # Keep only one training window for this diagnostic plot
  training_windows <- unique(r2_table$training_window[
    is.finite(r2_table$training_window)
  ])
  
  if (length(training_windows) != 1) {
    stop("r2_table must contain exactly one training window.",
         call. = FALSE)
  }
  
  training_window <- training_windows[1]
  
  # Keep only valid observations
  plot_data <- r2_table %>%
    dplyr::filter(
      component %in% c("stock_log_rv", "sector_etf_log_rv",
                       "sector_perp", "market_log_rv"),
      !is.na(sector),
      nzchar(sector),
      is.finite(r2_oos)
    )
  
  if (nrow(plot_data) == 0) {
    stop("No valid observations available for the forecastability plot.",
         call. = FALSE)
  }
  
  # Stock-level R2_OOS distribution by sector
  stock_df <- plot_data %>%
    dplyr::filter(component == "stock_log_rv")
  
  # Sector-perp R2_OOS marker by sector.
  # We use the median because sector_perp is computed at ticker level in the
  # diagnostic panel, even though it represents a sector-level component.
  sector_perp_df <- plot_data %>%
    dplyr::filter(component == "sector_perp") %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      sector_perp_r2_oos = stats::median(r2_oos, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Raw sector ETF R2_OOS marker by sector
  sector_etf_df <- plot_data %>%
    dplyr::filter(component == "sector_etf_log_rv") %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      sector_etf_r2_oos = stats::median(r2_oos, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Market R2_OOS marker.
  # We use the median across ticker-level diagnostic rows to avoid duplication.
  market_df <- plot_data %>%
    dplyr::filter(component == "market_log_rv") %>%
    dplyr::summarise(
      market_r2_oos = stats::median(r2_oos, na.rm = TRUE)
    )
  
  # Order sectors by median stock forecastability
  sector_order <- stock_df %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      median_stock_r2_oos = stats::median(r2_oos, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::arrange(median_stock_r2_oos) %>%
    dplyr::pull(sector)
  
  stock_df$sector <- factor(stock_df$sector, levels = sector_order)
  sector_perp_df$sector <- factor(sector_perp_df$sector, levels = sector_order)
  sector_etf_df$sector <- factor(sector_etf_df$sector, levels = sector_order)
  
  # Build the boxplot
  p <- ggplot2::ggplot(
    stock_df,
    ggplot2::aes(x = sector, y = r2_oos)
  ) +
    ggplot2::geom_boxplot(
      fill = "grey85",
      color = "grey35",
      outlier.alpha = 0.30
    ) +
    ggplot2::geom_point(
      data = sector_perp_df,
      ggplot2::aes(
        x = sector,
        y = sector_perp_r2_oos,
        color = "Sector-perp component"
      ),
      inherit.aes = FALSE,
      size = 2.8
    ) +
    ggplot2::geom_point(
      data = sector_etf_df,
      ggplot2::aes(
        x = sector,
        y = sector_etf_r2_oos,
        color = "Raw sector ETF"
      ),
      inherit.aes = FALSE,
      size = 2.8,
      shape = 17
    ) +
    ggplot2::geom_hline(
      data = market_df,
      ggplot2::aes(
        yintercept = market_r2_oos,
        color = "SPY"
      ),
      linetype = "dashed",
      linewidth = 0.8
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "SPY" = "red3",
        "Sector-perp component" = "blue3",
        "Raw sector ETF" = "darkgreen"
      )
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Forecastability of Stock Log Realized Variance by Sector",
      subtitle = paste0(
        "Stock R\u00B2 OOS by sector. Training Window = ",
        training_window,
        "."
      ),
      x = "Sector",
      y = expression(R[OOS]^2),
      color = "Component"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Save the plot if an output path is provided
  if (!is.null(output_path)) {
    
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    
    ggplot2::ggsave(
      filename = output_path,
      plot = p,
      width = width,
      height = height
    )
  }
  
  return(p)
}



#_____________________________END_OF_THE_SCRIPT_________________________________