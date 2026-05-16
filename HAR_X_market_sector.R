


# In this section we build and estimate rolling HAR-X market-sector models for
# realized volatility forecasting. The model extends the standard HAR benchmark
# by adding market and sector information directly as additional regressors.
# Starting from daily log realized variance series, we construct daily, weekly
# and monthly HAR components for each stock, for the market benchmark, and for
# the corresponding sector ETF. Then, we estimate rolling one-step-ahead OLS
# forecasts and store the corresponding forecast errors.



#____________BUILD_HAR_X_MARKET_SECTOR_DATAFRAME_ONE_TICKER____________________

# This function builds the HAR-X market-sector regression dataframe for one
# selected ticker. Starting from a wide daily log realized variance dataframe,
# it constructs daily, weekly and monthly HAR components for the stock, the
# market benchmark and the corresponding sector ETF. The target variable is the
# next-day stock log realized variance. The output is ready to be used in an OLS
# regression of the stock target on stock, market and sector HAR components.

BuildHARXMarketSectorDataForTicker <- function(daily_log_rv_wide, ticker,
                                               sector, sector_etf,
                                               market_ticker,
                                               first_lag = 1,
                                               second_lag = 5,
                                               third_lag = 22) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.",
         call. = FALSE)
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
  
  # Check that stock, market and sector ETF columns exist
  required_tickers <- c(ticker, market_ticker, sector_etf)
  
  if (!all(required_tickers %in% names(daily_log_rv_wide))) {
    stop(
      paste0(
        "daily_log_rv_wide must contain the selected stock, ",
        "market ticker and sector ETF."
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
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Extract dates and daily log realized variance series
  dates <- daily_log_rv_wide$date
  y_stock <- as.numeric(daily_log_rv_wide[[ticker]])
  y_market <- as.numeric(daily_log_rv_wide[[market_ticker]])
  y_sector <- as.numeric(daily_log_rv_wide[[sector_etf]])
  
  # Define the maximum lag needed to build all HAR components
  max_lag <- max(lags)
  n <- length(dates)
  
  # Check that there are enough observations to build the HAR-X variables
  if (n < (max_lag + 1)) {
    stop("Not enough observations to build HAR-X market-sector data.",
         call. = FALSE)
  }
  
  # Build stock HAR components
  stock_daily <- FastRollingMeanAllFinite(
    series = y_stock,
    window_length = first_lag
  )
  
  stock_weekly <- FastRollingMeanAllFinite(
    series = y_stock,
    window_length = second_lag
  )
  
  stock_monthly <- FastRollingMeanAllFinite(
    series = y_stock,
    window_length = third_lag
  )
  
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
  
  # Build sector ETF HAR components
  sector_daily <- FastRollingMeanAllFinite(
    series = y_sector,
    window_length = first_lag
  )
  
  sector_weekly <- FastRollingMeanAllFinite(
    series = y_sector,
    window_length = second_lag
  )
  
  sector_monthly <- FastRollingMeanAllFinite(
    series = y_sector,
    window_length = third_lag
  )
  
  # The first usable origin is the maximum HAR lag. The last usable origin is
  # the day before the last observation because the target is one day ahead.
  origin_indices <- max_lag:(n - 1)
  
  # Build the HAR-X market-sector regression dataframe
  har_x_data <- data.frame(
    ticker = ticker,
    sector = sector,
    sector_etf = sector_etf,
    market_ticker = market_ticker,
    origin_date = dates[origin_indices],
    target_date = dates[origin_indices + 1],
    target = y_stock[origin_indices + 1],
    stock_daily = stock_daily[origin_indices],
    stock_weekly = stock_weekly[origin_indices],
    stock_monthly = stock_monthly[origin_indices],
    market_daily = market_daily[origin_indices],
    market_weekly = market_weekly[origin_indices],
    market_monthly = market_monthly[origin_indices],
    sector_daily = sector_daily[origin_indices],
    sector_weekly = sector_weekly[origin_indices],
    sector_monthly = sector_monthly[origin_indices],
    stringsAsFactors = FALSE
  )
  
  # Keep only complete and finite HAR-X observations
  model_cols <- c(
    "target",
    "stock_daily",
    "stock_weekly",
    "stock_monthly",
    "market_daily",
    "market_weekly",
    "market_monthly",
    "sector_daily",
    "sector_weekly",
    "sector_monthly"
  )
  
  complete_rows <- Reduce(
    `&`,
    lapply(model_cols, function(column_name) {
      is.finite(har_x_data[[column_name]])
    })
  )
  
  har_x_data <- har_x_data[complete_rows, ]
  rownames(har_x_data) <- NULL
  
  return(har_x_data)
}



#_________________ROLLING_HAR_X_MARKET_SECTOR_MODEL_ONE_TICKER_________________

# This function estimates a rolling HAR-X market-sector model for one selected
# ticker. Starting from a ticker-level HAR-X dataframe, it uses a fixed rolling
# window to estimate the model target ~ stock HAR components + market HAR
# components + sector HAR components. At each step, it produces a one-day-ahead
# forecast and records the corresponding forecast error. The output has one row
# for each out-of-sample forecast date.

RollingHARXMarketSectorForecastByTicker <- function(har_x_data,
                                                    training_window = 100) {
  
  har_x_data <- as.data.frame(har_x_data)
  
  # Check that the required columns exist
  required_cols <- c(
    "ticker",
    "sector",
    "sector_etf",
    "market_ticker",
    "origin_date",
    "target_date",
    "target",
    "stock_daily",
    "stock_weekly",
    "stock_monthly",
    "market_daily",
    "market_weekly",
    "market_monthly",
    "sector_daily",
    "sector_weekly",
    "sector_monthly"
  )
  
  if (!all(required_cols %in% names(har_x_data))) {
    stop(
      paste0(
        "har_x_data must contain ticker, sector, sector_etf, market_ticker, ",
        "origin_date, target_date, target, stock HAR components, market HAR ",
        "components and sector HAR components."
      ),
      call. = FALSE
    )
  }
  
  # Order observations by origin date
  har_x_data$origin_date <- as.Date(har_x_data$origin_date)
  har_x_data$target_date <- as.Date(har_x_data$target_date)
  har_x_data <- har_x_data[order(har_x_data$origin_date), ]
  rownames(har_x_data) <- NULL
  
  # Keep only complete and finite observations
  model_cols <- c(
    "target",
    "stock_daily",
    "stock_weekly",
    "stock_monthly",
    "market_daily",
    "market_weekly",
    "market_monthly",
    "sector_daily",
    "sector_weekly",
    "sector_monthly"
  )
  
  complete_rows <- Reduce(
    `&`,
    lapply(model_cols, function(column_name) {
      is.finite(har_x_data[[column_name]])
    })
  )
  
  har_x_data <- har_x_data[complete_rows, ]
  rownames(har_x_data) <- NULL
  
  # Check that there are enough observations for rolling estimation
  if (nrow(har_x_data) <= training_window) {
    stop("Not enough HAR-X observations for the selected training window.",
         call. = FALSE)
  }
  
  # Define ticker-level identifiers
  ticker <- unique(har_x_data$ticker)
  sector <- unique(har_x_data$sector)
  sector_etf <- unique(har_x_data$sector_etf)
  market_ticker <- unique(har_x_data$market_ticker)
  
  if (length(ticker) != 1) {
    stop("har_x_data must refer to one ticker only.", call. = FALSE)
  }
  
  if (length(sector_etf) != 1) {
    stop("har_x_data must refer to one sector ETF only.", call. = FALSE)
  }
  
  if (length(market_ticker) != 1) {
    stop("har_x_data must refer to one market ticker only.", call. = FALSE)
  }
  
  # If sector is missing, keep it as NA but avoid length problems
  if (length(sector) != 1) {
    sector <- NA_character_
  }
  
  # Pre-build the HAR-X design matrix once
  x_matrix <- cbind(
    intercept = 1,
    stock_daily = har_x_data$stock_daily,
    stock_weekly = har_x_data$stock_weekly,
    stock_monthly = har_x_data$stock_monthly,
    market_daily = har_x_data$market_daily,
    market_weekly = har_x_data$market_weekly,
    market_monthly = har_x_data$market_monthly,
    sector_daily = har_x_data$sector_daily,
    sector_weekly = har_x_data$sector_weekly,
    sector_monthly = har_x_data$sector_monthly
  )
  
  y_vector <- har_x_data$target
  
  # Define the out-of-sample test indices
  test_indices <- (training_window + 1):nrow(har_x_data)
  
  # Estimate the rolling HAR-X model and store one-step-ahead forecast errors
  forecast_list <- lapply(test_indices, function(test_index) {
    
    # Select the rolling training window
    train_start <- test_index - training_window
    train_end <- test_index - 1
    
    train_rows <- train_start:train_end
    
    # Fit the HAR-X market-sector model on the rolling training window
    har_x_model <- tryCatch(
      stats::lm.fit(
        x = x_matrix[train_rows, , drop = FALSE],
        y = y_vector[train_rows]
      ),
      error = function(e) NULL
    )
    
    if (is.null(har_x_model) || is.null(har_x_model$coefficients)) {
      return(NULL)
    }
    
    # Use the estimated coefficients to produce the one-step-ahead forecast.
    # The pivot is used for safety in case the design matrix is rank-deficient.
    rank <- har_x_model$rank
    
    if (is.null(rank) || rank < 1) {
      return(NULL)
    }
    
    pivot <- har_x_model$qr$pivot[seq_len(rank)]
    coefficients <- har_x_model$coefficients[pivot]
    
    if (!all(is.finite(coefficients))) {
      return(NULL)
    }
    
    forecast_value <- as.numeric(sum(
      x_matrix[test_index, pivot] * coefficients
    ))
    
    if (!is.finite(forecast_value)) {
      return(NULL)
    }
    
    # Extract the realized value
    actual_value <- y_vector[test_index]
    
    if (!is.finite(actual_value)) {
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
      forecast_origin_date = har_x_data$origin_date[test_index],
      target_date = har_x_data$target_date[test_index],
      actual = actual_value,
      forecast = forecast_value,
      error = error_value,
      squared_error = squared_error,
      absolute_error = absolute_error,
      qlike = qlike,
      stringsAsFactors = FALSE
    )
  })
  
  # Remove skipped forecast origins
  forecast_list <- forecast_list[!sapply(forecast_list, is.null)]
  
  if (length(forecast_list) == 0) {
    warning(paste("Skipping", ticker, "- no valid HAR-X forecasts."))
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



#____________ROLLING_HAR_X_MARKET_SECTOR_FORECAST_PANEL________________________

# This function estimates rolling HAR-X market-sector forecasts for all eligible
# tickers. It applies RollingHARXMarketSectorForecastByTicker() stock by stock
# and combines the ticker-level forecast-error dataframes into one panel.

RollingHARXMarketSectorForecastPanel <- function(daily_log_rv_wide,
                                                 relative_har_tickers,
                                                 training_window,
                                                 market_ticker,
                                                 first_lag = 1,
                                                 second_lag = 5,
                                                 third_lag = 22) {
  
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
  market_ticker <- as.character(market_ticker)[1]
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.", call. = FALSE)
  }
  
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
    stop(
      paste0(
        "No valid ticker-sector ETF pair found for the HAR-X ",
        "market-sector model."
      ),
      call. = FALSE
    )
  }
  
  # Remove duplicated ticker-sector ETF pairs, if any
  relative_har_tickers <- relative_har_tickers[
    !duplicated(relative_har_tickers[, c("ticker", "sector_etf")]),
  ]
  
  rownames(relative_har_tickers) <- NULL
  
  # Estimate rolling HAR-X market-sector forecasts separately for each ticker.
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
        
        # Build and estimate the HAR-X model for the selected ticker.
        # tryCatch avoids stopping the full panel estimation if one ticker fails.
        forecast_errors <- tryCatch({
          
          # Build the HAR-X market-sector regression dataframe
          har_x_data <- BuildHARXMarketSectorDataForTicker(
            daily_log_rv_wide = daily_log_rv_wide,
            ticker = ticker,
            sector = sector,
            sector_etf = sector_etf,
            market_ticker = market_ticker,
            first_lag = first_lag,
            second_lag = second_lag,
            third_lag = third_lag
          )
          
          # Skip tickers with too few observations for the selected window
          if (nrow(har_x_data) <= training_window) {
            warning(paste("Skipping", ticker,
                          "- not enough HAR-X observations."))
            NULL
          } else {
            
            # Estimate rolling HAR-X forecasts for the selected ticker
            RollingHARXMarketSectorForecastByTicker(
              har_x_data = har_x_data,
              training_window = training_window
            )
          }
          
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
    stop("No ticker produced valid rolling HAR-X market-sector forecasts.",
         call. = FALSE)
  }
  
  # Combine all ticker-level forecast-error dataframes
  forecast_errors_panel <- data.table::rbindlist(
    forecast_list,
    use.names = TRUE,
    fill = FALSE
  )
  
  forecast_errors_panel <- as.data.frame(forecast_errors_panel)
  
  # Order the final panel by ticker and target date
  forecast_errors_panel$target_date <- as.Date(
    forecast_errors_panel$target_date
  )
  
  forecast_errors_panel <- forecast_errors_panel[
    order(forecast_errors_panel$ticker, forecast_errors_panel$target_date),
  ]
  
  rownames(forecast_errors_panel) <- NULL
  
  return(forecast_errors_panel)
}