#____________BUILD_RELATIVE_HAR_ERROR_DECOMPOSITION_PANEL______________________

# This function builds a row-level error decomposition panel for the relative
# HAR model. Starting from the relative HAR forecast-error output and the daily
# log RV dataframe, it reconstructs the realized market, sector-perp and q
# components at the target date using only the rolling decomposition
# coefficients stored at the forecast origin. It then decomposes the final
# relative HAR forecast error into weighted market, weighted sector-perp and
# q error contributions.

BuildRelativeHARErrorDecompositionPanel <- function(
    relative_har_forecast_errors, daily_log_rv_wide, tolerance = 1e-8) {
  
  relative_har_forecast_errors <- as.data.frame(relative_har_forecast_errors)
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  
  # Check that the required columns are available in the relative HAR output
  required_forecast_cols <- c(
    "ticker",
    "sector",
    "sector_etf",
    "market_ticker",
    "forecast_origin_date",
    "target_date",
    "actual",
    "forecast",
    "error",
    "squared_error",
    "market_forecast",
    "sector_perp_forecast",
    "q_forecast",
    "alpha_hat",
    "beta_market_hat",
    "beta_sector_hat",
    "sector_intercept_hat",
    "sector_beta_market_hat"
  )
  
  if (!all(required_forecast_cols %in% names(relative_har_forecast_errors))) {
    
    missing_cols <- setdiff(required_forecast_cols,
                            names(relative_har_forecast_errors))
    
    stop(
      paste0(
        "relative_har_forecast_errors does not contain the required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Check that the daily log RV dataframe contains the date column
  if (!"date" %in% names(daily_log_rv_wide)) {
    stop("daily_log_rv_wide must contain a 'date' column.",
         call. = FALSE)
  }
  
  # Check that tolerance is valid
  if (length(tolerance) != 1 ||
      !is.finite(tolerance) ||
      tolerance < 0) {
    stop("tolerance must be a single non-negative finite number.",
         call. = FALSE)
  }
  
  # Standardize key variables
  relative_har_forecast_errors$ticker <- 
    as.character(relative_har_forecast_errors$ticker)
  relative_har_forecast_errors$sector <- 
    as.character(relative_har_forecast_errors$sector)
  relative_har_forecast_errors$sector_etf <- 
    as.character(relative_har_forecast_errors$sector_etf)
  relative_har_forecast_errors$market_ticker <- 
    as.character(relative_har_forecast_errors$market_ticker)
  relative_har_forecast_errors$forecast_origin_date <- 
    as.Date(relative_har_forecast_errors$forecast_origin_date)
  relative_har_forecast_errors$target_date <- 
    as.Date(relative_har_forecast_errors$target_date)
  
  daily_log_rv_wide$date <- as.Date(daily_log_rv_wide$date)
  daily_log_rv_wide <- daily_log_rv_wide[order(daily_log_rv_wide$date), ]
  rownames(daily_log_rv_wide) <- NULL
  
  # Convert numerical columns in the relative HAR output
  numeric_cols <- c(
    "actual",
    "forecast",
    "error",
    "squared_error",
    "market_forecast",
    "sector_perp_forecast",
    "q_forecast",
    "alpha_hat",
    "beta_market_hat",
    "beta_sector_hat",
    "sector_intercept_hat",
    "sector_beta_market_hat"
  )
  
  for (column_name in numeric_cols) {
    relative_har_forecast_errors[[column_name]] <- suppressWarnings(
      as.numeric(as.character(relative_har_forecast_errors[[column_name]]))
    )
  }
  
  # Check that all stock, market and sector ETF columns are available
  required_tickers <- unique(c(
    relative_har_forecast_errors$ticker,
    relative_har_forecast_errors$market_ticker,
    relative_har_forecast_errors$sector_etf
  ))
  
  required_tickers <- required_tickers[
    !is.na(required_tickers) & nzchar(required_tickers)
  ]
  
  missing_tickers <- setdiff(required_tickers, names(daily_log_rv_wide))
  
  if (length(missing_tickers) > 0) {
    stop(
      paste0(
        "daily_log_rv_wide does not contain the required columns: ",
        paste(missing_tickers, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Match each forecast target date with the corresponding daily RV row
  target_row_index <- match(
    relative_har_forecast_errors$target_date,
    daily_log_rv_wide$date
  )
  
  if (any(is.na(target_row_index))) {
    stop("Some target dates are not available in daily_log_rv_wide.",
         call. = FALSE)
  }
  
  # Extract actual stock, market and raw sector ETF log RV values at target date
  daily_matrix <- as.matrix(
    daily_log_rv_wide[, required_tickers, drop = FALSE]
  )
  
  storage.mode(daily_matrix) <- "double"
  
  stock_col_index <- match(relative_har_forecast_errors$ticker,
                           required_tickers)
  market_col_index <- match(relative_har_forecast_errors$market_ticker,
                            required_tickers)
  sector_col_index <- match(relative_har_forecast_errors$sector_etf,
                            required_tickers)
  
  stock_actual <- daily_matrix[cbind(target_row_index, stock_col_index)]
  market_actual <- daily_matrix[cbind(target_row_index, market_col_index)]
  sector_actual_raw <- daily_matrix[cbind(target_row_index, sector_col_index)]
  
  # Check that the stock actual value matches the stored relative HAR actual
  stock_actual_difference <- stock_actual - relative_har_forecast_errors$actual
  
  max_stock_actual_difference <- max(
    abs(stock_actual_difference),
    na.rm = TRUE
  )
  
  if (is.finite(max_stock_actual_difference) &&
      max_stock_actual_difference > tolerance) {
    stop(
      paste0(
        "Stored relative HAR actual values do not match daily_log_rv_wide. ",
        "Maximum difference: ",
        max_stock_actual_difference,
        "."
      ),
      call. = FALSE
    )
  }
  
  # Reconstruct out-of-sample actual sector-perp using forecast-origin
  # decomposition coefficients
  sector_perp_actual <- sector_actual_raw -
    (relative_har_forecast_errors$sector_intercept_hat +
       relative_har_forecast_errors$sector_beta_market_hat * market_actual)
  
  # Reconstruct out-of-sample actual q using forecast-origin decomposition
  # coefficients
  q_actual <- stock_actual -
    (relative_har_forecast_errors$alpha_hat +
       relative_har_forecast_errors$beta_market_hat * market_actual +
       relative_har_forecast_errors$beta_sector_hat * sector_perp_actual)
  
  # Compute component-level forecast errors
  market_error <- market_actual - relative_har_forecast_errors$market_forecast
  
  sector_perp_error <- sector_perp_actual -
    relative_har_forecast_errors$sector_perp_forecast
  
  q_error <- q_actual - relative_har_forecast_errors$q_forecast
  
  # Compute weighted error contributions to final relative HAR forecast error
  market_error_contribution <- 
    relative_har_forecast_errors$beta_market_hat * market_error
  
  sector_error_contribution <- 
    relative_har_forecast_errors$beta_sector_hat * sector_perp_error
  
  q_error_contribution <- q_error
  
  # Reconstruct the final relative HAR forecast error from component errors
  reconstructed_error <- market_error_contribution +
    sector_error_contribution +
    q_error_contribution
  
  final_error <- relative_har_forecast_errors$actual -
    relative_har_forecast_errors$forecast
  
  reconstruction_difference <- reconstructed_error - final_error
  
  max_reconstruction_difference <- max(
    abs(reconstruction_difference),
    na.rm = TRUE
  )
  
  if (is.finite(max_reconstruction_difference) &&
      max_reconstruction_difference > tolerance) {
    stop(
      paste0(
        "The reconstructed relative HAR error does not match the stored ",
        "forecast error. Maximum difference: ",
        max_reconstruction_difference,
        "."
      ),
      call. = FALSE
    )
  }
  
  # Build the row-level decomposition panel
  decomposition_panel <- data.frame(
    ticker = relative_har_forecast_errors$ticker,
    sector = relative_har_forecast_errors$sector,
    sector_etf = relative_har_forecast_errors$sector_etf,
    market_ticker = relative_har_forecast_errors$market_ticker,
    forecast_origin_date = relative_har_forecast_errors$forecast_origin_date,
    target_date = relative_har_forecast_errors$target_date,
    
    stock_actual = stock_actual,
    stock_forecast = relative_har_forecast_errors$forecast,
    final_error = final_error,
    stored_error = relative_har_forecast_errors$error,
    
    market_actual = market_actual,
    market_forecast = relative_har_forecast_errors$market_forecast,
    market_error = market_error,
    
    sector_actual_raw = sector_actual_raw,
    sector_perp_actual = sector_perp_actual,
    sector_perp_forecast =
      relative_har_forecast_errors$sector_perp_forecast,
    sector_perp_error = sector_perp_error,
    
    q_actual = q_actual,
    q_forecast = relative_har_forecast_errors$q_forecast,
    q_error = q_error,
    
    alpha_hat = relative_har_forecast_errors$alpha_hat,
    beta_market_hat = relative_har_forecast_errors$beta_market_hat,
    beta_sector_hat = relative_har_forecast_errors$beta_sector_hat,
    sector_intercept_hat =
      relative_har_forecast_errors$sector_intercept_hat,
    sector_beta_market_hat =
      relative_har_forecast_errors$sector_beta_market_hat,
    
    market_error_contribution = market_error_contribution,
    sector_error_contribution = sector_error_contribution,
    q_error_contribution = q_error_contribution,
    reconstructed_error = reconstructed_error,
    reconstruction_difference = reconstruction_difference,
    
    final_squared_error = final_error^2,
    reconstructed_squared_error = reconstructed_error^2,
    market_squared_term = market_error_contribution^2,
    sector_squared_term = sector_error_contribution^2,
    q_squared_term = q_error_contribution^2,
    market_sector_cross_term =
      2 * market_error_contribution * sector_error_contribution,
    market_q_cross_term =
      2 * market_error_contribution * q_error_contribution,
    sector_q_cross_term =
      2 * sector_error_contribution * q_error_contribution,
    stringsAsFactors = FALSE
  )
  
  # Keep only rows with complete decomposition values
  decomposition_cols <- c(
    "final_error",
    "market_error_contribution",
    "sector_error_contribution",
    "q_error_contribution",
    "reconstructed_error",
    "final_squared_error",
    "market_squared_term",
    "sector_squared_term",
    "q_squared_term",
    "market_sector_cross_term",
    "market_q_cross_term",
    "sector_q_cross_term"
  )
  
  valid_rows <- Reduce(
    `&`,
    lapply(decomposition_cols, function(column_name) {
      is.finite(decomposition_panel[[column_name]])
    })
  )
  
  decomposition_panel <- decomposition_panel[valid_rows, ]
  rownames(decomposition_panel) <- NULL
  
  if (nrow(decomposition_panel) == 0) {
    stop("No valid relative HAR error-decomposition rows are available.",
         call. = FALSE)
  }
  
  return(decomposition_panel)
}



#___________SUMMARIZE_RELATIVE_HAR_ERROR_DECOMPOSITION_________________________

# This function summarizes the relative HAR error-decomposition panel. Starting
# from row-level weighted component error contributions, it computes the average
# contribution of market, sector-perp and q errors to the final relative HAR
# mean squared forecast error. It also reports the cross terms, which measure
# whether component forecast errors amplify or offset each other.

SummarizeRelativeHARErrorDecomposition <- function(decomposition_panel) {
  
  decomposition_panel <- as.data.frame(decomposition_panel)
  
  # Check that the required columns are available
  required_cols <- c(
    "ticker",
    "sector",
    "target_date",
    "final_squared_error",
    "reconstructed_squared_error",
    "market_squared_term",
    "sector_squared_term",
    "q_squared_term",
    "market_sector_cross_term",
    "market_q_cross_term",
    "sector_q_cross_term",
    "reconstruction_difference"
  )
  
  if (!all(required_cols %in% names(decomposition_panel))) {
    
    missing_cols <- setdiff(required_cols, names(decomposition_panel))
    
    stop(
      paste0(
        "decomposition_panel does not contain the required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Standardize key variables
  decomposition_panel$ticker <- as.character(decomposition_panel$ticker)
  decomposition_panel$sector <- as.character(decomposition_panel$sector)
  decomposition_panel$target_date <- as.Date(decomposition_panel$target_date)
  
  # Convert numerical variables to numeric format
  numeric_cols <- c(
    "final_squared_error",
    "reconstructed_squared_error",
    "market_squared_term",
    "sector_squared_term",
    "q_squared_term",
    "market_sector_cross_term",
    "market_q_cross_term",
    "sector_q_cross_term",
    "reconstruction_difference"
  )
  
  for (column_name in numeric_cols) {
    decomposition_panel[[column_name]] <- suppressWarnings(
      as.numeric(as.character(decomposition_panel[[column_name]]))
    )
  }
  
  # Keep only valid decomposition observations
  valid_rows <- Reduce(
    `&`,
    lapply(numeric_cols, function(column_name) {
      is.finite(decomposition_panel[[column_name]])
    })
  )
  
  decomposition_panel <- decomposition_panel[valid_rows, ]
  rownames(decomposition_panel) <- NULL
  
  if (nrow(decomposition_panel) == 0) {
    stop("No valid relative HAR error-decomposition rows are available.",
         call. = FALSE)
  }
  
  # Internal helper: compute one decomposition summary table
  BuildDecompositionSummary <- function(data) {
    
    summary_table <- data %>%
      dplyr::summarise(
        n_tickers = dplyr::n_distinct(ticker),
        n_forecasts = dplyr::n(),
        final_msfe = mean(final_squared_error, na.rm = TRUE),
        reconstructed_msfe = mean(reconstructed_squared_error, na.rm = TRUE),
        market_squared_component = mean(market_squared_term, na.rm = TRUE),
        sector_squared_component = mean(sector_squared_term, na.rm = TRUE),
        q_squared_component = mean(q_squared_term, na.rm = TRUE),
        market_sector_cross_component =
          mean(market_sector_cross_term, na.rm = TRUE),
        market_q_cross_component =
          mean(market_q_cross_term, na.rm = TRUE),
        sector_q_cross_component =
          mean(sector_q_cross_term, na.rm = TRUE),
        max_abs_reconstruction_difference =
          max(abs(reconstruction_difference), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        total_squared_component =
          market_squared_component +
          sector_squared_component +
          q_squared_component,
        total_cross_component =
          market_sector_cross_component +
          market_q_cross_component +
          sector_q_cross_component,
        total_decomposed_msfe =
          total_squared_component + total_cross_component,
        decomposition_gap = total_decomposed_msfe - final_msfe,
        
        market_squared_share_pct =
          ifelse(final_msfe > 0,
                 100 * market_squared_component / final_msfe,
                 NA_real_),
        sector_squared_share_pct =
          ifelse(final_msfe > 0,
                 100 * sector_squared_component / final_msfe,
                 NA_real_),
        q_squared_share_pct =
          ifelse(final_msfe > 0,
                 100 * q_squared_component / final_msfe,
                 NA_real_),
        total_cross_share_pct =
          ifelse(final_msfe > 0,
                 100 * total_cross_component / final_msfe,
                 NA_real_)
      )
    
    return(summary_table)
  }
  
  # Build sector-level decomposition summary
  sector_summary <- decomposition_panel %>%
    dplyr::filter(!is.na(sector), nzchar(sector)) %>%
    dplyr::group_by(sector) %>%
    BuildDecompositionSummary() %>%
    dplyr::arrange(total_cross_component)
  
  return(sector_summary)
}