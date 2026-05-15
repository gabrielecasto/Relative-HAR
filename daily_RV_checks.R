# In this section we check the quality of the cleaned daily log realized
# variance panel used by the HAR and relative HAR models. The checks verify
# that dates are ordered and unique, removed benchmark dates are no longer
# present, excluded tickers are no longer in the panel, and all model-relevant
# daily log realized variance columns contain only finite values.



#________________________CHECK_DAILY_RV_PANEL_QUALITY___________________________

# This function checks whether the cleaned daily log realized variance panel is
# valid for HAR and relative HAR estimation. It verifies the date structure,
# removed dates, removed tickers, non-finite values in the full RV panel,
# non-finite values in benchmark columns, and non-finite values in the columns
# used by the relative HAR model.

CheckDailyRVPanelQuality <- function(daily_rv_panel,
                                     ticker_sector_table,
                                     relative_har_tickers,
                                     market_ticker = "SPY",
                                     bad_benchmark_dates = NULL,
                                     removed_tickers = c("DOW")) {
  
  daily_rv_panel <- as.data.frame(daily_rv_panel)
  ticker_sector_table <- as.data.frame(ticker_sector_table)
  relative_har_tickers <- as.data.frame(relative_har_tickers)
  
  # Check that the date column exists
  if (!"date" %in% names(daily_rv_panel)) {
    stop("daily_rv_panel must contain a 'date' column.", call. = FALSE)
  }
  
  # Check that the relative HAR ticker table has the required columns
  required_cols <- c("ticker", "sector", "sector_etf")
  
  if (!all(required_cols %in% names(relative_har_tickers))) {
    stop("relative_har_tickers must contain ticker, sector and sector_etf.",
         call. = FALSE)
  }
  
  # Convert date and ticker columns to the appropriate formats
  daily_rv_panel$date <- as.Date(daily_rv_panel$date)
  relative_har_tickers$ticker <- as.character(relative_har_tickers$ticker)
  relative_har_tickers$sector_etf <- 
    as.character(relative_har_tickers$sector_etf)
  
  # Check date structure
  cat("\nDate structure checks:\n")
  cat("Number of rows:", nrow(daily_rv_panel), "\n")
  cat("Duplicated dates:", sum(duplicated(daily_rv_panel$date)), "\n")
  cat("Dates sorted:",
      all(order(daily_rv_panel$date) == seq_along(daily_rv_panel$date)),
      "\n")
  
  # Check that removed benchmark dates are not in the panel anymore
  if (!is.null(bad_benchmark_dates)) {
    cat("\nRemaining bad benchmark dates:\n")
    print(intersect(daily_rv_panel$date, as.Date(bad_benchmark_dates)))
  }
  
  # Check that removed tickers are not in the panel anymore
  cat("\nRemoved ticker checks:\n")
  
  for (ticker in removed_tickers) {
    cat(ticker, "still in daily_rv_panel:",
        ticker %in% names(daily_rv_panel), "\n")
  }
  
  # Check non-finite values in all daily RV columns
  rv_cols <- setdiff(names(daily_rv_panel), "date")
  
  rv_completeness <- data.frame(
    ticker = rv_cols,
    nonfinite_days = sapply(rv_cols, function(ticker) {
      sum(!is.finite(daily_rv_panel[[ticker]]))
    }),
    na_days = sapply(rv_cols, function(ticker) {
      sum(is.na(daily_rv_panel[[ticker]]))
    }),
    nan_days = sapply(rv_cols, function(ticker) {
      sum(is.nan(daily_rv_panel[[ticker]]))
    }),
    inf_days = sapply(rv_cols, function(ticker) {
      sum(is.infinite(daily_rv_panel[[ticker]]))
    }),
    stringsAsFactors = FALSE
  )
  
  rv_completeness <- rv_completeness[
    order(rv_completeness$nonfinite_days, decreasing = TRUE),
  ]
  
  problematic_rv_columns <- rv_completeness[
    rv_completeness$nonfinite_days > 0,
  ]
  
  cat("\nDaily RV completeness summary:\n")
  print(summary(rv_completeness$nonfinite_days))
  
  cat("\nProblematic daily RV columns:\n")
  print(problematic_rv_columns)
  
  # Check non-finite values in market and sector ETF benchmark columns
  benchmark_tickers <- unique(c(market_ticker, ticker_sector_table$sector_etf))
  
  benchmark_tickers <- benchmark_tickers[
    benchmark_tickers %in% names(daily_rv_panel)
  ]
  
  benchmark_nonfinite <- sapply(benchmark_tickers, function(ticker) {
    sum(!is.finite(daily_rv_panel[[ticker]]))
  })
  
  cat("\nBenchmark non-finite checks:\n")
  print(benchmark_nonfinite[benchmark_nonfinite > 0])
  
  # Check non-finite values in all columns needed by the relative HAR model
  model_columns <- unique(c(
    relative_har_tickers$ticker,
    market_ticker,
    relative_har_tickers$sector_etf
  ))
  
  model_columns <- model_columns[
    model_columns %in% names(daily_rv_panel)
  ]
  
  model_nonfinite <- sapply(model_columns, function(ticker) {
    sum(!is.finite(daily_rv_panel[[ticker]]))
  })
  
  cat("\nModel-column non-finite checks:\n")
  print(model_nonfinite[model_nonfinite > 0])
  
  # Check the final relative HAR ticker universe
  cat("\nRelative HAR ticker checks:\n")
  cat("Number of relative HAR tickers:", nrow(relative_har_tickers), "\n")
  
  for (ticker in removed_tickers) {
    cat(ticker, "in relative HAR tickers:",
        ticker %in% relative_har_tickers$ticker, "\n")
  }
  
  result <- list(
    rv_completeness = rv_completeness,
    problematic_rv_columns = problematic_rv_columns,
    benchmark_nonfinite = benchmark_nonfinite,
    model_nonfinite = model_nonfinite
  )
  
  return(result)
}



#_____________________________END_OF_THE_SCRIPT_________________________________