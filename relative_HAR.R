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



