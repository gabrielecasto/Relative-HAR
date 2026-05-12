
# In this section we will identify the corresponding sectors for each ticker and
# assign a sector ETF to each ticker.

# Importing sectors from Wikipedia
GetSp500SectorsWikipedia <- function(tickers) {
  
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  tbl <- url %>% rvest::read_html() %>% rvest::html_table() %>% .[[1]]
  
  # If tickers is a list, convert it into a vector
  tickers <- unlist(tickers)
  
  # Keep only tickers and sectors
  sector_map <- tbl %>%
    dplyr::select(
      ticker = Symbol,
      sector = `GICS Sector`
    ) %>%
    dplyr::distinct(ticker, .keep_all = TRUE)
  
  # Create final table with all input tickers and corresponding sectors
  ticker_sector_table <- tibble::tibble(ticker = tickers) %>%
    dplyr::left_join(sector_map, by = "ticker")
  
  return(ticker_sector_table)
}

# Associate an ETF to each sector
GetSectorETFMap <- function() {
  
  sector_etf_map <- tibble::tibble(
    
    sector = c("Industrials", "Health Care", "Information Technology",
                "Utilities", "Financials", "Materials",
                "Consumer Discretionary", "Real Estate",
                "Communication Services", "Consumer Staples", "Energy"),
    
    sector_etf = c("XLI", "XLV", "XLK", "XLU", "XLF", "XLB", "XLY", "XLRE",
                    "XLC", "XLP", "XLE")
    
    )
  
    return(sector_etf_map)
}

# Add ETF for each ticker
AddSectorEtf <- function(ticker_sector_table, sector_etf_map) {
  
  ticker_sector_table <- ticker_sector_table %>%
    dplyr::left_join(sector_etf_map, by="sector")
  
  return(ticker_sector_table)
  
}






