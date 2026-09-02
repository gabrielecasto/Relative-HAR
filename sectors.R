


# In this section we identify the sector associated with each SP500 ticker and
# assign the corresponding sector ETF to each stock.



#_________________________IMPORT_SP500_SECTORS__________________________________

# This function imports the SP500 constituents table from Wikipedia and matches
# each input ticker to its corresponding GICS sector.

GetSp500SectorsWikipedia <- function(tickers) {
  
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  tbl <- url %>% rvest::read_html() %>% rvest::html_table() %>% .[[1]]
  
  # If tickers is stored as a list, convert it into a vector
  tickers <- unlist(tickers)
  
  # Keep only ticker symbols and GICS sectors from the Wikipedia table
  sector_map <- tbl %>%
    dplyr::select(
      ticker = Symbol,
      sector = `GICS Sector`
    ) %>%
    dplyr::distinct(ticker, .keep_all = TRUE)
  
  # Create final table with all input tickers and their corresponding sectors
  ticker_sector_table <- tibble::tibble(ticker = tickers) %>%
    dplyr::left_join(sector_map, by = "ticker")
  
  return(ticker_sector_table)
}



#__________________________DEFINE_SECTOR_ETF_MAP________________________________

# This function manually defines the ETF associated with each GICS sector.

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



#_____________________________ADD_SECTOR_ETF____________________________________

# This function adds the corresponding sector ETF to each ticker by joining the
# ticker-sector table with the sector-ETF mapping.

AddSectorEtf <- function(ticker_sector_table, sector_etf_map) {
  
  ticker_sector_table <- ticker_sector_table %>%
    dplyr::left_join(sector_etf_map, by="sector")
  
  return(ticker_sector_table)
  
}



#_____________________________END_OF_THE_SCRIPT_________________________________