
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
      Symbol,
      sector = `GICS Sector`
    ) %>%
    dplyr::distinct(Symbol, .keep_all = TRUE)
  
  # Create final table with all input tickers and corresponding sectors
  ticker_sector_table <- tibble::tibble(Symbol = tickers) %>%
    dplyr::left_join(sector_map, by = "Symbol")
  
  return(ticker_sector_table)
}

# Finding ETFs