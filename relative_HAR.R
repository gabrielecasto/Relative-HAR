# In this section we will import minute level data. Firstly we will import
# tickers of SP500 stocks and then we will generate the data frame with aligned
# data for each stock.



#___________________________IMPORT_SP500_TICKERS________________________________

# Improt SP500 tickers using Wikipedia as a primary source, if the number of
# imported tickers falls below 400, we use tidyquant as a fallback to import
# tickers.

# Wikipedia (primary)
GetSp500TickersWikipedia <- function() {
  url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
  tbl <- url %>% rvest::read_html() %>% rvest::html_table() %>% .[[1]]
  tickers <- as.list(tbl$Symbol)
  return(tickers)
}

# tidyquant (fallback)
GetSp500TickersTidyquant <- function() {
  tidyquant::tq_index("SP500") %>%
    dplyr::pull(symbol) %>%
    unique()
}

# Wrapper
GetSp500Tickers <- function(verbose = TRUE) {
  x <- try(GetSp500TickersWikipedia(), silent = TRUE)
  if (!inherits(x, "try-error") && length(x) >= 400) {
    if (verbose) message("Using tickers from Wikipedia")
    return(x)
  }
  if (verbose) message("Fallback: using tidyquant")
  y <- GetSp500TickersTidyquant()
  return(y)
}



#_________________________POLYGON_IMPORT_FUNCTIONS______________________________

# Function to import minute level data from polygon, downloading groups of
# three months

GetIntradayRange <- function(ticker, from_date, to_date, multiplier = 1,
                             timespan = "minute", api_key = POLYGON_KEY,
                             verbose = FALSE) {
  
  from_date <- as.Date(from_date)
  to_date <- as.Date(to_date)
  from <- format(from_date, "%Y-%m-%d")
  to <- format(to_date, "%Y-%m-%d")
  
  url <- sprintf(
    "https://api.polygon.io/v2/aggs/ticker/%s/range/%s/%s/%s/%s",
    URLencode(ticker, TRUE), multiplier, timespan, from, to
  )
  
  res <- httr::RETRY(
    "GET", url,
    query = list(apiKey = api_key, adjusted = "true", limit = 50000),
    times = 5, pause_base = 1, pause_cap = 10, pause_min = 0.5
  )
  
  if (httr::status_code(res) != 200) {
    message("Error ", httr::status_code(res), " for ", ticker, " in range ",
            from, " - ", to)
    return(tibble::tibble())
  }
  
  js <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8"),
    simplifyDataFrame = TRUE
  )
  
  if (!("results" %in% names(js)) || !is.data.frame(js$results) ||
      nrow(js$results) == 0) {
    return(tibble::tibble())
  }
  
  df <- tibble::tibble(
    datetime = as.POSIXct(js$results$t / 1000, origin = "1970-01-01", tz =
                            "UTC") %>%
      lubridate::with_tz("America/New_York"),
    close = js$results$c
  ) %>%
    dplyr::mutate(
      ticker = ticker,
      h = lubridate::hour(datetime),
      m = lubridate::minute(datetime)
    ) %>%
    dplyr::filter(
      (h > 9 | (h == 9 & m >= 30)),
      h < 16
    ) %>%
    dplyr::select(ticker, datetime, close) %>%
    dplyr::arrange(datetime)
  
  return(df)
}

# Build a wide dataframe with imported data with aligned dates
BuildWideIntradayDf <- function(tickers, from_date, to_date, multiplier = 1,
                                timespan = "minute",
                                sleep_sec = 0, api_key = POLYGON_KEY,
                                verbose = FALSE,
                                Patch_Bad_Days = TRUE,
                                NA_Share_Threshold = 0.90,
                                Patch_Sleep_Sec = 0.05,
                                Max_Patches_Per_Quarter = 200) {
  
  # Trading days only
  dates <- tidyquant::tq_get("^GSPC", from = from_date, to = to_date,
                             get = "stock.prices") %>%
    dplyr::distinct(date) %>%
    dplyr::pull(date)
  
  ym <- paste0(lubridate::year(dates), "-Q", lubridate::quarter(dates))
  quarterx <- unique(ym)
  
  wide_month_list <- vector("list", length(quarterx))
  names(wide_month_list) <- quarterx
  
  for (m in quarterx) {
    
    month_dates <- dates[ym == m]
    if (length(month_dates) == 0) next
    
    from_d <- min(month_dates)
    to_d <- max(month_dates)
    
    if (verbose) message(m, " | ", from_d, " -> ", to_d, " ===")
    
    # Download in parallel by ticker for THIS MONTH
    res <- future.apply::future_lapply(
      tickers,
      function(sym) {
        Sys.sleep(sleep_sec)
        
        df <- try(
          GetIntradayRange(
            ticker = sym,
            from_date = from_d,
            to_date = to_d,
            multiplier = multiplier,
            timespan = timespan,
            api_key = api_key,
            verbose = verbose
          ),
          silent = TRUE
        )
        
        if (inherits(df, "try-error") || nrow(df) == 0) {
          if (verbose) message("No data for ", sym, " in Q ", m)
          return(NULL)
        }
        
        # Keep only trading days of this month (safety)
        df <- df %>%
          dplyr::mutate(date = as.Date(datetime)) %>%
          dplyr::filter(date %in% month_dates) %>%
          dplyr::select(datetime, ticker, close)
        
        df
      }
    )
    
    df_long_month <- dplyr::bind_rows(res)
    rm(res)
    
    if (nrow(df_long_month) == 0) {
      wide_month_list[[m]] <- NULL
      next
    }
    
    # Order once and pivot to wide for this month only
    df_long_month <- df_long_month %>% dplyr::arrange(datetime)
    data.table::setDT(df_long_month)
    df_long_month <- unique(df_long_month, by = c("datetime", "ticker"))
    
    df_wide_month <- data.table::dcast(
      df_long_month,
      datetime ~ ticker,
      value.var = "close"
    )
    
    
    
    #____________________________PATCH_BAD_TICKER_DAYS______________________________
    
    # In this section we patch extreme missingness that can arise from intermittent
    # API failures in large parallel downloads. We identify (date, ticker) pairs
    # where a ticker is almost entirely missing for that day, and we re-download
    # only that day for that ticker (day-only request). This is RAM- and
    # storage-efficient because we never rebuild a large long table and we patch
    # only a small number of problematic pairs.
    
    if (Patch_Bad_Days) {
      
      data.table::setDT(df_wide_month)
      df_wide_month[, date := as.Date(datetime)]
      
      ticker_cols <- setdiff(names(df_wide_month), c("datetime", "date"))
      unique_days <- unique(df_wide_month$date)
      
      # Collect bad (date, ticker) pairs without reshaping the whole
      # dataset to long
      bad_pairs <- vector("list", length(unique_days))
      k <- 1L
      
      for (d0 in unique_days) {
        rows_d <- df_wide_month$date == d0
        
        # Compute NA share per ticker for this day (small slice: ~390 rows)
        na_share <- colMeans(is.na(as.data.frame(df_wide_month[rows_d,
                                                               ..ticker_cols])))
        
        bad_tickers <- names(na_share)[na_share >= NA_Share_Threshold]
        
        if (length(bad_tickers) > 0) {
          bad_pairs[[k]] <- data.table::data.table(date = d0, ticker = 
                                                     bad_tickers)
          k <- k + 1L
        }
      }
      
      bad_pairs <- data.table::rbindlist(bad_pairs, use.names = TRUE)
      
      if (!is.null(bad_pairs) && nrow(bad_pairs) > 0) {
        
        # Cap the number of patches to avoid runaway API usage
        if (nrow(bad_pairs) > Max_Patches_Per_Quarter) {
          bad_pairs <- bad_pairs[1:Max_Patches_Per_Quarter]
          if (verbose) message("Patch cap reached: patching only first ", 
                               Max_Patches_Per_Quarter, " pairs.")
        }
        
        if (verbose) message("Patching ", nrow(bad_pairs), 
                             " (date, ticker) pairs with NA-share >= ", 
                             NA_Share_Threshold, " ...")
        
        # Patch sequentially to reduce RAM pressure and avoid API rate spikes
        for (i in seq_len(nrow(bad_pairs))) {
          d1 <- bad_pairs$date[i]
          sym <- bad_pairs$ticker[i]
          
          Sys.sleep(Patch_Sleep_Sec)
          
          df_day <- try(
            GetIntradayRange(
              ticker = sym,
              from_date = d1,
              to_date = d1,
              multiplier = multiplier,
              timespan = timespan,
              api_key = api_key,
              verbose = FALSE
            ),
            silent = TRUE
          )
          
          if (inherits(df_day, "try-error") || nrow(df_day) == 0) next
          
          # Keep only trading minutes and align to the wide timestamps
          data.table::setDT(df_day)
          df_day <- df_day[, .(datetime, close)]
          data.table::setkey(df_day, datetime)
          
          idx_day <- which(df_wide_month$date == d1)
          if (length(idx_day) == 0) next
          
          dt_slice <- df_wide_month[idx_day, .(datetime)]
          data.table::setkey(dt_slice, datetime)
          
          # Join on datetime and overwrite the ticker column for this day
          dt_slice[df_day, value := i.close]
          df_wide_month[idx_day, (sym) := dt_slice$value]
          
          rm(df_day, dt_slice, idx_day); invisible(gc())
        }
      }
      
      # Remove helper column
      df_wide_month[, date := NULL]
    }
    
    
    rm(df_long_month)
    
    wide_month_list[[m]] <- df_wide_month
    rm(df_wide_month)
  }
  
  # Combine months (faster + less copying)
  out <- data.table::rbindlist(purrr::compact(wide_month_list), fill = TRUE)
  
  # Deduplicate & sort
  out <- unique(out, by = "datetime")
  data.table::setorder(out, datetime)
  
  return(out)
}



#_____________________________END_OF_THE_SCRIPT_________________________________