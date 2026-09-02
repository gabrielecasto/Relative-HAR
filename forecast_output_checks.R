


# In this section we check the integrity and alignment of the forecast-error
# outputs produced by the different realized volatility forecasting models.
# The checks are performed before any model performance comparison. First, each
# model output is inspected separately to verify that the required columns are
# available, dates and tickers are correctly formatted, forecast errors are
# internally consistent, and no duplicated ticker-date observations are present.
# Then, the outputs of all models are compared to ensure that they refer to the
# same ticker universe, target dates, ticker-date pairs, realized actual values
# and forecast origins. Finally, sector-level metadata are checked for
# consistency across models and, when available, against the reference
# ticker-sector table.



#_________________________CHECK_SINGLE_MODEL_OUTPUT_____________________________

# This function checks the internal consistency of one model forecast-error
# dataframe. It verifies that the required output columns exist, converts
# tickers, dates and numerical variables to comparable formats, checks for
# missing or non-finite values, detects duplicated ticker-target-date pairs,
# verifies that forecast origins precede target dates, and checks that the
# stored forecast-error measures are internally consistent.

CheckSingleModelOutput <- function(forecast_errors,
                                   model_name,
                                   tolerance = 1e-10,
                                   verbose = TRUE) {
  
  forecast_errors <- as.data.frame(forecast_errors)
  
  # Check that model_name is a valid non-empty string
  model_name <- as.character(model_name)[1]
  
  if (is.na(model_name) || !nzchar(model_name)) {
    stop("model_name must be a valid non-empty string.", call. = FALSE)
  }
  
  # Check that the dataframe is not empty
  if (nrow(forecast_errors) == 0) {
    stop(paste(model_name, "forecast_errors is empty."), call. = FALSE)
  }
  
  # Check that the required columns exist
  required_cols <- c(
    "ticker",
    "forecast_origin_date",
    "target_date",
    "actual",
    "forecast",
    "error",
    "squared_error",
    "absolute_error",
    "qlike"
  )
  
  if (!all(required_cols %in% names(forecast_errors))) {
    
    missing_cols <- setdiff(required_cols, names(forecast_errors))
    
    stop(
      paste0(
        model_name,
        " forecast_errors does not contain the required columns: ",
        paste(missing_cols, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }
  
  # Convert tickers and dates to comparable formats
  forecast_errors$ticker <- as.character(forecast_errors$ticker)
  forecast_errors$forecast_origin_date <- 
    as.Date(forecast_errors$forecast_origin_date)
  forecast_errors$target_date <- as.Date(forecast_errors$target_date)
  
  # Convert core numerical columns to numeric format
  numeric_cols <- c(
    "actual",
    "forecast",
    "error",
    "squared_error",
    "absolute_error",
    "qlike"
  )
  
  for (column_name in numeric_cols) {
    forecast_errors[[column_name]] <- suppressWarnings(
      as.numeric(as.character(forecast_errors[[column_name]]))
    )
  }
  
  # Build ticker-target-date identifiers
  forecast_errors$pair_id <- paste(
    forecast_errors$ticker,
    forecast_errors$target_date,
    sep = "__"
  )
  
  # Check missing key values
  missing_key_rows <- is.na(forecast_errors$ticker) |
    !nzchar(forecast_errors$ticker) |
    is.na(forecast_errors$forecast_origin_date) |
    is.na(forecast_errors$target_date)
  
  n_missing_key_rows <- sum(missing_key_rows)
  
  # Check non-finite values in the core numerical columns
  finite_matrix <- sapply(numeric_cols, function(column_name) {
    is.finite(forecast_errors[[column_name]])
  })
  
  nonfinite_rows_index <- !apply(finite_matrix, 1, all)
  n_nonfinite_rows <- sum(nonfinite_rows_index)
  
  nonfinite_rows <- forecast_errors[
    nonfinite_rows_index,
    c("ticker", "forecast_origin_date", "target_date", numeric_cols)
  ]
  
  # Check duplicated ticker-target-date pairs
  pair_counts <- table(forecast_errors$pair_id)
  duplicated_pair_ids <- names(pair_counts[pair_counts > 1])
  
  duplicate_pairs <- data.frame(
    pair_id = duplicated_pair_ids,
    n_rows = as.integer(pair_counts[duplicated_pair_ids]),
    stringsAsFactors = FALSE
  )
  
  if (nrow(duplicate_pairs) > 0) {
    
    pair_parts <- strsplit(duplicate_pairs$pair_id, "__", fixed = TRUE)
    
    duplicate_pairs$ticker <- vapply(pair_parts, `[`, character(1), 1)
    duplicate_pairs$target_date <- as.Date(
      vapply(pair_parts, `[`, character(1), 2)
    )
    
    duplicate_pairs <- duplicate_pairs[
      , c("ticker", "target_date", "n_rows", "pair_id")
    ]
  }
  
  n_duplicate_pairs <- nrow(duplicate_pairs)
  
  # Check that forecast origins precede target dates
  invalid_date_order <- forecast_errors$forecast_origin_date >=
    forecast_errors$target_date
  
  invalid_date_order[is.na(invalid_date_order)] <- TRUE
  n_invalid_date_order <- sum(invalid_date_order)
  
  # Recompute the forecast-error measures from actual and forecast
  expected_error <- forecast_errors$actual - forecast_errors$forecast
  expected_squared_error <- expected_error^2
  expected_absolute_error <- abs(expected_error)
  expected_qlike <- exp(expected_error) - expected_error - 1
  
  # Check whether stored values match recomputed values
  error_mismatch <- abs(forecast_errors$error - expected_error) > tolerance
  squared_error_mismatch <- abs(
    forecast_errors$squared_error - expected_squared_error
  ) > tolerance
  absolute_error_mismatch <- abs(
    forecast_errors$absolute_error - expected_absolute_error
  ) > tolerance
  qlike_mismatch <- abs(forecast_errors$qlike - expected_qlike) > tolerance
  
  error_mismatch[is.na(error_mismatch)] <- TRUE
  squared_error_mismatch[is.na(squared_error_mismatch)] <- TRUE
  absolute_error_mismatch[is.na(absolute_error_mismatch)] <- TRUE
  qlike_mismatch[is.na(qlike_mismatch)] <- TRUE
  
  inconsistent_rows_index <- error_mismatch |
    squared_error_mismatch |
    absolute_error_mismatch |
    qlike_mismatch
  
  n_error_mismatch <- sum(error_mismatch)
  n_squared_error_mismatch <- sum(squared_error_mismatch)
  n_absolute_error_mismatch <- sum(absolute_error_mismatch)
  n_qlike_mismatch <- sum(qlike_mismatch)
  n_inconsistent_rows <- sum(inconsistent_rows_index)
  
  inconsistent_rows <- forecast_errors[
    inconsistent_rows_index,
    c("ticker", "forecast_origin_date", "target_date", numeric_cols)
  ]
  
  # Compute maximum numerical differences for diagnostics
  max_error_difference <- max(
    abs(forecast_errors$error - expected_error),
    na.rm = TRUE
  )
  
  max_squared_error_difference <- max(
    abs(forecast_errors$squared_error - expected_squared_error),
    na.rm = TRUE
  )
  
  max_absolute_error_difference <- max(
    abs(forecast_errors$absolute_error - expected_absolute_error),
    na.rm = TRUE
  )
  
  max_qlike_difference <- max(
    abs(forecast_errors$qlike - expected_qlike),
    na.rm = TRUE
  )
  
  # Build one compact model-level summary
  model_summary <- data.frame(
    model_name = model_name,
    n_rows = nrow(forecast_errors),
    n_tickers = length(unique(forecast_errors$ticker)),
    n_target_dates = length(unique(forecast_errors$target_date)),
    first_target_date = min(forecast_errors$target_date, na.rm = TRUE),
    last_target_date = max(forecast_errors$target_date, na.rm = TRUE),
    n_ticker_date_pairs = length(unique(forecast_errors$pair_id)),
    n_missing_key_rows = n_missing_key_rows,
    n_nonfinite_rows = n_nonfinite_rows,
    n_duplicate_pairs = n_duplicate_pairs,
    n_invalid_date_order = n_invalid_date_order,
    n_inconsistent_rows = n_inconsistent_rows,
    stringsAsFactors = FALSE
  )
  
  # Build the check summary table
  check_summary <- data.frame(
    check_name = c(
      "Required columns",
      "Missing key values",
      "Finite numerical values",
      "Duplicated ticker-date pairs",
      "Forecast origin before target date",
      "Error identity",
      "Squared error identity",
      "Absolute error identity",
      "QLIKE identity"
    ),
    status = c(
      "PASS",
      ifelse(n_missing_key_rows == 0, "PASS", "FAIL"),
      ifelse(n_nonfinite_rows == 0, "PASS", "FAIL"),
      ifelse(n_duplicate_pairs == 0, "PASS", "FAIL"),
      ifelse(n_invalid_date_order == 0, "PASS", "FAIL"),
      ifelse(n_error_mismatch == 0, "PASS", "FAIL"),
      ifelse(n_squared_error_mismatch == 0, "PASS", "FAIL"),
      ifelse(n_absolute_error_mismatch == 0, "PASS", "FAIL"),
      ifelse(n_qlike_mismatch == 0, "PASS", "FAIL")
    ),
    severity = c(
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal"
    ),
    details = c(
      "All required columns are available.",
      paste("Rows with missing ticker or date keys:", n_missing_key_rows),
      paste("Rows with non-finite core numerical values:", n_nonfinite_rows),
      paste("Duplicated ticker-target-date pairs:", n_duplicate_pairs),
      paste("Rows with forecast_origin_date >= target_date:",
            n_invalid_date_order),
      paste("Error mismatches:", n_error_mismatch,
            "| max difference:", max_error_difference),
      paste("Squared error mismatches:", n_squared_error_mismatch,
            "| max difference:", max_squared_error_difference),
      paste("Absolute error mismatches:", n_absolute_error_mismatch,
            "| max difference:", max_absolute_error_difference),
      paste("QLIKE mismatches:", n_qlike_mismatch,
            "| max difference:", max_qlike_difference)
    ),
    stringsAsFactors = FALSE
  )
  
  # Define the final pass/fail status
  passed <- all(check_summary$status == "PASS")
  
  # Print a compact diagnostic output, if requested
  if (verbose) {
    
    cat("\nSingle model output checks:", model_name, "\n")
    cat("Passed:", passed, "\n")
    
    cat("\nModel summary:\n")
    print(model_summary)
    
    cat("\nCheck summary:\n")
    print(check_summary)
  }
  
  # Store all diagnostics in a list for later inspection
  result <- list(
    model_name = model_name,
    passed = passed,
    model_summary = model_summary,
    check_summary = check_summary,
    duplicate_pairs = duplicate_pairs,
    nonfinite_rows = nonfinite_rows,
    inconsistent_rows = inconsistent_rows,
    clean_output = forecast_errors
  )
  
  return(result)
}



#_______________________CHECK_FORECAST_OUTPUT_ALIGNMENT_________________________

# This function checks whether several model forecast-error outputs are aligned.
# It first applies CheckSingleModelOutput() to each model separately. Then, it
# compares ticker universes, target-date universes, ticker-target-date pairs,
# realized actual values and forecast-origin dates across all models. The goal
# is to verify that all models are evaluated on the same forecast panel before
# any statistical or graphical performance comparison is performed.

CheckForecastOutputAlignment <- function(model_outputs,
                                         reference_model = NULL,
                                         tolerance = 1e-10,
                                         verbose = TRUE) {
  
  # Check that model_outputs is a named list with at least two models
  if (!is.list(model_outputs)) {
    stop("model_outputs must be a named list of forecast-error dataframes.",
         call. = FALSE)
  }
  
  if (length(model_outputs) < 2) {
    stop("model_outputs must contain at least two model outputs.",
         call. = FALSE)
  }
  
  model_names <- names(model_outputs)
  
  if (is.null(model_names) ||
      any(is.na(model_names)) ||
      any(!nzchar(model_names))) {
    stop("model_outputs must be a named list with non-empty model names.",
         call. = FALSE)
  }
  
  if (any(duplicated(model_names))) {
    stop("model_outputs contains duplicated model names.", call. = FALSE)
  }
  
  # Check that the single-model check function is available
  if (!exists("CheckSingleModelOutput")) {
    stop(
      paste0(
        "CheckSingleModelOutput() is not available. ",
        "Please source the file containing it before calling this function."
      ),
      call. = FALSE
    )
  }
  
  # Define the reference model used for pairwise diagnostic counts
  if (is.null(reference_model)) {
    reference_model <- model_names[1]
  }
  
  reference_model <- as.character(reference_model)[1]
  
  if (is.na(reference_model) || !reference_model %in% model_names) {
    stop("reference_model must be one of the names in model_outputs.",
         call. = FALSE)
  }
  
  # Apply the single-model output check to each model
  single_model_checks <- lapply(model_names, function(model_name) {
    
    CheckSingleModelOutput(
      forecast_errors = model_outputs[[model_name]],
      model_name = model_name,
      tolerance = tolerance,
      verbose = FALSE
    )
  })
  
  names(single_model_checks) <- model_names
  
  # Extract the cleaned and standardized model outputs
  clean_outputs <- lapply(single_model_checks, function(check_object) {
    check_object$clean_output
  })
  
  names(clean_outputs) <- model_names
  
  # Make sure that pair_id exists in all cleaned outputs
  clean_outputs <- lapply(clean_outputs, function(model_output) {
    
    model_output <- as.data.frame(model_output)
    
    if (!"pair_id" %in% names(model_output)) {
      model_output$pair_id <- paste(
        model_output$ticker,
        model_output$target_date,
        sep = "__"
      )
    }
    
    return(model_output)
  })
  
  # Collect the single-model summaries
  model_summary <- data.table::rbindlist(
    lapply(single_model_checks, function(check_object) {
      check_object$model_summary
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  model_summary <- as.data.frame(model_summary)
  rownames(model_summary) <- NULL
  
  # Check whether all individual model outputs passed their own checks
  individual_passed <- all(sapply(single_model_checks, function(check_object) {
    isTRUE(check_object$passed)
  }))
  
  # Extract ticker, date and ticker-date pair sets for each model
  ticker_sets <- lapply(clean_outputs, function(model_output) {
    sort(unique(model_output$ticker))
  })
  
  date_sets <- lapply(clean_outputs, function(model_output) {
    sort(unique(model_output$target_date))
  })
  
  pair_sets <- lapply(clean_outputs, function(model_output) {
    sort(unique(model_output$pair_id))
  })
  
  names(ticker_sets) <- model_names
  names(date_sets) <- model_names
  names(pair_sets) <- model_names
  
  # Define reference sets
  reference_tickers <- ticker_sets[[reference_model]]
  reference_dates <- date_sets[[reference_model]]
  reference_pairs <- pair_sets[[reference_model]]
  
  # Check whether all models have the same ticker universe
  same_ticker_universe <- all(sapply(model_names, function(model_name) {
    identical(ticker_sets[[model_name]], reference_tickers)
  }))
  
  # Check whether all models have the same target-date universe
  same_date_universe <- all(sapply(model_names, function(model_name) {
    identical(date_sets[[model_name]], reference_dates)
  }))
  
  # Check whether all models have the same ticker-target-date universe
  same_pair_universe <- all(sapply(model_names, function(model_name) {
    identical(pair_sets[[model_name]], reference_pairs)
  }))
  
  # Build ticker-level alignment summary
  ticker_alignment_summary <- data.frame(
    model_name = model_names,
    n_tickers = sapply(model_names, function(model_name) {
      length(ticker_sets[[model_name]])
    }),
    n_missing_tickers_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(reference_tickers, ticker_sets[[model_name]]))
    }),
    n_extra_tickers_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(ticker_sets[[model_name]], reference_tickers))
    }),
    stringsAsFactors = FALSE
  )
  
  rownames(ticker_alignment_summary) <- NULL
  
  # Build date-level alignment summary
  date_alignment_summary <- data.frame(
    model_name = model_names,
    n_target_dates = sapply(model_names, function(model_name) {
      length(date_sets[[model_name]])
    }),
    first_target_date = as.Date(sapply(model_names, function(model_name) {
      min(date_sets[[model_name]], na.rm = TRUE)
    }), origin = "1970-01-01"),
    last_target_date = as.Date(sapply(model_names, function(model_name) {
      max(date_sets[[model_name]], na.rm = TRUE)
    }), origin = "1970-01-01"),
    n_missing_dates_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(reference_dates, date_sets[[model_name]]))
    }),
    n_extra_dates_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(date_sets[[model_name]], reference_dates))
    }),
    stringsAsFactors = FALSE
  )
  
  rownames(date_alignment_summary) <- NULL
  
  # Build ticker-date pair alignment summary
  pair_alignment_summary <- data.frame(
    model_name = model_names,
    n_ticker_date_pairs = sapply(model_names, function(model_name) {
      length(pair_sets[[model_name]])
    }),
    n_missing_pairs_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(reference_pairs, pair_sets[[model_name]]))
    }),
    n_extra_pairs_vs_reference = sapply(model_names, function(model_name) {
      length(setdiff(pair_sets[[model_name]], reference_pairs))
    }),
    stringsAsFactors = FALSE
  )
  
  rownames(pair_alignment_summary) <- NULL
  
  # Store detailed ticker, date and pair differences against the reference model
  ticker_differences <- lapply(model_names, function(model_name) {
    
    list(
      missing_from_model = setdiff(reference_tickers, ticker_sets[[model_name]]),
      extra_in_model = setdiff(ticker_sets[[model_name]], reference_tickers)
    )
  })
  
  names(ticker_differences) <- model_names
  
  date_differences <- lapply(model_names, function(model_name) {
    
    list(
      missing_from_model = setdiff(reference_dates, date_sets[[model_name]]),
      extra_in_model = setdiff(date_sets[[model_name]], reference_dates)
    )
  })
  
  names(date_differences) <- model_names
  
  pair_differences <- lapply(model_names, function(model_name) {
    
    list(
      missing_from_model = setdiff(reference_pairs, pair_sets[[model_name]]),
      extra_in_model = setdiff(pair_sets[[model_name]], reference_pairs)
    )
  })
  
  names(pair_differences) <- model_names
  
  # Define common and union ticker-date panels across all models
  common_tickers <- Reduce(intersect, ticker_sets)
  common_dates <- Reduce(intersect, date_sets)
  common_pairs <- Reduce(intersect, pair_sets)
  union_pairs <- Reduce(union, pair_sets)
  
  # Check whether the common ticker-date panel is balanced
  expected_common_pairs <- length(common_tickers) * length(common_dates)
  common_panel_balanced <- length(common_pairs) == expected_common_pairs
  
  common_panel_summary <- data.frame(
    n_common_tickers = length(common_tickers),
    n_common_target_dates = length(common_dates),
    n_common_pairs = length(common_pairs),
    n_union_pairs = length(union_pairs),
    expected_common_pairs_if_balanced = expected_common_pairs,
    common_panel_balanced = common_panel_balanced,
    stringsAsFactors = FALSE
  )
  
  # Build a long actual-value table for all models
  actual_long <- data.table::rbindlist(
    lapply(model_names, function(model_name) {
      
      model_output <- clean_outputs[[model_name]]
      
      data.frame(
        model_name = model_name,
        pair_id = model_output$pair_id,
        ticker = model_output$ticker,
        target_date = model_output$target_date,
        actual = model_output$actual,
        stringsAsFactors = FALSE
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  actual_long <- as.data.frame(actual_long)
  
  # Keep only pairs that are common to all models
  actual_long_common <- actual_long[
    actual_long$pair_id %in% common_pairs,
  ]
  
  # Check whether actual values coincide across models on the common panel
  if (nrow(actual_long_common) > 0) {
    
    data.table::setDT(actual_long_common)
    
    actual_consistency <- actual_long_common[
      ,
      .(
        ticker = ticker[1],
        target_date = target_date[1],
        n_models = data.table::uniqueN(model_name),
        actual_min = min(actual, na.rm = TRUE),
        actual_max = max(actual, na.rm = TRUE),
        actual_range = max(actual, na.rm = TRUE) - min(actual, na.rm = TRUE)
      ),
      by = pair_id
    ]
    
    actual_consistency <- as.data.frame(actual_consistency)
    
  } else {
    
    actual_consistency <- data.frame(
      pair_id = character(0),
      ticker = character(0),
      target_date = as.Date(character(0)),
      n_models = integer(0),
      actual_min = numeric(0),
      actual_max = numeric(0),
      actual_range = numeric(0),
      stringsAsFactors = FALSE
    )
  
  }
  
  actual_mismatches <- actual_consistency[
    actual_consistency$actual_range > tolerance,
  ]
  
  n_actual_mismatches <- nrow(actual_mismatches)
  
  if (nrow(actual_consistency) > 0) {
    max_actual_difference <- max(actual_consistency$actual_range, na.rm = TRUE)
  } else {
    max_actual_difference <- NA_real_
  }
  
  actual_mismatch_details <- actual_long_common[
    actual_long_common$pair_id %in% actual_mismatches$pair_id,
  ]
  
  # Build a long forecast-origin table for all models
  origin_long <- data.table::rbindlist(
    lapply(model_names, function(model_name) {
      
      model_output <- clean_outputs[[model_name]]
      
      data.frame(
        model_name = model_name,
        pair_id = model_output$pair_id,
        ticker = model_output$ticker,
        target_date = model_output$target_date,
        forecast_origin_date = model_output$forecast_origin_date,
        stringsAsFactors = FALSE
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  origin_long <- as.data.frame(origin_long)
  
  # Keep only pairs that are common to all models
  origin_long_common <- origin_long[
    origin_long$pair_id %in% common_pairs,
  ]
  
  # Check whether forecast origins coincide across models on the common panel
  if (nrow(origin_long_common) > 0) {
    
    data.table::setDT(origin_long_common)
    
    origin_consistency <- origin_long_common[
      ,
      .(
        ticker = ticker[1],
        target_date = target_date[1],
        n_models = data.table::uniqueN(model_name),
        first_origin_date = min(forecast_origin_date, na.rm = TRUE),
        last_origin_date = max(forecast_origin_date, na.rm = TRUE),
        origin_range_days = as.numeric(
          max(forecast_origin_date, na.rm = TRUE) -
            min(forecast_origin_date, na.rm = TRUE)
        )
      ),
      by = pair_id
    ]
    
    origin_consistency <- as.data.frame(origin_consistency)
    
  } else {
    
    origin_consistency <- data.frame(
      pair_id = character(0),
      ticker = character(0),
      target_date = as.Date(character(0)),
      n_models = integer(0),
      first_origin_date = as.Date(character(0)),
      last_origin_date = as.Date(character(0)),
      origin_range_days = numeric(0),
      stringsAsFactors = FALSE
    )
  }
  
  origin_mismatches <- origin_consistency[
    origin_consistency$origin_range_days > 0,
  ]
  
  n_origin_mismatches <- nrow(origin_mismatches)
  
  if (nrow(origin_consistency) > 0) {
    max_origin_difference_days <- max(
      origin_consistency$origin_range_days,
      na.rm = TRUE
    )
  } else {
    max_origin_difference_days <- NA_real_
  }
  
  origin_mismatch_details <- origin_long_common[
    origin_long_common$pair_id %in% origin_mismatches$pair_id,
  ]
  
  # Build the final check summary
  check_summary <- data.frame(
    check_name = c(
      "Individual model output checks",
      "Same ticker universe",
      "Same target-date universe",
      "Same ticker-date universe",
      "Non-empty common ticker-date panel",
      "Actual values identical on common pairs",
      "Forecast origins identical on common pairs",
      "Balanced common panel"
    ),
    status = c(
      ifelse(individual_passed, "PASS", "FAIL"),
      ifelse(same_ticker_universe, "PASS", "FAIL"),
      ifelse(same_date_universe, "PASS", "FAIL"),
      ifelse(same_pair_universe, "PASS", "FAIL"),
      ifelse(length(common_pairs) > 0, "PASS", "FAIL"),
      ifelse(n_actual_mismatches == 0, "PASS", "FAIL"),
      ifelse(n_origin_mismatches == 0, "PASS", "WARNING"),
      ifelse(common_panel_balanced, "PASS", "WARNING")
    ),
    severity = c(
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "warning",
      "warning"
    ),
    details = c(
      paste("All individual model checks passed:", individual_passed),
      paste("All models have the same ticker universe:",
            same_ticker_universe),
      paste("All models have the same target-date universe:",
            same_date_universe),
      paste("All models have the same ticker-target-date universe:",
            same_pair_universe),
      paste("Common ticker-date pairs:", length(common_pairs)),
      paste("Actual mismatches:", n_actual_mismatches,
            "| max difference:", max_actual_difference),
      paste("Forecast-origin mismatches:", n_origin_mismatches,
            "| max difference in days:", max_origin_difference_days),
      paste("Common panel balanced:", common_panel_balanced,
            "| observed pairs:", length(common_pairs),
            "| expected pairs:", expected_common_pairs)
    ),
    stringsAsFactors = FALSE
  )
  
  # Define the final pass/fail status using only fatal failures
  fatal_failures <- check_summary$status == "FAIL" &
    check_summary$severity == "fatal"
  
  passed <- !any(fatal_failures)
  
  # Print compact diagnostic output, if requested
  if (verbose) {
    
    cat("\nForecast output alignment checks\n")
    cat("Reference model:", reference_model, "\n")
    cat("Passed:", passed, "\n")
    
    cat("\nModel summary:\n")
    print(model_summary)
    
    cat("\nTicker alignment summary:\n")
    print(ticker_alignment_summary)
    
    cat("\nDate alignment summary:\n")
    print(date_alignment_summary)
    
    cat("\nTicker-date pair alignment summary:\n")
    print(pair_alignment_summary)
    
    cat("\nCommon panel summary:\n")
    print(common_panel_summary)
    
    cat("\nCheck summary:\n")
    print(check_summary)
  }
  
  # Store all diagnostics in a list for later inspection
  result <- list(
    model_names = model_names,
    reference_model = reference_model,
    passed = passed,
    check_summary = check_summary,
    single_model_checks = single_model_checks,
    clean_outputs = clean_outputs,
    model_summary = model_summary,
    ticker_alignment_summary = ticker_alignment_summary,
    date_alignment_summary = date_alignment_summary,
    pair_alignment_summary = pair_alignment_summary,
    ticker_differences = ticker_differences,
    date_differences = date_differences,
    pair_differences = pair_differences,
    common_tickers = common_tickers,
    common_target_dates = common_dates,
    common_pairs = common_pairs,
    union_pairs = union_pairs,
    common_panel_summary = common_panel_summary,
    actual_consistency = actual_consistency,
    actual_mismatches = actual_mismatches,
    actual_mismatch_details = actual_mismatch_details,
    origin_consistency = origin_consistency,
    origin_mismatches = origin_mismatches,
    origin_mismatch_details = origin_mismatch_details
  )
  
  return(result)
}



#_______________________CHECK_MODEL_METADATA_CONSISTENCY________________________

# This function checks whether model metadata are internally and cross-model
# consistent. It focuses on sector, sector ETF and market ticker information.
# Models without metadata columns are not treated as failed, because the
# standard HAR model may not store sector-level information. The function is
# useful before running sector-level model comparisons.

CheckModelMetadataConsistency <- function(alignment_checks,
                                          ticker_sector_table = NULL,
                                          market_ticker = "SPY",
                                          verbose = TRUE) {
  
  # Check that alignment_checks contains cleaned model outputs
  if (!is.list(alignment_checks) ||
      !"clean_outputs" %in% names(alignment_checks)) {
    stop("alignment_checks must contain an element named clean_outputs.",
         call. = FALSE)
  }
  
  clean_outputs <- alignment_checks$clean_outputs
  
  if (!is.list(clean_outputs) || length(clean_outputs) == 0) {
    stop("alignment_checks$clean_outputs must be a non-empty list.",
         call. = FALSE)
  }
  
  model_names <- names(clean_outputs)
  
  if (is.null(model_names) ||
      any(is.na(model_names)) ||
      any(!nzchar(model_names))) {
    stop("clean_outputs must be a named list.", call. = FALSE)
  }
  
  # Check that market_ticker is a valid non-empty string
  market_ticker <- as.character(market_ticker)[1]
  
  if (is.na(market_ticker) || !nzchar(market_ticker)) {
    stop("market_ticker must be a valid non-empty string.", call. = FALSE)
  }
  
  # Helper function: identify missing character values
  IsMissingCharacter <- function(x) {
    x <- as.character(x)
    is.na(x) | !nzchar(trimws(x))
  }
  
  # Helper function: count distinct non-missing values
  CountDistinctNonMissing <- function(x) {
    x <- as.character(x)
    x <- x[!IsMissingCharacter(x)]
    length(unique(x))
  }
  
  metadata_cols <- c("sector", "sector_etf", "market_ticker")
  
  # Identify models that contain sector metadata columns
  has_metadata_cols <- sapply(clean_outputs, function(model_output) {
    model_output <- as.data.frame(model_output)
    all(c("ticker", metadata_cols) %in% names(model_output))
  })
  
  metadata_model_names <- model_names[has_metadata_cols]
  
  # Build model-level metadata summaries
  metadata_summary_list <- lapply(model_names, function(model_name) {
    
    model_output <- as.data.frame(clean_outputs[[model_name]])
    
    if (!has_metadata_cols[[model_name]]) {
      
      return(data.frame(
        model_name = model_name,
        has_metadata_cols = FALSE,
        n_rows = nrow(model_output),
        n_tickers = length(unique(as.character(model_output$ticker))),
        n_missing_sector = NA_integer_,
        n_missing_sector_etf = NA_integer_,
        n_missing_market_ticker = NA_integer_,
        n_tickers_multiple_sector = NA_integer_,
        n_tickers_multiple_sector_etf = NA_integer_,
        n_tickers_multiple_market_ticker = NA_integer_,
        market_ticker_expected = NA,
        stringsAsFactors = FALSE
      ))
    }
    
    metadata_df <- model_output[, c("ticker", metadata_cols)]
    
    metadata_df$ticker <- as.character(metadata_df$ticker)
    metadata_df$sector <- as.character(metadata_df$sector)
    metadata_df$sector_etf <- as.character(metadata_df$sector_etf)
    metadata_df$market_ticker <- as.character(metadata_df$market_ticker)
    
    ticker_metadata_counts <- aggregate(
      metadata_df[, metadata_cols],
      by = list(ticker = metadata_df$ticker),
      FUN = CountDistinctNonMissing
    )
    
    valid_market_tickers <- metadata_df$market_ticker[
      !IsMissingCharacter(metadata_df$market_ticker)
    ]
    
    market_ticker_expected <- length(valid_market_tickers) > 0 &&
      all(valid_market_tickers == market_ticker)
    
    data.frame(
      model_name = model_name,
      has_metadata_cols = TRUE,
      n_rows = nrow(model_output),
      n_tickers = length(unique(metadata_df$ticker)),
      n_missing_sector = sum(IsMissingCharacter(metadata_df$sector)),
      n_missing_sector_etf = sum(IsMissingCharacter(metadata_df$sector_etf)),
      n_missing_market_ticker =
        sum(IsMissingCharacter(metadata_df$market_ticker)),
      n_tickers_multiple_sector =
        sum(ticker_metadata_counts$sector > 1),
      n_tickers_multiple_sector_etf =
        sum(ticker_metadata_counts$sector_etf > 1),
      n_tickers_multiple_market_ticker =
        sum(ticker_metadata_counts$market_ticker > 1),
      market_ticker_expected = market_ticker_expected,
      stringsAsFactors = FALSE
    )
  })
  
  metadata_summary <- data.table::rbindlist(
    metadata_summary_list,
    use.names = TRUE,
    fill = TRUE
  )
  
  metadata_summary <- as.data.frame(metadata_summary)
  rownames(metadata_summary) <- NULL
  
  # Build a long metadata table using only models with metadata columns
  if (length(metadata_model_names) > 0) {
    
    metadata_long <- data.table::rbindlist(
      lapply(metadata_model_names, function(model_name) {
        
        model_output <- as.data.frame(clean_outputs[[model_name]])
        
        metadata_df <- model_output[, c("ticker", metadata_cols)]
        
        metadata_df$model_name <- model_name
        metadata_df$ticker <- as.character(metadata_df$ticker)
        metadata_df$sector <- as.character(metadata_df$sector)
        metadata_df$sector_etf <- as.character(metadata_df$sector_etf)
        metadata_df$market_ticker <- as.character(metadata_df$market_ticker)
        
        metadata_df <- unique(metadata_df)
        
        metadata_df[, c(
          "model_name",
          "ticker",
          "sector",
          "sector_etf",
          "market_ticker"
        )]
      }),
      use.names = TRUE,
      fill = TRUE
    )
    
    metadata_long <- as.data.frame(metadata_long)
    rownames(metadata_long) <- NULL
    
  } else {
    
    metadata_long <- data.frame(
      model_name = character(0),
      ticker = character(0),
      sector = character(0),
      sector_etf = character(0),
      market_ticker = character(0),
      stringsAsFactors = FALSE
    )
  }
  
  # Check whether metadata are consistent across models
  if (nrow(metadata_long) > 0) {
    
    metadata_by_ticker <- aggregate(
      metadata_long[, metadata_cols],
      by = list(ticker = metadata_long$ticker),
      FUN = CountDistinctNonMissing
    )
    
    names(metadata_by_ticker) <- c(
      "ticker",
      "n_unique_sector",
      "n_unique_sector_etf",
      "n_unique_market_ticker"
    )
    
    metadata_mismatches <- metadata_by_ticker[
      metadata_by_ticker$n_unique_sector > 1 |
        metadata_by_ticker$n_unique_sector_etf > 1 |
        metadata_by_ticker$n_unique_market_ticker > 1,
    ]
    
    metadata_mismatch_details <- metadata_long[
      metadata_long$ticker %in% metadata_mismatches$ticker,
    ]
    
  } else {
    
    metadata_mismatches <- data.frame(
      ticker = character(0),
      n_unique_sector = integer(0),
      n_unique_sector_etf = integer(0),
      n_unique_market_ticker = integer(0),
      stringsAsFactors = FALSE
    )
    
    metadata_mismatch_details <- metadata_long
  }
  
  # Optional check against the reference ticker-sector table
  if (!is.null(ticker_sector_table)) {
    
    ticker_sector_table <- as.data.frame(ticker_sector_table)
    
    required_reference_cols <- c("ticker", "sector", "sector_etf")
    
    if (!all(required_reference_cols %in% names(ticker_sector_table))) {
      stop("ticker_sector_table must contain ticker, sector and sector_etf.",
           call. = FALSE)
    }
    
    reference_table <- ticker_sector_table[, required_reference_cols]
    
    reference_table$ticker <- as.character(reference_table$ticker)
    reference_table$sector <- as.character(reference_table$sector)
    reference_table$sector_etf <- as.character(reference_table$sector_etf)
    
    reference_duplicates <- reference_table[
      duplicated(reference_table$ticker) |
        duplicated(reference_table$ticker, fromLast = TRUE),
    ]
    
    reference_table <- reference_table[!duplicated(reference_table$ticker), ]
    
    metadata_reference <- merge(
      metadata_long,
      reference_table,
      by = "ticker",
      all.x = TRUE,
      suffixes = c("", "_reference")
    )
    
    missing_reference <- IsMissingCharacter(metadata_reference$sector_reference) &
      IsMissingCharacter(metadata_reference$sector_etf_reference)
    
    sector_mismatch <- !missing_reference &
      metadata_reference$sector != metadata_reference$sector_reference
    
    sector_etf_mismatch <- !missing_reference &
      metadata_reference$sector_etf != metadata_reference$sector_etf_reference
    
    sector_mismatch[is.na(sector_mismatch)] <- FALSE
    sector_etf_mismatch[is.na(sector_etf_mismatch)] <- FALSE
    
    reference_mismatches <- metadata_reference[
      missing_reference | sector_mismatch | sector_etf_mismatch,
    ]
    
  } else {
    
    reference_duplicates <- data.frame()
    reference_mismatches <- data.frame()
  }
  
  # Build compact check summary
  n_models_with_metadata <- sum(has_metadata_cols)
  n_missing_metadata_values <- sum(
    metadata_summary$n_missing_sector,
    metadata_summary$n_missing_sector_etf,
    metadata_summary$n_missing_market_ticker,
    na.rm = TRUE
  )
  
  n_internal_metadata_issues <- sum(
    metadata_summary$n_tickers_multiple_sector,
    metadata_summary$n_tickers_multiple_sector_etf,
    metadata_summary$n_tickers_multiple_market_ticker,
    na.rm = TRUE
  )
  
  market_ticker_ok <- all(
    metadata_summary$market_ticker_expected[
      metadata_summary$has_metadata_cols
    ],
    na.rm = TRUE
  )
  
  reference_check_status <- if (is.null(ticker_sector_table)) {
    "WARNING"
  } else if (nrow(reference_duplicates) == 0 &&
             nrow(reference_mismatches) == 0) {
    "PASS"
  } else {
    "FAIL"
  }
  
  check_summary <- data.frame(
    check_name = c(
      "Metadata available in at least one model",
      "No missing metadata values",
      "Unique metadata per ticker within each model",
      "Expected market ticker",
      "Metadata consistent across models",
      "Metadata consistent with reference table"
    ),
    status = c(
      ifelse(n_models_with_metadata > 0, "PASS", "WARNING"),
      ifelse(n_missing_metadata_values == 0, "PASS", "FAIL"),
      ifelse(n_internal_metadata_issues == 0, "PASS", "FAIL"),
      ifelse(market_ticker_ok, "PASS", "FAIL"),
      ifelse(nrow(metadata_mismatches) == 0, "PASS", "FAIL"),
      reference_check_status
    ),
    severity = c(
      "warning",
      "fatal",
      "fatal",
      "fatal",
      "fatal",
      "warning"
    ),
    details = c(
      paste("Models with metadata columns:", n_models_with_metadata),
      paste("Missing metadata values:", n_missing_metadata_values),
      paste("Ticker-level internal metadata issues:",
            n_internal_metadata_issues),
      paste("Expected market ticker:", market_ticker),
      paste("Tickers with cross-model metadata mismatches:",
            nrow(metadata_mismatches)),
      ifelse(
        is.null(ticker_sector_table),
        "Reference table not provided.",
        paste("Reference mismatches:", nrow(reference_mismatches),
              "| reference duplicated tickers:", nrow(reference_duplicates))
      )
    ),
    stringsAsFactors = FALSE
  )
  
  # Define final pass/fail status using only fatal failures
  fatal_failures <- check_summary$status == "FAIL" &
    check_summary$severity == "fatal"
  
  passed <- !any(fatal_failures)
  
  # Print compact diagnostic output, if requested
  if (verbose) {
    
    cat("\nModel metadata consistency checks\n")
    cat("Passed:", passed, "\n")
    
    cat("\nMetadata summary:\n")
    print(metadata_summary)
    
    cat("\nCheck summary:\n")
    print(check_summary)
    
    if (nrow(metadata_mismatches) > 0) {
      cat("\nMetadata mismatches across models:\n")
      print(metadata_mismatch_details)
    }
    
    if (!is.null(ticker_sector_table) && nrow(reference_mismatches) > 0) {
      cat("\nMetadata mismatches against reference table:\n")
      print(reference_mismatches)
    }
  }
  
  # Store all diagnostics in a list for later inspection
  result <- list(
    passed = passed,
    check_summary = check_summary,
    metadata_summary = metadata_summary,
    metadata_long = metadata_long,
    metadata_mismatches = metadata_mismatches,
    metadata_mismatch_details = metadata_mismatch_details,
    reference_duplicates = reference_duplicates,
    reference_mismatches = reference_mismatches
  )
  
  return(result)
}



#_____________________________END_OF_THE_SCRIPT_________________________________