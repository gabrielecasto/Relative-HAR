


# In this section we estimate HAR, HAR-X market-sector and relative HAR models
# across different training windows. We store aggregate loss measures (across
# all stocks and across by sector) and plot them.



#_______________ESTIMATE_AND_CHECK_MODELS_ONE_TRAINING_WINDOW__________________

# This function estimates HAR, HAR-X market-sector and relative HAR models for
# one selected training window. It assumes that the daily log RV dataframe has
# already been built in main.R. The function runs model checks for the selected
# training window only. If a check fails, the function stops immediately.
# If all checks pass, it returns the original model outputs.

EstimateAndCheckModelsOneTrainingWindow <- function(daily_log_rv_wide,
                                                    relative_har_tickers,
                                                    ticker_sector_table,
                                                    training_window,
                                                    market_ticker = "SPY",
                                                    first_lag = 1,
                                                    second_lag = 5,
                                                    third_lag = 22,
                                                    minimum_observations = 30,
                                                    tolerance = 1e-10) {
  
  daily_log_rv_wide <- as.data.frame(daily_log_rv_wide)
  relative_har_tickers <- as.data.frame(relative_har_tickers)
  ticker_sector_table <- as.data.frame(ticker_sector_table)
  
  # Check that training_window is a single positive integer
  if (length(training_window) != 1 ||
      !is.finite(training_window) ||
      training_window < 1 ||
      training_window != floor(training_window)) {
    stop("training_window must be a single positive integer.", call. = FALSE)
  }
  
  # Use the same stock universe for all models
  stock_tickers <- as.character(relative_har_tickers$ticker)
  
  cat("\nEstimating models with training window:", training_window, "\n")
  
  # Estimate standard HAR model
  har_errors <- RollingHARForecastPanel(
    daily_log_rv_wide = daily_log_rv_wide,
    tickers = stock_tickers,
    training_window = training_window,
    first_lag = first_lag,
    second_lag = second_lag,
    third_lag = third_lag
  )
  
  # Estimate HAR-X market-sector model
  har_x_errors <- RollingHARXMarketSectorForecastPanel(
    daily_log_rv_wide = daily_log_rv_wide,
    relative_har_tickers = relative_har_tickers,
    training_window = training_window,
    market_ticker = market_ticker,
    first_lag = first_lag,
    second_lag = second_lag,
    third_lag = third_lag
  )
  
  # Estimate relative HAR model
  relative_har_errors <- RollingRelativeHARForecastPanel(
    daily_log_rv_wide = daily_log_rv_wide,
    relative_har_tickers = relative_har_tickers,
    training_window = training_window,
    market_ticker = market_ticker,
    first_lag = first_lag,
    second_lag = second_lag,
    third_lag = third_lag,
    minimum_observations = minimum_observations
  )
  
  # Store the original model outputs
  model_outputs <- list(
    HAR = har_errors,
    HAR_X_MARKET_SECTOR = har_x_errors,
    RELATIVE_HAR = relative_har_errors
  )
  
  # Run alignment checks for this training window only
  alignment_checks <- CheckForecastOutputAlignment(
    model_outputs = model_outputs,
    reference_model = "HAR",
    tolerance = tolerance,
    verbose = FALSE
  )
  
  if (!isTRUE(alignment_checks$passed)) {
    print(alignment_checks$check_summary)
    stop(
      paste("Model output alignment failed for training window",
            training_window),
      call. = FALSE
    )
  }
  
  # Run metadata checks for this training window only
  metadata_checks <- CheckModelMetadataConsistency(
    alignment_checks = alignment_checks,
    ticker_sector_table = ticker_sector_table,
    market_ticker = market_ticker,
    verbose = FALSE
  )
  
  if (!isTRUE(metadata_checks$passed)) {
    print(metadata_checks$check_summary)
    stop(
      paste("Model metadata checks failed for training window",
            training_window),
      call. = FALSE
    )
  }
  
  cat("All checks passed for training window:", training_window, "\n")
  
  result <- list(
    training_window = training_window,
    model_outputs = model_outputs
  )
  
  return(result)
}



#_____________________BUILD_LOSS_TABLES_ONE_TRAINING_WINDOW_____________________

# This function builds loss tables for one selected training window. Starting
# from checked model outputs, it creates one long loss panel and then computes
# average losses overall and by sector.

BuildLossTablesOneTrainingWindow <- function(one_window_result,
                                             ticker_sector_table,
                                             rv_interval_minutes = 30) {
  
  # Extract model outputs and training window
  model_outputs <- one_window_result$model_outputs
  training_window <- one_window_result$training_window
  ticker_sector_table <- as.data.frame(ticker_sector_table)
  
  # Build one long dataframe with all model forecasts and losses
  loss_panel <- data.table::rbindlist(
    lapply(names(model_outputs), function(model_name) {
      
      model_df <- as.data.frame(model_outputs[[model_name]])
      
      # Add sector information to models that do not store it directly
      if (!"sector" %in% names(model_df)) {
        model_df <- dplyr::left_join(
          model_df,
          ticker_sector_table[, c("ticker", "sector")],
          by = "ticker"
        )
      }
      
      data.frame(
        training_window = training_window,
        rv_interval_minutes = rv_interval_minutes,
        model = model_name,
        ticker = model_df$ticker,
        sector = model_df$sector,
        forecast_origin_date = model_df$forecast_origin_date,
        target_date = model_df$target_date,
        actual = model_df$actual,
        forecast = model_df$forecast,
        squared_error = model_df$squared_error,
        absolute_error = model_df$absolute_error,
        qlike = model_df$qlike,
        stringsAsFactors = FALSE
      )
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  loss_panel <- as.data.frame(loss_panel)
  
  # Compute losses first at ticker level
  ticker_losses <- loss_panel %>%
    dplyr::group_by(training_window, rv_interval_minutes, model, ticker,
                    sector) %>%
    dplyr::summarise(
      mse = mean(squared_error, na.rm = TRUE),
      mae = mean(absolute_error, na.rm = TRUE),
      qlike = mean(qlike, na.rm = TRUE),
      n_forecasts = dplyr::n(),
      .groups = "drop"
    )
  
  # Compute overall model losses by averaging across tickers
  overall_losses <- ticker_losses %>%
    dplyr::group_by(training_window, rv_interval_minutes, model) %>%
    dplyr::summarise(
      mse = mean(mse, na.rm = TRUE),
      mae = mean(mae, na.rm = TRUE),
      qlike = mean(qlike, na.rm = TRUE),
      n_tickers = dplyr::n_distinct(ticker),
      n_forecasts = sum(n_forecasts),
      .groups = "drop"
    )
  
  # Compute sector-level losses by averaging across tickers within each sector
  sector_losses <- ticker_losses %>%
    dplyr::group_by(training_window, rv_interval_minutes, sector, model) %>%
    dplyr::summarise(
      mse = mean(mse, na.rm = TRUE),
      mae = mean(mae, na.rm = TRUE),
      qlike = mean(qlike, na.rm = TRUE),
      n_tickers = dplyr::n_distinct(ticker),
      n_forecasts = sum(n_forecasts),
      .groups = "drop"
    )
  
  return(list(
    loss_panel = loss_panel,
    ticker_losses = ticker_losses,
    overall_losses = overall_losses,
    sector_losses = sector_losses
  ))
}



#____________RUN_MODEL_COMPARISON_ACROSS_TRAINING_WINDOWS______________________

# This function runs the full model comparison across different training
# windows. It does not rebuild realized volatility and does not set workers.
# For each training window, it estimates and checks the models, builds the loss
# tables, and stores the results.

RunModelComparisonAcrossTrainingWindows <- function(daily_log_rv_wide,
                                                    relative_har_tickers,
                                                    ticker_sector_table,
                                                    training_windows,
                                                    rv_interval_minutes = 30,
                                                    market_ticker = "SPY",
                                                    first_lag = 1,
                                                    second_lag = 5,
                                                    third_lag = 22,
                                                    minimum_observations = 30,
                                                    tolerance = 1e-10) {
  
  # Check that training windows are valid positive integers
  if (length(training_windows) == 0 ||
      any(!is.finite(training_windows)) ||
      any(training_windows < 1) ||
      any(training_windows != floor(training_windows))) {
    stop("training_windows must contain positive integers.", call. = FALSE)
  }
  
  training_windows <- unique(as.integer(training_windows))
  
  # Prepare storage lists
  all_loss_panels <- list()
  all_ticker_losses <- list()
  all_overall_losses <- list()
  all_sector_losses <- list()
  
  # Loop across training windows
  for (training_window in training_windows) {
    
    window_name <- paste0("training_window_", training_window)
    
    cat("\nRunning model comparison for training window:",
        training_window, "\n")
    
    # Estimate models and stop immediately if checks fail
    one_window_result <- EstimateAndCheckModelsOneTrainingWindow(
      daily_log_rv_wide = daily_log_rv_wide,
      relative_har_tickers = relative_har_tickers,
      ticker_sector_table = ticker_sector_table,
      training_window = training_window,
      market_ticker = market_ticker,
      first_lag = first_lag,
      second_lag = second_lag,
      third_lag = third_lag,
      minimum_observations = minimum_observations,
      tolerance = tolerance
    )
    
    # Build loss tables for this training window
    loss_tables <- BuildLossTablesOneTrainingWindow(
      one_window_result = one_window_result,
      ticker_sector_table = ticker_sector_table,
      rv_interval_minutes = rv_interval_minutes
    )
    
    # Store results
    all_loss_panels[[window_name]] <- loss_tables$loss_panel
    all_ticker_losses[[window_name]] <- loss_tables$ticker_losses
    all_overall_losses[[window_name]] <- loss_tables$overall_losses
    all_sector_losses[[window_name]] <- loss_tables$sector_losses
    
    cat("Completed training window:", training_window, "\n")
    
    rm(one_window_result, loss_tables)
    invisible(gc())
  }
  
  # Combine results across all training windows
  result <- list(
    loss_panel = as.data.frame(data.table::rbindlist(
      all_loss_panels,
      use.names = TRUE,
      fill = TRUE
    )),
    
    ticker_losses = as.data.frame(data.table::rbindlist(
      all_ticker_losses,
      use.names = TRUE,
      fill = TRUE
    )),
    
    overall_losses = as.data.frame(data.table::rbindlist(
      all_overall_losses,
      use.names = TRUE,
      fill = TRUE
    )),
    
    sector_losses = as.data.frame(data.table::rbindlist(
      all_sector_losses,
      use.names = TRUE,
      fill = TRUE
    ))
  )
  
  return(result)
}



#____________PLOT_MODEL_LOSS_CURVES_ACROSS_TRAINING_WINDOWS____________________

# This function plots model loss curves across different training windows.
# It produces three overall plots and three sector-level plots, one for each
# loss measure: MSE, MAE and QLIKE.

PlotModelLossCurvesAcrossTrainingWindows <- function(overall_losses,
                                                     sector_losses,
                                                     output_dir, save_plots,
                                                     overall_x_breaks,
                                                     sector_x_breaks) {
  
  overall_losses <- as.data.frame(overall_losses)
  sector_losses <- as.data.frame(sector_losses)
  
  # Check that the required columns are available
  required_overall_cols <- c("training_window", "model", "mse", "mae", "qlike")
  required_sector_cols <- c("training_window", "sector", "model",
                            "mse", "mae", "qlike")
  
  if (!all(required_overall_cols %in% names(overall_losses))) {
    stop("overall_losses does not contain the required columns.",
         call. = FALSE)
  }
  
  if (!all(required_sector_cols %in% names(sector_losses))) {
    stop("sector_losses does not contain the required columns.",
         call. = FALSE)
  }
  
  # Create output folder if plots are saved
  if (save_plots) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Define stable model order, colors and line types
  model_order <- c("HAR", "HAR_X_MARKET_SECTOR", "RELATIVE_HAR")
  
  model_linetypes <- c(
    HAR = "solid",
    HAR_X_MARKET_SECTOR = "dashed",
    RELATIVE_HAR = "solid"
  )
  
  model_shapes <- c(
    HAR = 16,                  # circle
    HAR_X_MARKET_SECTOR = 17,  # triangle
    RELATIVE_HAR = 4           # cross
  )
  
  model_colors <- c(
    HAR = "grey60",
    HAR_X_MARKET_SECTOR = "grey35",
    RELATIVE_HAR = "black"
  )
  
  overall_losses$model <- factor(overall_losses$model, levels = model_order)
  sector_losses$model <- factor(sector_losses$model, levels = model_order)
  
  # Define loss measures to plot
  metrics <- c("mse", "mae", "qlike")
  
  metric_labels <- c(
    mse = "MSE",
    mae = "MAE",
    qlike = "QLIKE"
  )
  
  overall_plots <- list()
  sector_plots <- list()
  
  # Build one combined overall plot with MSE, MAE and QLIKE in separate panels
  overall_combined_df <- overall_losses %>%
    tidyr::pivot_longer(
      cols = c(mse, mae, qlike),
      names_to = "metric",
      values_to = "loss_value"
    ) %>%
    dplyr::mutate(
      training_window = as.numeric(training_window),
      loss_value = as.numeric(loss_value),
      metric_label = dplyr::case_when(
        metric == "mse" ~ "MSE",
        metric == "mae" ~ "MAE",
        metric == "qlike" ~ "QLIKE",
        TRUE ~ metric
      )
    ) %>%
    dplyr::filter(
      is.finite(training_window),
      is.finite(loss_value)
    )
  
  p_overall_combined <- ggplot2::ggplot(
    overall_combined_df,
    ggplot2::aes(
      x = training_window,
      y = loss_value,
      group = model,
      color = model,
      linetype = model,
      shape = model
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2.5, stroke = 1) +
    ggplot2::facet_wrap(~ metric_label, ncol = 1, scales = "free_y") +
    ggplot2::scale_color_manual(values = model_colors, drop = FALSE) +
    ggplot2::scale_linetype_manual(values = model_linetypes, drop = FALSE) +
    ggplot2::scale_shape_manual(values = model_shapes, drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = overall_x_breaks) +
    ggplot2::labs(
      title = "Overall Losses Across Training Windows",
      subtitle = "Average losses across all stocks",
      x = "Training window",
      y = "Loss value",
      color = "Model",
      linetype = "Model",
      shape = "Model"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if (save_plots) {
    ggplot2::ggsave(
      filename = file.path(output_dir,
                           "overall_losses_training_window_combined.pdf"),
      plot = p_overall_combined,
      width = 9,
      height = 14
    )
  }
  
  # Build one overall plot and one sector plot for each loss measure
  for (metric in metrics) {
    
    # Prepare overall data
    overall_plot_df <- overall_losses %>%
      dplyr::mutate(
        training_window = as.numeric(training_window),
        loss_value = as.numeric(.data[[metric]])
      ) %>%
      dplyr::filter(
        is.finite(training_window),
        is.finite(loss_value)
      )
    
    # Prepare sector-level data
    sector_plot_df <- sector_losses %>%
      dplyr::mutate(
        training_window = as.numeric(training_window),
        sector = as.character(sector),
        loss_value = as.numeric(.data[[metric]])
      ) %>%
      dplyr::filter(
        is.finite(training_window),
        is.finite(loss_value),
        !is.na(sector),
        nzchar(sector)
      )
    
    # Plot overall model losses
    p_overall <- ggplot2::ggplot(
      overall_plot_df,
      ggplot2::aes(
        x = training_window,
        y = loss_value,
        group = model,
        color = model,
        linetype = model,
        shape = model
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(values = model_colors, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = model_linetypes, drop = FALSE) +
      ggplot2::scale_shape_manual(values = model_shapes, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = overall_x_breaks) +
      ggplot2::labs(
        title = paste0("Overall ", metric_labels[[metric]],
                       " Across Training Windows"),
        subtitle = "Average losses across all stocks",
        x = "Training window",
        y = metric_labels[[metric]],
        color = "Model",
        linetype = "Model",
        shape = "Model"
      ) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
    
    # Plot sector-level model losses
    p_sector <- ggplot2::ggplot(
      sector_plot_df,
      ggplot2::aes(
        x = training_window,
        y = loss_value,
        group = model,
        color = model,
        linetype = model,
        shape = model
      )
    ) +
      ggplot2::geom_line(linewidth = 0.5) +
      ggplot2::geom_point(size = 1.2) +
      ggplot2::scale_color_manual(values = model_colors, drop = FALSE) +
      ggplot2::scale_linetype_manual(values = model_linetypes, drop = FALSE) +
      ggplot2::scale_shape_manual(values = model_shapes, drop = FALSE) +
      ggplot2::scale_x_continuous(breaks = sector_x_breaks) +
      ggplot2::facet_wrap(~ sector, scales = "free_y") +
      ggplot2::labs(
        title = paste0("Sector-Level ", metric_labels[[metric]],
                       " Across Training Windows"),
        subtitle = "Average losses by sector",
        x = "Training window",
        y = metric_labels[[metric]],
        color = "Model",
        linetype = "Model",
        shape = "Model"
      ) +
      ggplot2::theme_minimal(base_size = 18) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        strip.text = ggplot2::element_text(face = "bold"),
        legend.position = "bottom",
        panel.grid.minor = ggplot2::element_blank()
      )
    
    # Store plots
    overall_plots[[metric]] <- p_overall
    sector_plots[[metric]] <- p_sector
    
    # Save plots if requested
    if (save_plots) {
      
      ggplot2::ggsave(
        filename = file.path(output_dir,
                             paste0("overall_", metric,
                                    "_training_window.pdf")),
        plot = p_overall,
        width = 9,
        height = 6
      )
      
      ggplot2::ggsave(
        filename = file.path(output_dir,
                             paste0("sector_", metric,
                                    "_training_window.pdf")),
        plot = p_sector,
        width = 15,
        height = 10
      )
    }
  }
  
  return(list(
    overall_combined_plot = p_overall_combined,
    overall_plots = overall_plots,
    sector_plots = sector_plots
  ))
}



#_____________________________END_OF_THE_SCRIPT_________________________________