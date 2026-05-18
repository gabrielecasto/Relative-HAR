


#_______________________RELATIVE_HAR_SIGNIFICANCE_TESTS_________________________

# This function tests whether the relative HAR model significantly improves or
# underperforms against selected benchmark models. Starting from the long
# LOSS_PANEL, it computes model-level loss differences for each ticker and
# training window, runs a Diebold-Mariano-West test with Newey-West variance,
# and returns both stock-level results and training-window summaries.

RunRelativeHARSignificanceTests <- function(loss_panel, relative_model,
                                            benchmark_models, metrics, alpha,
                                            nw_lag) {
  
  loss_panel <- as.data.frame(loss_panel)
  
  # Check that the required columns are available
  required_cols <- unique(c("training_window", "model", "ticker", "sector",
                            "target_date", metrics))
  
  if (!all(required_cols %in% names(loss_panel))) {
    stop("loss_panel does not contain the required columns.",
         call. = FALSE)
  }
  
  # Check that all requested models are available
  model_names <- unique(as.character(loss_panel$model))
  requested_models <- c(relative_model, benchmark_models)
  missing_models <- setdiff(requested_models, model_names)
  
  if (length(missing_models) > 0) {
    stop(paste("Missing models:", paste(missing_models, collapse = ", ")),
         call. = FALSE)
  }
  
  # Check that the Newey-West lag is valid, if manually supplied
  if (!is.null(nw_lag) &&
      (length(nw_lag) != 1 || !is.finite(nw_lag) ||
       nw_lag < 0 || nw_lag != floor(nw_lag))) {
    stop("nw_lag must be NULL or a non-negative integer.", call. = FALSE)
  }
  
  # Standardize key variables
  loss_panel$training_window <- as.integer(loss_panel$training_window)
  loss_panel$model <- as.character(loss_panel$model)
  loss_panel$ticker <- as.character(loss_panel$ticker)
  loss_panel$sector <- as.character(loss_panel$sector)
  loss_panel$target_date <- as.Date(loss_panel$target_date)
  
  # Internal helper: compute one DMW test from a loss-difference series.
  # Positive differences mean that relative HAR has lower losses.
  ComputeDMWTest <- function(difference_values) {
    
    d <- as.numeric(difference_values)
    d <- d[is.finite(d)]
    n_obs <- length(d)
    
    mean_difference <- mean(d)
    
    lag_used <- if (is.null(nw_lag)) {
      floor(4 * (n_obs / 100)^(2 / 9))
    } else {
      as.integer(nw_lag)
    }
    
    lag_used <- min(max(as.integer(lag_used), 0L), n_obs - 1L)
    
    centered_d <- d - mean_difference
    long_run_variance <- sum(centered_d * centered_d) / n_obs
    
    if (lag_used > 0L) {
      
      for (lag in seq_len(lag_used)) {
        
        weight <- 1 - lag / (lag_used + 1)
        autocovariance <- sum(
          centered_d[(lag + 1):n_obs] * centered_d[1:(n_obs - lag)]
        ) / n_obs
        
        long_run_variance <- long_run_variance +
          2 * weight * autocovariance
      }
    }
    
    if (!is.finite(long_run_variance) || long_run_variance <= 0) {
      return(list(
        n_obs = n_obs,
        nw_lag = lag_used,
        mean_difference = mean_difference,
        long_run_variance = long_run_variance,
        dmw_stat = NA_real_,
        p_improvement = NA_real_,
        p_underperformance = NA_real_,
        test_result = "invalid_variance",
        test_status = "invalid"
      ))
    }
    
    dmw_stat <- sqrt(n_obs) * mean_difference / sqrt(long_run_variance)
    
    p_improvement <- stats::pnorm(dmw_stat, lower.tail = FALSE)
    p_underperformance <- stats::pnorm(dmw_stat)
    
    test_result <- dplyr::case_when(
      mean_difference > 0 && p_improvement < alpha ~
        "significant_improvement",
      mean_difference < 0 && p_underperformance < alpha ~
        "significant_underperformance",
      TRUE ~ "not_significant"
    )
    
    return(list(
      n_obs = n_obs,
      nw_lag = lag_used,
      mean_difference = mean_difference,
      long_run_variance = long_run_variance,
      dmw_stat = dmw_stat,
      p_improvement = p_improvement,
      p_underperformance = p_underperformance,
      test_result = test_result,
      test_status = "valid"
    ))
  }
  
  # Run pairwise DMW tests for each benchmark and each loss metric
  stock_results <- list()
  counter <- 1L
  
  for (metric in metrics) {
    
    metric_name <- ifelse(metric == "squared_error", "mse", metric)
    
    for (benchmark_model in benchmark_models) {
      
      benchmark_df <- loss_panel[
        loss_panel$model == benchmark_model,
        c("training_window", "ticker", "sector", "target_date", metric)
      ]
      
      relative_df <- loss_panel[
        loss_panel$model == relative_model,
        c("training_window", "ticker", "sector", "target_date", metric)
      ]
      
      names(benchmark_df)[names(benchmark_df) == metric] <- "benchmark_loss"
      names(relative_df)[names(relative_df) == metric] <- "relative_loss"
      
      comparison_df <- dplyr::inner_join(
        benchmark_df,
        relative_df,
        by = c("training_window", "ticker", "target_date"),
        suffix = c("_benchmark", "_relative")
      )
      
      comparison_df$sector <- ifelse(
        !is.na(comparison_df$sector_relative) &
          nzchar(comparison_df$sector_relative),
        comparison_df$sector_relative,
        comparison_df$sector_benchmark
      )
      
      comparison_df$difference <- comparison_df$benchmark_loss -
        comparison_df$relative_loss
      
      comparison_df <- comparison_df[
        is.finite(comparison_df$benchmark_loss) &
          is.finite(comparison_df$relative_loss) &
          is.finite(comparison_df$difference),
      ]
      
      group_list <- split(
        comparison_df,
        list(comparison_df$training_window, comparison_df$ticker),
        drop = TRUE
      )
      
      for (group_df in group_list) {
        
        group_df <- group_df[order(group_df$target_date), ]
        
        dmw_result <- ComputeDMWTest(group_df$difference)
        
        sector_values <- unique(as.character(group_df$sector))
        sector_values <- sector_values[
          !is.na(sector_values) & nzchar(sector_values)
        ]
        
        sector_value <- ifelse(length(sector_values) > 0,
                               sector_values[1],
                               NA_character_)
        
        mean_benchmark_loss <- mean(group_df$benchmark_loss, na.rm = TRUE)
        mean_relative_loss <- mean(group_df$relative_loss, na.rm = TRUE)
        
        pct_loss_reduction <- ifelse(
          is.finite(mean_benchmark_loss) && mean_benchmark_loss != 0,
          100 * dmw_result$mean_difference / mean_benchmark_loss,
          NA_real_
        )
        
        stock_results[[counter]] <- data.frame(
          training_window = unique(group_df$training_window)[1],
          benchmark_model = benchmark_model,
          relative_model = relative_model,
          metric = metric_name,
          metric_column = metric,
          ticker = unique(group_df$ticker)[1],
          sector = sector_value,
          n_obs = dmw_result$n_obs,
          nw_lag = dmw_result$nw_lag,
          mean_benchmark_loss = mean_benchmark_loss,
          mean_relative_loss = mean_relative_loss,
          mean_difference = dmw_result$mean_difference,
          pct_loss_reduction = pct_loss_reduction,
          long_run_variance = dmw_result$long_run_variance,
          dmw_stat = dmw_result$dmw_stat,
          p_improvement = dmw_result$p_improvement,
          p_underperformance = dmw_result$p_underperformance,
          test_result = dmw_result$test_result,
          test_status = dmw_result$test_status,
          stringsAsFactors = FALSE
        )
        
        counter <- counter + 1L
      }
    }
  }
  
  stock_tests <- as.data.frame(data.table::rbindlist(
    stock_results,
    use.names = TRUE,
    fill = TRUE
  ))
  
  # Summarize the percentage of significant improvements and underperformances
  summary_by_window <- stock_tests %>%
    dplyr::filter(test_status == "valid") %>%
    dplyr::group_by(training_window, benchmark_model, relative_model, metric) %>%
    dplyr::summarise(
      n_tickers = dplyr::n_distinct(ticker),
      n_significant_improvement =
        sum(test_result == "significant_improvement"),
      n_significant_underperformance =
        sum(test_result == "significant_underperformance"),
      n_not_significant = sum(test_result == "not_significant"),
      pct_significant_improvement =
        100 * n_significant_improvement / n_tickers,
      pct_significant_underperformance =
        100 * n_significant_underperformance / n_tickers,
      pct_not_significant =
        100 * n_not_significant / n_tickers,
      mean_pct_loss_reduction = mean(pct_loss_reduction, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    stock_tests = stock_tests,
    summary_by_window = summary_by_window
  ))
}



#___________PLOT_RELATIVE_HAR_SIGNIFICANCE_ACROSS_TRAINING_WINDOWS_____________

# This function plots the percentage of stocks for which the relative HAR model
# significantly improves or underperforms against selected benchmark models.
# Starting from the summary table produced by RunRelativeHARSignificanceTests(),
# it builds one line plot across training windows, with separate panels for
# MSE and QLIKE.

PlotRelativeHARSignificance <- function(summary_by_window, test_type,
                                        output_path = NULL, width = 9,
                                        height = 7) {
  
  summary_by_window <- as.data.frame(summary_by_window)
  
  # Check that test_type is valid
  test_type <- match.arg(
    test_type,
    choices = c("improvement", "underperformance")
  )
  
  # Check that the required columns are available
  required_cols <- c(
    "training_window",
    "benchmark_model",
    "metric",
    "pct_significant_improvement",
    "pct_significant_underperformance"
  )
  
  if (!all(required_cols %in% names(summary_by_window))) {
    stop("summary_by_window does not contain the required columns.",
         call. = FALSE)
  }
  
  # Select the percentage column and plot labels
  if (test_type == "improvement") {
    
    percentage_col <- "pct_significant_improvement"
    plot_title <- paste0("Significant Improvement of Relative HAR ",
      "Across Training Windows")
    plot_subtitle <- paste0("Percentage of stocks with significant ",
      "DMW improvement")
    y_label <- "Stocks with significant improvement (%)"
    
  } else {
    
    percentage_col <- "pct_significant_underperformance"
    plot_title <- paste0("Significant Underperformance of Relative HAR ",
      "Across Training Windows")
    plot_subtitle <- paste0("Percentage of stocks with significant ",
      "DMW underperformance")
    y_label <- "Stocks with significant underperformance (%)"
  }
  
  # Define stable benchmark order, labels, colors and line types
  benchmark_order <- c("HAR", "HAR_X_MARKET_SECTOR")
  
  benchmark_labels <- c(
    HAR = "Relative HAR vs HAR",
    HAR_X_MARKET_SECTOR = "Relative HAR vs HAR-X Market-Sector"
  )
  
  benchmark_linetypes <- c(
    HAR = "solid",
    HAR_X_MARKET_SECTOR = "dashed"
  )
  
  benchmark_colors <- c(
    HAR = "grey35",
    HAR_X_MARKET_SECTOR = "black"
  )
  
  # Prepare the plot dataframe
  plot_df <- summary_by_window %>%
    dplyr::mutate(
      training_window = as.numeric(training_window),
      benchmark_model = as.character(benchmark_model),
      metric = as.character(metric),
      metric_label = dplyr::case_when(
        metric == "mse" ~ "MSE",
        metric == "qlike" ~ "QLIKE",
        TRUE ~ metric
      ),
      significance_pct = as.numeric(.data[[percentage_col]])
    ) %>%
    dplyr::filter(
      is.finite(training_window),
      is.finite(significance_pct),
      benchmark_model %in% benchmark_order
    )
  
  # Stop if no valid observations remain for plotting
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the significance plot.",
         call. = FALSE)
  }
  
  # Apply stable benchmark order
  plot_df$benchmark_model <- factor(
    plot_df$benchmark_model,
    levels = benchmark_order
  )
  
  # Build the significance plot
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = training_window,
      y = significance_pct,
      group = benchmark_model,
      color = benchmark_model,
      linetype = benchmark_model
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ metric_label, ncol = 1) +
    ggplot2::scale_color_manual(
      values = benchmark_colors,
      labels = benchmark_labels,
      drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = benchmark_linetypes,
      labels = benchmark_labels,
      drop = FALSE
    ) +
    ggplot2::scale_x_continuous(
      breaks = sort(unique(plot_df$training_window))
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, by = 20)
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = "Training window",
      y = y_label,
      color = "Comparison",
      linetype = "Comparison"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Save the plot if an output path is provided
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    
    ggplot2::ggsave(
      filename = output_path,
      plot = p,
      width = width,
      height = height
    )
  }
  
  return(p)
}