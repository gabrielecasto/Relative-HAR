# In this section we compare the forecasting performance of the standard HAR
# model and the relative HAR model. Forecasts are matched by ticker and target
# date, then percentage errors are computed on the log realized variance scale.



#________________________BUILD_MODEL_COMPARISON_PANEL___________________________

# This function builds a common comparison panel for standard HAR and relative
# HAR forecasts. Percentage errors are computed directly on the log realized
# variance scale.

BuildModelComparisonPanel <- function(har_forecast_errors,
                                      relative_har_forecast_errors,
                                      min_denominator = 1e-8) {
  
  har_forecast_errors <- as.data.frame(har_forecast_errors)
  relative_har_forecast_errors <- as.data.frame(relative_har_forecast_errors)
  
  required_cols <- c("ticker", "target_date", "actual", "forecast")
  
  if (!all(required_cols %in% names(har_forecast_errors))) {
    stop("har_forecast_errors does not contain the required columns.",
         call. = FALSE)
  }
  
  if (!all(c(required_cols, "sector", "sector_etf") %in%
           names(relative_har_forecast_errors))) {
    stop("relative_har_forecast_errors does not contain the required columns.",
         call. = FALSE)
  }
  
  har_df <- har_forecast_errors %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      target_date = as.Date(target_date)
    ) %>%
    dplyr::select(
      ticker,
      target_date,
      actual_har = actual,
      forecast_har = forecast
    )
  
  relative_df <- relative_har_forecast_errors %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      sector = as.character(sector),
      sector_etf = as.character(sector_etf),
      target_date = as.Date(target_date)
    ) %>%
    dplyr::select(
      ticker,
      sector,
      sector_etf,
      target_date,
      actual_relative = actual,
      forecast_relative = forecast
    )
  
  comparison_df <- relative_df %>%
    dplyr::inner_join(har_df, by = c("ticker", "target_date")) %>%
    dplyr::mutate(
      actual_difference = actual_relative - actual_har,
      
      percentage_error_har = dplyr::if_else(
        abs(actual_har) > min_denominator,
        100 * abs(actual_har - forecast_har) / abs(actual_har),
        NA_real_
      ),
      
      percentage_error_relative = dplyr::if_else(
        abs(actual_relative) > min_denominator,
        100 * abs(actual_relative - forecast_relative) / abs(actual_relative),
        NA_real_
      )
    )
  
  if (nrow(comparison_df) == 0) {
    stop("No common ticker-date observations between HAR and relative HAR.",
         call. = FALSE)
  }
  
  if (max(abs(comparison_df$actual_difference), na.rm = TRUE) > 1e-8) {
    warning("Actual values differ between HAR and relative HAR for some rows.")
  }
  
  comparison_long <- dplyr::bind_rows(
    
    comparison_df %>%
      dplyr::transmute(
        ticker,
        sector,
        sector_etf,
        target_date,
        model = "HAR",
        actual = actual_har,
        forecast = forecast_har,
        percentage_error = percentage_error_har
      ),
    
    comparison_df %>%
      dplyr::transmute(
        ticker,
        sector,
        sector_etf,
        target_date,
        model = "Relative HAR",
        actual = actual_relative,
        forecast = forecast_relative,
        percentage_error = percentage_error_relative
      )
  )
  
  return(list(
    paired = comparison_df,
    long = comparison_long
  ))
}



#_____________________PLOT_PERCENTAGE_ERRORS_OVER_TIME__________________________

# This function plots sector-level percentage forecast errors over time.
# For each sector and date, errors are averaged across stocks.

PlotPercentageErrorsOverTimeBySector <- function(model_comparison,
                                                 facet_ncol = 3,
                                                 output_path = NULL) {
  
  plot_df <- model_comparison$long %>%
    dplyr::filter(
      is.finite(percentage_error),
      !is.na(sector)
    ) %>%
    dplyr::group_by(sector, target_date, model) %>%
    dplyr::summarise(
      mean_percentage_error = mean(percentage_error, na.rm = TRUE),
      .groups = "drop"
    )
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = target_date,
      y = mean_percentage_error,
      color = model
    )
  ) +
    ggplot2::geom_line(linewidth = 0.55, alpha = 0.9, na.rm = TRUE) +
    ggplot2::facet_wrap(~ sector, scales = "free_y", ncol = facet_ncol) +
    ggplot2::scale_color_manual(
      values = c(
        "HAR" = "#1F4E79",
        "Relative HAR" = "#B03A2E"
      )
    ) +
    ggplot2::scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m"
    ) +
    ggplot2::labs(
      title = "Forecast Error Comparison by Sector",
      subtitle = "Daily mean percentage error on the log realized variance scale",
      x = NULL,
      y = "Mean percentage error (%)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 11, color = "grey35"),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2,
                                               color = "grey85"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(output_path, p, width = 13, height = 8)
  }
  
  return(p)
}



#____________________PLOT_PERCENTAGE_ERROR_HISTOGRAMS___________________________

# This function plots overlapping histograms of percentage forecast errors by
# sector for the standard HAR and relative HAR models.

PlotPercentageErrorHistogramsBySector <- function(model_comparison,
                                                  bins = 45,
                                                  cap_quantile = 0.99,
                                                  facet_ncol = 3,
                                                  output_path = NULL) {
  
  plot_df <- model_comparison$long %>%
    dplyr::filter(
      is.finite(percentage_error),
      !is.na(sector)
    )
  
  if (!is.null(cap_quantile)) {
    
    error_cap <- stats::quantile(
      plot_df$percentage_error,
      probs = cap_quantile,
      na.rm = TRUE
    )
    
    plot_df <- plot_df %>%
      dplyr::filter(percentage_error <= error_cap)
  }
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = percentage_error,
      fill = model,
      color = model
    )
  ) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(density)),
      bins = bins,
      alpha = 0.35,
      position = "identity",
      linewidth = 0.25
    ) +
    ggplot2::facet_wrap(~ sector, scales = "free_y", ncol = facet_ncol) +
    ggplot2::scale_fill_manual(
      values = c(
        "HAR" = "#1F4E79",
        "Relative HAR" = "#B03A2E"
      )
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "HAR" = "#1F4E79",
        "Relative HAR" = "#B03A2E"
      )
    ) +
    ggplot2::labs(
      title = "Distribution of Percentage Forecast Errors by Sector",
      subtitle = "Errors computed on the log realized variance scale; top 1% capped for readability",
      x = "Percentage forecast error (%)",
      y = "Density",
      fill = NULL,
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 15),
      plot.subtitle = ggplot2::element_text(size = 11, color = "grey35"),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(linewidth = 0.2,
                                               color = "grey85"),
      axis.text.x = ggplot2::element_text(size = 8),
      axis.text.y = ggplot2::element_text(size = 8)
    )
  
  if (!is.null(output_path)) {
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(output_path, p, width = 13, height = 8)
  }
  
  return(p)
}



#_____________________________END_OF_THE_SCRIPT_________________________________