


# In this section we prepare and plot the volatility signature results. Starting
# from the stock-level realized variance estimates computed at different
# sampling intervals, we normalize each stock by its own stable plateau level.
# Then, we build cross-sectional and sector-level summaries and produce plots
# that show how realized variance changes across sampling frequencies.



#_________________________PREPARE_SIGNATURE_PLOT_DATA___________________________

# This function prepares the volatility signature data for plotting.
# It normalizes each stock's mean RV by its own stable plateau level.
# The plateau is defined as the median mean RV over selected intervals,
# for example 10:30 minutes.
#
# Output columns include:
# - plateau_rv: stock-specific stable RV benchmark
# - rv_ratio: mean_rv / plateau_rv
# - sector information, if ticker_sector_table is provided

PrepareSignaturePlotData <- function(signature_by_stock,
                                     ticker_sector_table = NULL,
                                     plateau_intervals = 10:30,
                                     minimum_days_required = 1,
                                     drop_invalid_plateau = TRUE) {
  
  # Check that the required columns are available
  required_cols <- c("ticker", "interval_minutes", "mean_rv", "n_days")
  
  if (!all(required_cols %in% names(signature_by_stock))) {
    stop(
      paste0(
        "signature_by_stock must contain ticker, interval_minutes, ",
        "mean_rv and n_days."
      ),
      call. = FALSE
    )
  }
  
  # Prepare the signature dataframe and keep only valid RV observations
  signature_plot_df <- signature_by_stock %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      mean_rv = as.numeric(mean_rv),
      n_days = as.integer(n_days)
    ) %>%
    dplyr::filter(
      is.finite(mean_rv),
      mean_rv > 0,
      n_days >= minimum_days_required
    )
  
  # Stop if no valid observations remain after filtering
  if (nrow(signature_plot_df) == 0) {
    stop("No valid rows remain after filtering signature_by_stock.",
         call. = FALSE)
  }
  
  # Compute the stock-specific plateau RV
  plateau_df <- signature_plot_df %>%
    dplyr::filter(interval_minutes %in% plateau_intervals) %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      plateau_rv = stats::median(mean_rv, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Normalize each stock's RV by its own plateau
  signature_plot_df <- signature_plot_df %>%
    dplyr::left_join(plateau_df, by = "ticker") %>%
    dplyr::mutate(
      rv_ratio = mean_rv / plateau_rv
    )
  
  # Remove rows with invalid plateau or ratio values, if requested
  if (drop_invalid_plateau) {
    signature_plot_df <- signature_plot_df %>%
      dplyr::filter(
        is.finite(plateau_rv),
        plateau_rv > 0,
        is.finite(rv_ratio),
        rv_ratio > 0
      )
  }
  
  # Add sector information if available
  if (!is.null(ticker_sector_table)) {
    
    # Check that the sector table can be joined by ticker
    if (!"ticker" %in% names(ticker_sector_table)) {
      stop("ticker_sector_table must contain a ticker column.",
           call. = FALSE)
    }
    
    # Keep one sector-table row per ticker
    ticker_sector_table <- ticker_sector_table %>%
      dplyr::mutate(ticker = as.character(ticker)) %>%
      dplyr::distinct(ticker, .keep_all = TRUE)
    
    # Merge sector information into the signature dataframe
    signature_plot_df <- signature_plot_df %>%
      dplyr::left_join(ticker_sector_table, by = "ticker")
  }
  
  # Order the final output by stock and sampling interval
  signature_plot_df <- signature_plot_df %>%
    dplyr::arrange(ticker, interval_minutes)
  
  # Return the plot-ready signature dataframe
  return(signature_plot_df)
}



#________________________BUILD_SIGNATURE_INTERVAL_SUMMARY_______________________

# This function builds a cross-sectional summary of the normalized volatility
# signature across stocks for each sampling interval.
#
# It is mainly used to plot:
# - the median normalized RV signature
# - cross-sectional bands such as p10-p90 and p25-p75
# - the number of stocks available at each interval

BuildSignatureIntervalSummary <- function(signature_plot_df,
                                          ratio_col = "rv_ratio") {
  
  required_cols <- c("ticker", "interval_minutes", ratio_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop(
      paste0(
        "signature_plot_df must contain ticker, interval_minutes ",
        "and the selected ratio column."
      ),
      call. = FALSE
    )
  }
  
  signature_interval_summary <- signature_plot_df %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      ratio_value = as.numeric(.data[[ratio_col]])
    ) %>%
    dplyr::filter(
      is.finite(ratio_value),
      ratio_value > 0
    ) %>%
    dplyr::group_by(interval_minutes) %>%
    dplyr::summarise(
      median_ratio = stats::median(ratio_value, na.rm = TRUE),
      mean_ratio = mean(ratio_value, na.rm = TRUE),
      p10_ratio = stats::quantile(ratio_value, probs = 0.10, na.rm = TRUE),
      p25_ratio = stats::quantile(ratio_value, probs = 0.25, na.rm = TRUE),
      p75_ratio = stats::quantile(ratio_value, probs = 0.75, na.rm = TRUE),
      p90_ratio = stats::quantile(ratio_value, probs = 0.90, na.rm = TRUE),
      sd_ratio = stats::sd(ratio_value, na.rm = TRUE),
      n_stocks = dplyr::n_distinct(ticker),
      .groups = "drop"
    ) %>%
    dplyr::arrange(interval_minutes)
  
  return(signature_interval_summary)
}



#_________________________BUILD_SIGNATURE_SECTOR_SUMMARY________________________

# This function builds a sector-level summary of the normalized volatility
# signature across stocks for each sampling interval.
#
# It is mainly used to plot:
# - the median normalized RV signature by sector
# - sector-level cross-sectional bands such as p25-p75
# - the number of stocks available in each sector and interval

BuildSignatureSectorSummary <- function(signature_plot_df,
                                        sector_col = "sector",
                                        ratio_col = "rv_ratio",
                                        minimum_stocks_per_sector = 1,
                                        drop_missing_sector = TRUE) {
  
  required_cols <- c("ticker", "interval_minutes", sector_col, ratio_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop(
      paste0(
        "signature_plot_df must contain ticker, interval_minutes, ",
        "the selected sector column and the selected ratio column."
      ),
      call. = FALSE
    )
  }
  
  signature_sector_df <- signature_plot_df %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      sector = as.character(.data[[sector_col]]),
      ratio_value = as.numeric(.data[[ratio_col]])
    ) %>%
    dplyr::filter(
      is.finite(ratio_value),
      ratio_value > 0
    )
  
  if (drop_missing_sector) {
    signature_sector_df <- signature_sector_df %>%
      dplyr::filter(
        !is.na(sector),
        nzchar(sector)
      )
  }
  
  sector_size_df <- signature_sector_df %>%
    dplyr::group_by(sector) %>%
    dplyr::summarise(
      total_stocks_sector = dplyr::n_distinct(ticker),
      .groups = "drop"
    )
  
  signature_sector_summary <- signature_sector_df %>%
    dplyr::left_join(sector_size_df, by = "sector") %>%
    dplyr::filter(total_stocks_sector >= minimum_stocks_per_sector) %>%
    dplyr::group_by(sector, interval_minutes) %>%
    dplyr::summarise(
      median_ratio = stats::median(ratio_value, na.rm = TRUE),
      mean_ratio = mean(ratio_value, na.rm = TRUE),
      p10_ratio = stats::quantile(ratio_value, probs = 0.10, na.rm = TRUE),
      p25_ratio = stats::quantile(ratio_value, probs = 0.25, na.rm = TRUE),
      p75_ratio = stats::quantile(ratio_value, probs = 0.75, na.rm = TRUE),
      p90_ratio = stats::quantile(ratio_value, probs = 0.90, na.rm = TRUE),
      sd_ratio = stats::sd(ratio_value, na.rm = TRUE),
      n_stocks = dplyr::n_distinct(ticker),
      total_stocks_sector = dplyr::first(total_stocks_sector),
      .groups = "drop"
    ) %>%
    dplyr::arrange(sector, interval_minutes)
  
  return(signature_sector_summary)
}



#___________________________PLOT_SIGNATURE_SPAGHETTI____________________________

# This function plots the normalized volatility signature across all stocks.
#
# The plot includes:
# - thin transparent lines for individual stocks
# - a cross-sectional median line
# - a cross-sectional ribbon, for example p10-p90 or p25-p75
# - a horizontal reference line at 1
# - a vertical reference line at the selected interval, usually 5 minutes

PlotSignatureSpaghetti <- function(signature_plot_df,
                                   signature_interval_summary = NULL,
                                   ratio_col = "rv_ratio",
                                   lower_band_col = "p10_ratio",
                                   upper_band_col = "p90_ratio",
                                   median_col = "median_ratio",
                                   reference_interval = 5,
                                   x_breaks = c(1, 2, 5, 10, 15, 20, 25, 30),
                                   y_limits = NULL,
                                   title,
                                   subtitle,
                                   output_path = NULL,
                                   width = 10,
                                   height = 6) {
  
  # Check that the required columns are available
  required_cols <- c("ticker", "interval_minutes", ratio_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop(
      paste0(
        "signature_plot_df must contain ticker, interval_minutes ",
        "and the selected ratio column."
      ),
      call. = FALSE
    )
  }
  
  # Prepare stock-level data for the individual spaghetti lines
  plot_df <- signature_plot_df %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      ratio_value = as.numeric(.data[[ratio_col]])
    ) %>%
    dplyr::filter(
      is.finite(ratio_value),
      ratio_value > 0
    ) %>%
    dplyr::arrange(ticker, interval_minutes)
  
  # Stop if no valid observations remain for plotting
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the spaghetti plot.",
         call. = FALSE)
  }
  
  # Build the interval summary internally if it is not provided
  if (is.null(signature_interval_summary)) {
    signature_interval_summary <- BuildSignatureIntervalSummary(
      signature_plot_df = signature_plot_df,
      ratio_col = ratio_col
    )
  }
  
  # Check that the summary contains the required band and median columns
  summary_required_cols <- c(
    "interval_minutes",
    lower_band_col,
    upper_band_col,
    median_col
  )
  
  if (!all(summary_required_cols %in% names(signature_interval_summary))) {
    stop(
      paste0(
        "signature_interval_summary does not contain ",
        "the required summary columns."
      ),
      call. = FALSE
    )
  }
  
  # Prepare interval-level data for the ribbon and median line
  summary_df <- signature_interval_summary %>%
    dplyr::mutate(
      interval_minutes = as.integer(interval_minutes),
      lower_band = as.numeric(.data[[lower_band_col]]),
      upper_band = as.numeric(.data[[upper_band_col]]),
      median_value = as.numeric(.data[[median_col]])
    ) %>%
    dplyr::filter(
      is.finite(lower_band),
      is.finite(upper_band),
      is.finite(median_value)
    ) %>%
    dplyr::arrange(interval_minutes)
  
  # Build the spaghetti plot layer by layer
  p <- ggplot2::ggplot() +
    
    # Plot one thin line for each stock
    ggplot2::geom_line(
      data = plot_df,
      ggplot2::aes(
        x = interval_minutes,
        y = ratio_value,
        group = ticker
      ),
      alpha = 0.12,
      linewidth = 0.25,
      color = "grey45"
    ) +
    
    # Add the cross-sectional uncertainty band
    ggplot2::geom_ribbon(
      data = summary_df,
      ggplot2::aes(
        x = interval_minutes,
        ymin = lower_band,
        ymax = upper_band
      ),
      alpha = 0.25,
      fill = "grey70"
    ) +
    
    # Add the cross-sectional median signature
    ggplot2::geom_line(
      data = summary_df,
      ggplot2::aes(
        x = interval_minutes,
        y = median_value
      ),
      linewidth = 1.1,
      color = "black"
    ) +
    
    # Add the plateau reference line
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    
    # Set x-axis breaks
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    # Add plot labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sampling interval (minutes)",
      y = "Mean RV / stock-specific plateau RV"
    ) +
    
    # Use a clean minimal theme
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  # Add the selected reference interval, if requested
  if (!is.null(reference_interval)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = reference_interval,
        linetype = "dotted",
        linewidth = 0.5
      )
  }
  
  # Apply optional y-axis limits without removing data
  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::coord_cartesian(ylim = y_limits)
  }
  
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
  
  # Return the ggplot object
  return(p)
}



#___________________________PLOT_SIGNATURE_BY_SECTOR____________________________

# This function plots the normalized volatility signature by sector.
#
# The plot includes:
# - one facet for each sector
# - a sector-level median normalized RV line
# - a sector-level ribbon, for example p25-p75
# - a horizontal reference line at 1
# - a vertical reference line at the selected interval, usually 5 minutes

PlotSignatureBySector <- function(signature_sector_summary,
                                  sector_col = "sector",
                                  lower_band_col = "p25_ratio",
                                  upper_band_col = "p75_ratio",
                                  median_col = "median_ratio",
                                  reference_interval = 5,
                                  x_breaks = c(1, 2, 5, 10, 15, 20, 25, 30),
                                  y_limits = NULL,
                                  facet_ncol = 3,
                                  title,
                                  subtitle,
                                  output_path = NULL,
                                  width = 12,
                                  height = 8) {
  
  # Check that the required columns are available
  required_cols <- c(
    sector_col,
    "interval_minutes",
    lower_band_col,
    upper_band_col,
    median_col
  )
  
  if (!all(required_cols %in% names(signature_sector_summary))) {
    stop("signature_sector_summary does not contain the required columns.",
         call. = FALSE)
  }
  
  # Prepare sector-level data for the ribbon and median line
  plot_df <- signature_sector_summary %>%
    dplyr::mutate(
      sector = as.character(.data[[sector_col]]),
      interval_minutes = as.integer(interval_minutes),
      lower_band = as.numeric(.data[[lower_band_col]]),
      upper_band = as.numeric(.data[[upper_band_col]]),
      median_value = as.numeric(.data[[median_col]])
    ) %>%
    dplyr::filter(
      !is.na(sector),
      nzchar(sector),
      is.finite(lower_band),
      is.finite(upper_band),
      is.finite(median_value)
    ) %>%
    dplyr::arrange(sector, interval_minutes)
  
  # Stop if no valid observations remain for plotting
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the sector signature plot.",
         call. = FALSE)
  }
  
  # Add sector labels with the number of stocks, if available
  if ("total_stocks_sector" %in% names(plot_df)) {
    
    sector_label_df <- plot_df %>%
      dplyr::group_by(sector) %>%
      dplyr::summarise(
        total_stocks_sector = dplyr::first(total_stocks_sector),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        sector_label = paste0(sector, " (n = ", total_stocks_sector, ")")
      )
    
    plot_df <- plot_df %>%
      dplyr::left_join(sector_label_df, by = "sector")
    
  } else {
    
    # Use sector names only if stock counts are not available
    plot_df <- plot_df %>%
      dplyr::mutate(sector_label = sector)
  }
  
  # Build the sector-level volatility signature plot
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = interval_minutes)
  ) +
    
    # Add the sector-level cross-sectional band
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = lower_band,
        ymax = upper_band,
        group = sector_label
      ),
      alpha = 0.25,
      fill = "grey70"
    ) +
    
    # Add the sector-level median signature
    ggplot2::geom_line(
      ggplot2::aes(
        y = median_value,
        group = sector_label
      ),
      linewidth = 0.9,
      color = "black"
    ) +
    
    # Add the plateau reference line
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.35
    ) +
    
    # Set x-axis breaks
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    # Create one panel for each sector
    ggplot2::facet_wrap(
      ~ sector_label,
      ncol = facet_ncol
    ) +
    
    # Add plot labels
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sampling interval (minutes)",
      y = "Mean RV / stock-specific plateau RV"
    ) +
    
    # Use a clean minimal theme
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  # Add the selected reference interval, if requested
  if (!is.null(reference_interval)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = reference_interval,
        linetype = "dotted",
        linewidth = 0.4
      )
  }
  
  # Apply optional y-axis limits without removing data
  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::coord_cartesian(ylim = y_limits)
  }
  
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
  
  # Return the ggplot object
  return(p)
}



#_____________________________END_OF_THE_SCRIPT_________________________________