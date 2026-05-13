#_________________________PREPARE_SIGNATURE_PLOT_DATA___________________________

# This function prepares the volatility signature data for plotting.
# It normalizes each stock's mean RV by its own stable plateau level.
# The plateau is defined as the median mean RV over selected intervals,
# for example 10:30 minutes.
#
# Output columns include:
# - plateau_rv: stock-specific stable RV benchmark
# - rv_ratio: mean_rv / plateau_rv
# - log_rv_ratio: log(mean_rv / plateau_rv)
# - noise_ratio: average short-interval RV / plateau_rv
# - ratio_5min: 5-minute RV / plateau_rv

PrepareSignaturePlotData <- function(signature_by_stock,
                                     ticker_sector_table = NULL,
                                     plateau_intervals = 10:30,
                                     noise_intervals = 1:2,
                                     reference_interval = 5,
                                     minimum_days_required = 1,
                                     drop_invalid_plateau = TRUE) {
  
  required_cols <- c("ticker", "interval_minutes", "mean_rv", "n_days")
  
  if (!all(required_cols %in% names(signature_by_stock))) {
    stop("signature_by_stock must contain ticker, interval_minutes, mean_rv and n_days.",
         call. = FALSE)
  }
  
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
  
  if (nrow(signature_plot_df) == 0) {
    stop("No valid rows remain after filtering signature_by_stock.",
         call. = FALSE)
  }
  
  # Compute stock-specific plateau RV
  plateau_df <- signature_plot_df %>%
    dplyr::filter(interval_minutes %in% plateau_intervals) %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      plateau_rv = stats::median(mean_rv, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compute stock-specific short-interval noise ratio
  noise_df <- signature_plot_df %>%
    dplyr::filter(interval_minutes %in% noise_intervals) %>%
    dplyr::group_by(ticker) %>%
    dplyr::summarise(
      short_interval_rv = mean(mean_rv, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Compute reference interval ratio, usually the 5-minute ratio
  reference_df <- signature_plot_df %>%
    dplyr::filter(interval_minutes == reference_interval) %>%
    dplyr::select(
      ticker,
      reference_rv = mean_rv
    )
  
  signature_plot_df <- signature_plot_df %>%
    dplyr::left_join(plateau_df, by = "ticker") %>%
    dplyr::left_join(noise_df, by = "ticker") %>%
    dplyr::left_join(reference_df, by = "ticker") %>%
    dplyr::mutate(
      rv_ratio = mean_rv / plateau_rv,
      log_rv_ratio = log(rv_ratio),
      noise_ratio = short_interval_rv / plateau_rv,
      ratio_5min = reference_rv / plateau_rv
    )
  
  if (drop_invalid_plateau) {
    signature_plot_df <- signature_plot_df %>%
      dplyr::filter(
        is.finite(plateau_rv),
        plateau_rv > 0,
        is.finite(rv_ratio),
        rv_ratio > 0
      )
  }
  
  # Add sector and sector ETF information if available
  if (!is.null(ticker_sector_table)) {
    
    if (!"ticker" %in% names(ticker_sector_table)) {
      stop("ticker_sector_table must contain a ticker column.",
           call. = FALSE)
    }
    
    ticker_sector_table <- ticker_sector_table %>%
      dplyr::mutate(ticker = as.character(ticker)) %>%
      dplyr::distinct(ticker, .keep_all = TRUE)
    
    signature_plot_df <- signature_plot_df %>%
      dplyr::left_join(ticker_sector_table, by = "ticker")
  }
  
  signature_plot_df <- signature_plot_df %>%
    dplyr::arrange(ticker, interval_minutes)
  
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
    stop("signature_plot_df must contain ticker, interval_minutes and the selected ratio column.",
         call. = FALSE)
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
    stop("signature_plot_df must contain ticker, interval_minutes, the selected sector column and the selected ratio column.",
         call. = FALSE)
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



#________________________BUILD_SIGNATURE_DECISION_TABLE_________________________

# This function builds a decision table for choosing the sampling interval.
# For each interval, it measures how far the normalized RV is from the plateau.
#
# The key statistic is:
# abs_deviation_from_plateau = |rv_ratio - 1|
#
# If the median deviation is small at a given interval, that interval is close
# to the stable region of the volatility signature.

BuildSignatureDecisionTable <- function(signature_plot_df,
                                        ratio_col = "rv_ratio") {
  
  required_cols <- c("ticker", "interval_minutes", ratio_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop("signature_plot_df must contain ticker, interval_minutes and the selected ratio column.",
         call. = FALSE)
  }
  
  signature_decision_table <- signature_plot_df %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      ratio_value = as.numeric(.data[[ratio_col]]),
      abs_deviation_from_plateau = abs(ratio_value - 1)
    ) %>%
    dplyr::filter(
      is.finite(ratio_value),
      ratio_value > 0,
      is.finite(abs_deviation_from_plateau)
    ) %>%
    dplyr::group_by(interval_minutes) %>%
    dplyr::summarise(
      median_ratio = stats::median(ratio_value, na.rm = TRUE),
      mean_ratio = mean(ratio_value, na.rm = TRUE),
      
      median_abs_deviation_from_plateau =
        stats::median(abs_deviation_from_plateau, na.rm = TRUE),
      
      mean_abs_deviation_from_plateau =
        mean(abs_deviation_from_plateau, na.rm = TRUE),
      
      p75_abs_deviation_from_plateau =
        as.numeric(stats::quantile(abs_deviation_from_plateau,
                                   probs = 0.75,
                                   na.rm = TRUE)),
      
      p90_abs_deviation_from_plateau =
        as.numeric(stats::quantile(abs_deviation_from_plateau,
                                   probs = 0.90,
                                   na.rm = TRUE)),
      
      share_within_2pct = mean(abs_deviation_from_plateau <= 0.02,
                               na.rm = TRUE),
      
      share_within_5pct = mean(abs_deviation_from_plateau <= 0.05,
                               na.rm = TRUE),
      
      share_within_10pct = mean(abs_deviation_from_plateau <= 0.10,
                                na.rm = TRUE),
      
      n_stocks = dplyr::n_distinct(ticker),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      median_abs_deviation_pct =
        100 * median_abs_deviation_from_plateau,
      
      mean_abs_deviation_pct =
        100 * mean_abs_deviation_from_plateau,
      
      p75_abs_deviation_pct =
        100 * p75_abs_deviation_from_plateau,
      
      p90_abs_deviation_pct =
        100 * p90_abs_deviation_from_plateau,
      
      share_within_2pct = 100 * share_within_2pct,
      share_within_5pct = 100 * share_within_5pct,
      share_within_10pct = 100 * share_within_10pct
    ) %>%
    dplyr::arrange(interval_minutes)
  
  return(signature_decision_table)
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
                                   title = "Volatility Signature Plot",
                                   subtitle = "Normalized realized volatility across stocks",
                                   output_path = NULL,
                                   width = 10,
                                   height = 6) {
  
  required_cols <- c("ticker", "interval_minutes", ratio_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop("signature_plot_df must contain ticker, interval_minutes and the selected ratio column.",
         call. = FALSE)
  }
  
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
  
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the spaghetti plot.",
         call. = FALSE)
  }
  
  if (is.null(signature_interval_summary)) {
    signature_interval_summary <- BuildSignatureIntervalSummary(
      signature_plot_df = signature_plot_df,
      ratio_col = ratio_col
    )
  }
  
  summary_required_cols <- c(
    "interval_minutes",
    lower_band_col,
    upper_band_col,
    median_col
  )
  
  if (!all(summary_required_cols %in% names(signature_interval_summary))) {
    stop("signature_interval_summary does not contain the required summary columns.",
         call. = FALSE)
  }
  
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
  
  p <- ggplot2::ggplot() +
    
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
    
    ggplot2::geom_line(
      data = summary_df,
      ggplot2::aes(
        x = interval_minutes,
        y = median_value
      ),
      linewidth = 1.1,
      color = "black"
    ) +
    
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.4
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sampling interval (minutes)",
      y = "Mean RV / stock-specific plateau RV"
    ) +
    
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
  
  if (!is.null(reference_interval)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = reference_interval,
        linetype = "dotted",
        linewidth = 0.5
      )
  }
  
  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::coord_cartesian(ylim = y_limits)
  }
  
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



#____________________________PLOT_SIGNATURE_HEATMAP_____________________________

# This function plots a heatmap of the normalized volatility signature.
#
# Rows are stocks, columns are sampling intervals, and the fill is usually
# log_rv_ratio = log(mean_rv / plateau_rv).
#
# Stocks are ordered by noise_ratio, so the stocks most affected by short-interval
# RV deviations appear at the top.

PlotSignatureHeatmap <- function(signature_plot_df,
                                 fill_col = "log_rv_ratio",
                                 order_col = "noise_ratio",
                                 reference_interval = 5,
                                 x_breaks = c(1, 2, 5, 10, 15, 20, 25, 30),
                                 cap_quantile = 0.98,
                                 show_ticker_labels = FALSE,
                                 title = "Volatility Signature Heatmap",
                                 subtitle = "Stocks ordered by short-interval RV deviation",
                                 output_path = NULL,
                                 width = 10,
                                 height = 8) {
  
  required_cols <- c("ticker", "interval_minutes", fill_col)
  
  if (!all(required_cols %in% names(signature_plot_df))) {
    stop("signature_plot_df must contain ticker, interval_minutes and the selected fill column.",
         call. = FALSE)
  }
  
  plot_df <- signature_plot_df %>%
    dplyr::mutate(
      ticker = as.character(ticker),
      interval_minutes = as.integer(interval_minutes),
      fill_value = as.numeric(.data[[fill_col]])
    ) %>%
    dplyr::filter(
      is.finite(fill_value)
    )
  
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the heatmap.",
         call. = FALSE)
  }
  
  if (order_col %in% names(plot_df)) {
    
    ticker_order_df <- plot_df %>%
      dplyr::group_by(ticker) %>%
      dplyr::summarise(
        order_value = dplyr::first(as.numeric(.data[[order_col]])),
        .groups = "drop"
      )
    
  } else {
    
    ticker_order_df <- plot_df %>%
      dplyr::group_by(ticker) %>%
      dplyr::summarise(
        order_value = stats::median(fill_value, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  ticker_levels <- ticker_order_df %>%
    dplyr::arrange(dplyr::desc(order_value), ticker) %>%
    dplyr::pull(ticker)
  
  plot_df <- plot_df %>%
    dplyr::mutate(
      ticker_ordered = factor(ticker, levels = rev(ticker_levels))
    )
  
  if (!is.null(cap_quantile)) {
    
    cap_value <- stats::quantile(
      abs(plot_df$fill_value),
      probs = cap_quantile,
      na.rm = TRUE
    )
    
    cap_value <- as.numeric(cap_value)
    
    if (is.finite(cap_value) && cap_value > 0) {
      plot_df <- plot_df %>%
        dplyr::mutate(
          fill_value_plot = pmax(pmin(fill_value, cap_value), -cap_value)
        )
    } else {
      plot_df <- plot_df %>%
        dplyr::mutate(fill_value_plot = fill_value)
    }
    
  } else {
    
    plot_df <- plot_df %>%
      dplyr::mutate(fill_value_plot = fill_value)
  }
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = interval_minutes,
      y = ticker_ordered,
      fill = fill_value_plot
    )
  ) +
    
    ggplot2::geom_tile() +
    
    ggplot2::geom_vline(
      xintercept = reference_interval,
      linetype = "dotted",
      linewidth = 0.4
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    ggplot2::scale_fill_gradient2(
      low = "steelblue",
      mid = "white",
      high = "firebrick",
      midpoint = 0,
      name = "Log ratio"
    ) +
    
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sampling interval (minutes)",
      y = "Stocks ordered by noise ratio"
    ) +
    
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  
  if (show_ticker_labels) {
    p <- p +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(size = 5),
        axis.ticks.y = ggplot2::element_line()
      )
  }
  
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
                                  title = "Sector-Level Volatility Signature Plot",
                                  subtitle = "Normalized realized volatility by sector",
                                  output_path = NULL,
                                  width = 12,
                                  height = 8) {
  
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
  
  if (nrow(plot_df) == 0) {
    stop("No valid rows available for the sector signature plot.",
         call. = FALSE)
  }
  
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
    
    plot_df <- plot_df %>%
      dplyr::mutate(sector_label = sector)
  }
  
  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = interval_minutes)
  ) +
    
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = lower_band,
        ymax = upper_band,
        group = sector_label
      ),
      alpha = 0.25,
      fill = "grey70"
    ) +
    
    ggplot2::geom_line(
      ggplot2::aes(
        y = median_value,
        group = sector_label
      ),
      linewidth = 0.9,
      color = "black"
    ) +
    
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "dashed",
      linewidth = 0.35
    ) +
    
    ggplot2::scale_x_continuous(
      breaks = x_breaks
    ) +
    
    ggplot2::facet_wrap(
      ~ sector_label,
      ncol = facet_ncol
    ) +
    
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Sampling interval (minutes)",
      y = "Mean RV / stock-specific plateau RV"
    ) +
    
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
  
  if (!is.null(reference_interval)) {
    p <- p +
      ggplot2::geom_vline(
        xintercept = reference_interval,
        linetype = "dotted",
        linewidth = 0.4
      )
  }
  
  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::coord_cartesian(ylim = y_limits)
  }
  
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