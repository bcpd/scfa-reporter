#' Build ggplot2 boxplots for each SCFA analyte
#'
#' @param scfa_long Long-format tibble produced by [scfa_long_format()].
#' @param analyte_col Column name that stores the analyte identifier.
#' @param value_col Column name that stores the numeric measurements.
#' @param x_col Column mapped to the x-axis (defaults to `"Group"`).
#' @param facet_col Optional column used for faceting.
#' @param colour_col Column used for the colour aesthetic.
#' @param y_label Axis label for the numeric axis.
#' @param palette Character vector with manual colour values.
#'
#' @return A named list of ggplot objects, one per analyte.
#' @importFrom rlang .data
#' @export
plot_scfa_panels <- function(scfa_long,
                             analyte_col = "analyte",
                             value_col = "value",
                             x_col = "group",
                             facet_col = NULL,
                             colour_col = x_col,
                             y_label = "SCFA concentration",
                             palette = c("#009E73", "#E79F00", "#9AD0F3", "#0072B2", "#D55E00", "#CC79A7")) {
  scfa_long <- janitor::clean_names(scfa_long)
  analyte_col <- janitor::make_clean_names(analyte_col)
  value_col <- janitor::make_clean_names(value_col)
  x_col <- janitor::make_clean_names(x_col)
  colour_col <- janitor::make_clean_names(colour_col)
  if (!is.null(facet_col)) {
    facet_col <- janitor::make_clean_names(facet_col)
  }

  analytes <- unique(scfa_long[[analyte_col]])
  plots <- vector("list", length = length(analytes))
  names(plots) <- analytes

  for (analyte in analytes) {
    subset_df <- scfa_long[scfa_long[[analyte_col]] == analyte, ]
    p <- ggplot2::ggplot(
      subset_df,
      ggplot2::aes(x = .data[[x_col]], y = .data[[value_col]])
    ) +
      ggplot2::geom_boxplot(outlier.alpha = 0) +
      ggplot2::geom_jitter(
        ggplot2::aes(colour = .data[[colour_col]]),
        width = 0.2,
        height = 0,
        alpha = 0.7,
        size = 2
      ) +
      ggplot2::labs(title = analyte, y = y_label, colour = colour_col) +
      ggplot2::theme_bw()

    if (!is.null(facet_col)) {
      formula <- stats::as.formula(paste("~", facet_col))
      p <- p + ggplot2::facet_grid(formula, scales = "free_x", space = "free_x")
    }

    if (!is.null(palette)) {
      p <- p + ggplot2::scale_colour_manual(values = palette)
    }

    plots[[analyte]] <- p
  }

  plots
}
