#' Plot CRA Distribution
#'
#' Bar chart of CRA score distribution (0-5), with CRA_high highlighted.
#'
#' @param data Data frame with CRA column (from calc_cra or run_ckm_cra).
#'
#' @return A ggplot object.
#'
#' @export
plot_cra_dist <- function(data) {
  if (!"CRA" %in% colnames(data)) {
    stop("Column 'CRA' not found. Run calc_cra() or run_ckm_cra() first.")
  }

  df <- as.data.frame(table(CRA = factor(data$CRA, levels = 0:5)))
  df$pct <- df$Freq / sum(df$Freq) * 100
  df$high <- ifelse(as.numeric(as.character(df$CRA)) >= 4,
                    "CRA high (≥4)", "CRA low (<4)")

  ggplot2::ggplot(df, ggplot2::aes(x = CRA, y = pct, fill = high)) +
    ggplot2::geom_col(width = 0.7, color = "grey30") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct)),
                       vjust = -0.5, size = 4) +
    ggplot2::scale_fill_manual(
      values = c("CRA high (≥4)" = "#4A90A4",
                 "CRA low (<4)"  = "#C77D4A")
    ) +
    ggplot2::labs(
      x = "CRA score",
      y = "Proportion (%)",
      fill = "",
      title = "CRA score distribution"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )
}
