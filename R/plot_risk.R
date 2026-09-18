#' Plot Mortality Risk Stratification
#'
#' Bar chart of risk categories (Low / Moderate / High / Very High).
#'
#' @param data Data frame with risk_category.
#'
#' @return A ggplot object.
#'
#' @export
plot_risk_stratification <- function(data) {
  if (!"risk_category" %in% colnames(data)) {
    stop("Column 'risk_category' not found. Run run_ckm_cra() first.")
  }

  df <- as.data.frame(table(data$risk_category))
  colnames(df) <- c("Category", "N")
  df$pct <- df$N / sum(df$N) * 100
  df$Category <- factor(df$Category,
                        levels = c("Low", "Moderate",
                                   "High", "Very High"))

  ggplot2::ggplot(df, ggplot2::aes(x = Category, y = pct, fill = Category)) +
    ggplot2::geom_col(width = 0.7, color = "grey30") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%\n(n=%d)",
                                    pct, N)),
                       vjust = -0.3, size = 3.5) +
    ggplot2::scale_fill_manual(
      values = c("Low" = "#4A90A4",
                 "Moderate" = "#7BA7B8",
                 "High" = "#D4A843",
                 "Very High" = "#8B3A3A")
    ) +
    ggplot2::labs(
      x = "",
      y = "Proportion (%)",
      fill = "",
      title = "5-year mortality risk stratification"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(face = "bold")
    )
}
