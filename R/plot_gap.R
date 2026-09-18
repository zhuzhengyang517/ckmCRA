#' Plot Organ Age Gap Distribution
#'
#' Density plot of organ age gaps for Heart, Kidney, and Metab.
#'
#' @param data Data frame with HeartAgeGap_ML, KidneyAgeGap_ML,
#'   MetabAgeGap_ML.
#'
#' @return A ggplot object.
#'
#' @export
plot_organ_gap <- function(data) {
  gap_cols <- c("HeartAgeGap_ML", "KidneyAgeGap_ML", "MetabAgeGap_ML")
  if (!all(gap_cols %in% colnames(data))) {
    stop("Missing organ age gap columns. Run run_ckm_cra() first.")
  }

  df <- data.frame(
    Gap   = c(data$HeartAgeGap_ML, data$KidneyAgeGap_ML, data$MetabAgeGap_ML),
    Organ = rep(c("Heart", "Kidney", "Metab"),
                each = nrow(data))
  )
  df <- df[!is.na(df$Gap), ]

  ggplot2::ggplot(df, ggplot2::aes(x = Gap, fill = Organ, color = Organ)) +
    ggplot2::geom_density(alpha = 0.4, linewidth = 0.9) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed",
                        color = "grey30") +
    ggplot2::scale_fill_manual(
      values = c("Heart" = "#4A90A4",
                 "Kidney" = "#D4A843",
                 "Metab"  = "#8B3A3A")
    ) +
    ggplot2::scale_color_manual(
      values = c("Heart" = "#4A90A4",
                 "Kidney" = "#D4A843",
                 "Metab"  = "#8B3A3A")
    ) +
    ggplot2::labs(
      x = "Organ age gap (years)",
      y = "Density",
      fill = "",
      title = "Organ age gap distribution"
    ) +
    ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    )
}
