#' Calculate Organ Age Gaps
#'
#' Uses pre-trained linear regression coefficients to compute organ age
#' gaps (residuals) for new patients. This allows single-patient gap
#' calculation without needing a reference population.
#'
#' @param data Data frame with organ age predictions and 'age'.
#' @param organ One of 'Heart', 'Kidney', 'MetabInf'.
#'
#' @return Data frame with new column '<organ>AgeGap_ML'.
#'   For MetabInf, the column is named 'MetabAgeGap_ML'.
#'
#' @export
calc_organ_gap <- function(data, organ = c("Heart", "Kidney", "MetabInf")) {
  organ <- match.arg(organ)

  # Load pre-trained gap regression coefficients
  coef_path <- system.file("extdata", "models", "gap_coefficients.rds",
                           package = "ckmCRA")
  if (!file.exists(coef_path)) stop("Gap coefficients not found")
  coefs <- readRDS(coef_path)

  age_col <- if (organ == "MetabInf") "MetabAge_ML" else paste0(organ, "Age_ML")
  gap_col <- if (organ == "MetabInf") "MetabAgeGap_ML" else paste0(organ, "AgeGap_ML")
  coef_key <- if (organ == "MetabInf") "Metab" else organ

  if (!age_col %in% colnames(data)) {
    stop("Column ", age_col, " not found. Run predict_organ_age() first.")
  }
  if (!"age" %in% colnames(data)) stop("Column 'age' not found.")

  # Compute gap using pre-trained coefficients
  b0 <- coefs[[coef_key]][1]
  b1 <- coefs[[coef_key]][2]
  data[[gap_col]] <- data[[age_col]] - (b0 + b1 * data$age)

  data
}
