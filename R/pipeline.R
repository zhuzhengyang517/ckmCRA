#' Run Complete CKM-CRA Pipeline
#'
#' Runs the full pipeline: data preparation, CRA scoring, organ age
#' prediction, organ age gap calculation, and mortality risk prediction.
#'
#' @param data Data frame with clinical biomarkers and covariates.
#' @param verbose Print progress messages (default TRUE).
#'
#' @return Data frame with all computed columns:
#'   \itemize{
#'     \item cra_gly, cra_bp, cra_kid, cra_alb, cra_ob - 5 CRA dimensions
#'     \item CRA, CRA_high - CRA score and high-accessibility flag
#'     \item HeartAge_ML, KidneyAge_ML, MetabAge_ML - organ ages (Path A)
#'     \item HeartAgeGap_ML, KidneyAgeGap_ML, MetabAgeGap_ML - organ gaps
#'     \item HR_all_cause, sHR_cvd, sHR_non_cvd - mortality hazard ratios
#'     \item risk_5y, risk_10y - absolute 5-year and 10-year mortality risk
#'     \item risk_category - Low / Moderate / High / Very High
#'   }
#'
#' @export
run_ckm_cra <- function(data, verbose = TRUE) {
  if (verbose) cat("Step 1/5: Preparing data...\n")
  data <- prepare_data(data)

  # --- CRA ---
  cra_vars <- c("dm", "LBXGH", "BPXSY1", "BPXDI1",
                "EGFR", "UACR", "bmi")
  if (all(cra_vars %in% colnames(data))) {
    if (verbose) cat("Step 2/5: Calculating CRA...\n")
    data <- calc_cra(data)
  } else {
    if (verbose) cat("Step 2/5: Skipped CRA (missing variables)\n")
  }

  # --- Organ ages ---
  if (verbose) cat("Step 3/5: Predicting organ ages...\n")
  for (organ in c("Heart", "Kidney", "MetabInf")) {
    model_file <- switch(organ,
      Heart    = "pathA_heart.rds",
      Kidney   = "pathA_kidney.rds",
      MetabInf = "pathA_metab.rds"
    )
    model_path <- system.file("extdata", "models", model_file,
                              package = "ckmCRA")
    if (file.exists(model_path)) {
      model <- readRDS(model_path)
      if (all(model$final_vars %in% colnames(data))) {
        data <- predict_organ_age(data, organ = organ)
      } else {
        if (verbose) cat("  Skipped ", organ, " (missing variables)\n", sep = "")
      }
    }
  }

  # --- Organ gaps ---
  if (verbose) cat("Step 4/5: Calculating organ age gaps...\n")
  for (organ in c("Heart", "Kidney", "MetabInf")) {
    age_col <- if (organ == "MetabInf") "MetabAge_ML" else paste0(organ, "Age_ML")
    if (age_col %in% colnames(data) && "age" %in% colnames(data)) {
      data <- calc_organ_gap(data, organ = organ)
    }
  }

  # --- Mortality ---
  mort_vars <- c("CRA_high", "age", "sex_f", "race_f", "educ_f", "pir_f")
  if (all(mort_vars %in% colnames(data))) {
    if (verbose) cat("Step 5/5: Predicting mortality risk...\n")
    data <- predict_mortality(data)
  } else {
    if (verbose) cat("Step 5/5: Skipped mortality (missing variables)\n")
  }

  if (verbose) cat("\n✅ Pipeline complete\n")
  data
}
