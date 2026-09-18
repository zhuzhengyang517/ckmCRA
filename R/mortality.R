#' Predict Mortality Risk (CRA-based)
#'
#' Uses pre-trained Cox and Fine-Gray models to estimate all-cause
#' and cause-specific mortality risk based on CRA status and covariates.
#'
#' @param data Data frame with columns: CRA_high, age, sex_f, race_f,
#'   educ_f, pir_f.
#'
#' @return Data frame with new columns:
#'   \itemize{
#'     \item HR_all_cause - hazard ratio vs reference patient
#'     \item sHR_cvd - subdistribution HR for CVD death
#'     \item sHR_non_cvd - subdistribution HR for non-CVD death
#'     \item risk_5y - 5-year absolute mortality risk
#'     \item risk_10y - 10-year absolute mortality risk
#'     \item risk_category - Low / Moderate / High / Very High
#'   }
#'
#' @export
predict_mortality <- function(data) {
  # --- 1. Load models ---
  model_path <- system.file("extdata", "models", "mortality_models.rds",
                            package = "ckmCRA")
  if (!file.exists(model_path)) stop("Mortality models not found")
  m <- readRDS(model_path)

  # --- 2. Validate ---
  required <- c("CRA_high", "age", "sex_f", "race_f", "educ_f", "pir_f")
  missing_vars <- setdiff(required, colnames(data))
  if (length(missing_vars) > 0) {
    stop("Missing columns: ", paste(missing_vars, collapse = ", "))
  }

  # --- 3. HR relative to reference ---
  formula_mort <- ~ CRA_high + age + sex_f + race_f + educ_f + pir_f
  X <- model.matrix(formula_mort, data = data)[, -1, drop = FALSE]

  lp_main  <- as.numeric(X %*% coef(m$fit_main))
  lp_cvd   <- as.numeric(X %*% m$fit_fg_cvd$coef)
  lp_other <- as.numeric(X %*% m$fit_fg_other$coef)

  ref <- data.frame(
    CRA_high = 0,
    age      = 60,
    sex_f    = factor("Male", levels = levels(data$sex_f)),
    race_f   = factor("NH White", levels = levels(data$race_f)),
    educ_f   = factor("HS", levels = levels(data$educ_f)),
    pir_f    = factor("PIR 1.3-3.5", levels = levels(data$pir_f))
  )
  X_ref <- model.matrix(formula_mort, data = ref)[, -1, drop = FALSE]
  lp_ref_main  <- as.numeric(X_ref %*% coef(m$fit_main))
  lp_ref_cvd   <- as.numeric(X_ref %*% m$fit_fg_cvd$coef)
  lp_ref_other <- as.numeric(X_ref %*% m$fit_fg_other$coef)

  data$HR_all_cause <- exp(lp_main  - lp_ref_main)
  data$sHR_cvd      <- exp(lp_cvd   - lp_ref_cvd)
  data$sHR_non_cvd  <- exp(lp_other - lp_ref_other)

  # --- 4. Absolute risk via survfit ---
  sf <- tryCatch(
    survival::survfit(m$fit_main, newdata = data),
    error = function(e) NULL
  )

  if (!is.null(sf)) {
    # 5 年 (60 个月) 和 10 年 (120 个月)
    get_risk_at <- function(sf, t_target) {
      idx <- which.min(abs(sf$time - t_target))
      if (sf$time[idx] > t_target && idx > 1) idx <- idx - 1
      surv_mat <- sf$surv
      if (is.matrix(surv_mat)) {
        1 - surv_mat[idx, ]
      } else {
        1 - surv_mat[idx]
      }
    }
    data$risk_5y  <- get_risk_at(sf, 60)
    data$risk_10y <- get_risk_at(sf, 120)
  } else {
    data$risk_5y  <- NA_real_
    data$risk_10y <- NA_real_
  }

  # --- 5. Risk stratification based on 5-year absolute risk ---
  if (all(is.na(data$risk_5y))) {
    data$risk_category <- NA_character_
  } else {
    data$risk_category <- cut(
      data$risk_5y,
      breaks = c(-Inf, 0.05, 0.10, 0.20, Inf),
      labels = c("Low", "Moderate", "High", "Very High"),
      right  = TRUE
    )
  }

  data
}
