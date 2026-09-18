#' Explain Organ Age Prediction
#'
#' Computes each biomarker's contribution to the predicted organ age
#' using leave-one-out perturbation, with sign-consistency filtering
#' to handle GBM collinearity.
#'
#' @param data Data frame with clinical biomarkers.
#' @param organ One of 'Heart', 'Kidney', 'MetabInf'.
#'
#' @return Data frame with variable, z-score, contribution.
#'
#' @export
explain_organ_age <- function(data, organ = c("Heart", "Kidney", "MetabInf")) {
  organ <- match.arg(organ)
  model_file <- switch(organ,
    Heart    = "pathA_heart.rds",
    Kidney   = "pathA_kidney.rds",
    MetabInf = "pathA_metab.rds"
  )
  model_path <- system.file("extdata", "models", model_file,
                            package = "ckmCRA")
  model <- readRDS(model_path)

  missing_vars <- setdiff(model$final_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("Missing variables: ", paste(missing_vars, collapse = ", "))
  }

  # 标准化
  X <- as.matrix(data[, model$final_vars, drop = FALSE])
  X_scaled <- sweep(X, 2, model$train_mean[model$final_vars], "-")
  X_scaled <- sweep(X_scaled, 2, model$train_sd[model$final_vars], "/")
  X_final <- X_scaled[, model$best_vars, drop = FALSE]

  newdata_df <- as.data.frame(X_final, stringsAsFactors = FALSE)
  colnames(newdata_df) <- model$best_vars

  pred_base <- predict(model$best_fit, newdata = newdata_df,
                       n.trees = model$best_fit$n.trees)

  # 留一扰动
  contribs <- numeric(length(model$best_vars))
  names(contribs) <- model$best_vars
  for (v in model$best_vars) {
    X_pert <- X_final
    X_pert[, v] <- 0
    pert_df <- as.data.frame(X_pert, stringsAsFactors = FALSE)
    colnames(pert_df) <- model$best_vars
    pred_pert <- predict(model$best_fit, newdata = pert_df,
                         n.trees = model$best_fit$n.trees)
    contribs[v] <- pred_base - pred_pert
  }

  out <- data.frame(
    organ        = organ,
    variable     = model$best_vars,
    raw_value    = as.numeric(data[1, model$best_vars]),
    z_score      = as.numeric(X_final[1, ]),
    contribution = as.numeric(contribs)
  )

  # ★ 符号一致性过滤
  # 如果 z_score 与 contribution 符号相反，视为共线性冲突，置为 0
  out$sign_ok <- sign(out$z_score) == sign(out$contribution) |
                 abs(out$contribution) < 0.5
  out$contribution[!out$sign_ok] <- 0
  out$sign_ok <- NULL

  out <- out[order(-abs(out$contribution)), ]
  rownames(out) <- NULL
  out
}

# 变量名到标签的映射
var_labels <- c(
  BPXSY1 = "Systolic BP",   BPXDI1 = "Diastolic BP",
  BPXPLS = "Pulse",         LBXTC = "Total Cholesterol",
  LBDLDL = "LDL",           bmi = "BMI",
  BMXWAIST = "Waist",       LBXWBCSI = "WBC",
  LBXNEPCT = "Neutrophil %", LBXLYPCT = "Lymphocyte %",
  LBXMOPCT = "Monocyte %",   LBXRDW = "RDW",
  LBXMCVSI = "MCV",         LBXSAPSI = "ALP",
  LBXSTB = "Bilirubin",     LBXSUA = "Uric Acid",
  LBXSCR = "Creatinine",    LBXSBU = "BUN",
  UACR = "UACR",            LBXSAL = "Albumin",
  LBXHGB = "Hemoglobin",    LBXGLU = "Glucose",
  LBXIN = "Insulin",        LBXGH = "HbA1c",
  LBXTR = "Triglycerides",  LBDHDD = "HDL",
  LBXSATSI = "ALT",         LBXSASSI = "AST",
  LBXSGTSI = "GGT"
)

#' @export
label_vars <- function(x) {
  out <- var_labels[x]
  out[is.na(out)] <- x[is.na(out)]
  unname(out)
}
