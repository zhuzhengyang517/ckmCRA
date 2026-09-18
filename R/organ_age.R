#' Predict Organ-Specific Biological Age (Path A)
#'
#' @param data Data frame with clinical biomarkers.
#' @param organ One of 'Heart', 'Kidney', 'MetabInf'.
#'
#' @return Data frame with new column '<organ>Age_ML'.
#'   For MetabInf, the column is named 'MetabAge_ML'.
#'
#' @export
predict_organ_age <- function(data, organ = c("Heart", "Kidney", "MetabInf")) {
  organ <- match.arg(organ)

  model_file <- switch(organ,
    Heart    = "pathA_heart.rds",
    Kidney   = "pathA_kidney.rds",
    MetabInf = "pathA_metab.rds"
  )
  model_path <- system.file("extdata", "models", model_file,
                            package = "ckmCRA")
  if (!file.exists(model_path)) stop("Model not found: ", model_path)
  model <- readRDS(model_path)

  missing_vars <- setdiff(model$final_vars, colnames(data))
  if (length(missing_vars) > 0) {
    stop("Missing variables for ", organ, ": ",
         paste(missing_vars, collapse = ", "))
  }

  X <- as.matrix(data[, model$final_vars, drop = FALSE])
  X_scaled <- sweep(X, 2, model$train_mean[model$final_vars], "-")
  X_scaled <- sweep(X_scaled, 2, model$train_sd[model$final_vars], "/")
  X_final <- X_scaled[, model$best_vars, drop = FALSE]

  newdata_df <- as.data.frame(X_final, stringsAsFactors = FALSE)
  colnames(newdata_df) <- model$best_vars
  rownames(newdata_df) <- NULL

  pred <- gbm::predict.gbm(model$best_fit,
                  newdata = newdata_df,
                  n.trees = model$best_fit$n.trees)

  # ★ 统一列名：MetabInf → Metab
  out_col <- if (organ == "MetabInf") "MetabAge_ML" else paste0(organ, "Age_ML")
  data[[out_col]] <- as.numeric(pred)
  data
}

#' Calculate Organ Age Gaps
#'
#' @param data Data frame with organ age predictions and 'age'.
#' @param organ One of 'Heart', 'Kidney', 'MetabInf'.
#'
#' @return Data frame with new column '<organ>AgeGap_ML'.
#'   For MetabInf, the column is named 'MetabAgeGap_ML'.
#'
