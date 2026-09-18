#' Prepare Data for CKM-CRA Pipeline
#'
#' Converts raw NHANES columns (sex, race, educ, pir_cat) into
#' factor columns (sex_f, race_f, educ_f, pir_f) required by the
#' mortality models. Missing columns default to reference categories.
#'
#' @param data Data frame with at least sex and race columns.
#'
#' @return Data frame with sex_f, race_f, educ_f, pir_f added.
#'
#' @export
prepare_data <- function(data) {
  # --- sex_f ---
  if (!"sex_f" %in% colnames(data)) {
    if ("sex" %in% colnames(data)) {
      data$sex_f <- factor(
        ifelse(data$sex == 1, "Male", "Female"),
        levels = c("Male", "Female")
      )
    } else {
      data$sex_f <- factor("Male", levels = c("Male", "Female"))
    }
  }

  # --- race_f ---
  if (!"race_f" %in% colnames(data)) {
    race_levels <- c("Mexican American", "Other Hispanic",
                     "NH White", "NH Black", "Other")
    if ("race" %in% colnames(data)) {
      race_labels <- c("Mexican American", "Other Hispanic",
                       "NH White", "NH Black", "Other")
      data$race_f <- factor(race_labels[data$race],
                            levels = race_levels)
    } else {
      data$race_f <- factor("NH White", levels = race_levels)
    }
  }

  # --- educ_f ---
  if (!"educ_f" %in% colnames(data)) {
    educ_levels <- c("<HS", "HS", "College+")
    if ("educ" %in% colnames(data)) {
      educ_labels <- c("<HS", "HS", "College+")
      data$educ_f <- factor(educ_labels[data$educ],
                            levels = educ_levels)
    } else {
      data$educ_f <- factor("HS", levels = educ_levels)
    }
  }

  # --- pir_f ---
  if (!"pir_f" %in% colnames(data)) {
    pir_levels <- c("PIR<1.3", "PIR 1.3-3.5", "PIR>=3.5")
    if ("pir_cat" %in% colnames(data)) {
      pir_labels <- c("PIR<1.3", "PIR 1.3-3.5", "PIR>=3.5")
      data$pir_f <- factor(pir_labels[data$pir_cat],
                           levels = pir_levels)
    } else {
      data$pir_f <- factor("PIR 1.3-3.5", levels = pir_levels)
    }
  }

  data
}
