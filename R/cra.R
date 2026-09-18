#' Calculate CRA (CKM Regression Accessibility) Score
#'
#' Computes a 5-dimension de-escalation accessibility score for adults
#' with cardiometabolic-kidney (CKM) syndrome.
#'
#' @param data A data frame with the following columns:
#'   \describe{
#'     \item{dm}{Diabetes status (0/1)}
#'     \item{LBXGH}{HbA1c (\%)}
#'     \item{BPXSY1}{Systolic blood pressure (mmHg)}
#'     \item{BPXDI1}{Diastolic blood pressure (mmHg)}
#'     \item{EGFR}{Estimated glomerular filtration rate (mL/min/1.73m2)}
#'     \item{UACR}{Urine albumin-to-creatinine ratio (mg/g)}
#'     \item{bmi}{Body mass index (kg/m2)}
#'   }
#'
#' @return The input data frame with 7 new columns:
#'   \code{cra_gly}, \code{cra_bp}, \code{cra_kid}, \code{cra_alb},
#'   \code{cra_ob}, \code{CRA}, \code{CRA_high}.
#'
#' @examples
#' df <- data.frame(
#'   dm = 0, LBXGH = 5.5,
#'   BPXSY1 = 120, BPXDI1 = 75,
#'   EGFR = 90, UACR = 15, bmi = 24
#' )
#' calc_cra(df)
#'
#' @export
calc_cra <- function(data) {
  required <- c("dm", "LBXGH", "BPXSY1", "BPXDI1",
                "EGFR", "UACR", "bmi")
  missing_vars <- setdiff(required, colnames(data))
  if (length(missing_vars) > 0) {
    stop("Missing required columns: ",
         paste(missing_vars, collapse = ", "))
  }

  data$cra_gly <- ifelse(data$dm == 0 | data$LBXGH < 7.5, 1, 0)
  data$cra_bp  <- ifelse(data$BPXSY1 < 130 & data$BPXDI1 < 80, 1, 0)
  data$cra_kid <- ifelse(data$EGFR >= 60, 1, 0)
  data$cra_alb <- ifelse(data$UACR < 300, 1, 0)
  data$cra_ob  <- ifelse(data$bmi < 30, 1, 0)

  data$CRA <- data$cra_gly + data$cra_bp + data$cra_kid +
              data$cra_alb + data$cra_ob
  data$CRA_high <- ifelse(data$CRA >= 4, 1, 0)

  data
}
