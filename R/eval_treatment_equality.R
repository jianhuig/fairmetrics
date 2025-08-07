#' Examine Treatment Equality of a Model
#'
#' This function evaluates *Treatment Equality*, a fairness criterion that assesses whether the
#' ratio of false negatives to false positives is similar across groups defined by a binary protected attribute.
#' Treatment Equality ensures that the model does not disproportionately favor or disadvantage any group
#' in terms of the relative frequency of missed detections (false negatives) versus false alarms (false positives).
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' binary protected attribute
#' @param outcome Name of the outcome variable
#' @param group group Name of the binary protected attribute. Must consist of only two groups.
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A list containing the following elements:
#' - False Negative / False Positive ratio for Group 1
#' - False Negative / False Positive ratio for Group 2
#' - Difference in False Negative / False Positive ratio
#' - Ratio in False Negative / False Positive ratio
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in False Negative / False Positive ratio
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the ratio in False Negative / False Positive ratio
#' @importFrom stats qnorm sd
#' @examples
#' \donttest{
#' library(fairmetrics)
#' library(dplyr)
#' library(magrittr)
#' library(randomForest)
#' # Data for tests
#' data("mimic_preprocessed")
#' set.seed(123)
#' train_data <- mimic_preprocessed %>%
#'   dplyr::filter(dplyr::row_number() <= 700)
#' # Fit a random forest model
#' rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
#' # Test the model on the remaining data
#' test_data <- mimic_preprocessed %>%
#'   dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) %>%
#'   dplyr::filter(dplyr::row_number() > 700)
#'
#' test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
#'
#' # Fairness evaluation
#' # We will use sex as the protected attribute and day_28_flg as the outcome.
#'
#' # Evaluate Treatment Equality
#' eval_treatment_equality(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41,
#'   confint = TRUE,
#'   alpha = 0.05,
#'   bootstraps = 2500,
#'   digits = 2,
#'   message = FALSE
#' )
#' }
#' @seealso \code{\link{eval_acc_parity}}, \code{\link{eval_bs_parity}}, \code{\link{eval_pos_pred_parity}},  \code{\link{eval_neg_pred_parity}}
#' @export

eval_treatment_equality <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                                    alpha = 0.05, bootstraps = 2500,
                                    digits = 2, message = TRUE) {
  # Check if outcome and groups are binary
  unique_values <- unique(data[[outcome]])
  groups <- unique(data[[group]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("`outcome` must be binary (containing only 0 and 1).")
  }
  if (!(length(groups) == 2)) {
    stop("`group` argument must only consist of two groups (i.e. `length(unique(data[[group]])) == 2`")
  }


  err_ratio <- get_err_ratio(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  err_ratio_diff <- err_ratio[[1]] - err_ratio[[2]]
  err_ratio_ratio <- err_ratio[[1]] / err_ratio[[2]]

  if(confint){
    se <- replicate(bootstraps, {
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                       replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                       replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])
      err_ratio_boot <- get_err_ratio(
        data = data_boot, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff, digits = digits
      )
      return(c(
        err_ratio_boot[[1]] - err_ratio_boot[[2]],
        log(err_ratio_boot[[1]] / err_ratio_boot[[2]])
      ))
    })
    se[!is.finite(se)] <- NA

    lower_ci <- round(err_ratio_diff - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    upper_ci <- round(err_ratio_diff + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    lower_ratio_ci <- round(exp(log(err_ratio_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)
    upper_ratio_ci <- round(exp(log(err_ratio_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)

    result_df <- data.frame(
      "(False Negative)/(False Positive) Ratio",
      err_ratio[[1]],
      err_ratio[[2]],
      err_ratio_diff,
      paste0("[", lower_ci, ", ", upper_ci, "]"),
      round(err_ratio_ratio, digits),
      paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
    )

    colnames(result_df) <- c(
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[1]),
      paste0("Group", sort(unique(data[[group]]))[2]),
      "Difference",
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )

    if (message) {
      if (lower_ci > 0 || upper_ci < 0) {
        cat("There is evidence that the model does not satisfy
            treatment equality.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            treatment equality.\n")
      }
    }
  }else{
    result_df <- data.frame(
      "(False Negative)/(False Positive) Ratio",
      err_ratio[[1]],
      err_ratio[[2]],
      err_ratio_diff,
      round(err_ratio_ratio, digits)
    )

    colnames(result_df) <- c(
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[1]),
      paste0("Group", sort(unique(data[[group]]))[2]),
      "Difference",
      "Ratio"
    )
  }


  return(result_df)
}
