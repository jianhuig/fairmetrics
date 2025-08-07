#' Examine Accuracy Parity of a Model
#'
#' This function assesses *Accuracy Parity*, a fairness criterion that evaluates whether the overall accuracy of a predictive model is consistent across two groups defined by a binary protected attribute.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' binary protected attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the binary protected attribute. Must consist of only two groups.
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A list containing the following elements:
#' - Accuracy for Group 1
#' - Accuracy for Group 2
#' - Difference in accuracy
#' - Ratio in accuracy
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in accuracy
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the ratio in accurac
#' @importFrom stats qnorm sd
#' @examples
#' \donttest{
#' library(fairmetrics)
#' library(dplyr)
#' library(magrittr)
#' library(randomForest)
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
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Accuracy Parity
#' eval_acc_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#'
#' @seealso \code{\link{eval_cond_acc_equality}}
#' @export

eval_acc_parity <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
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

  acc <- get_acc(
    data = data, outcome = outcome, group = group, probs = probs,
    digits = digits, cutoff = cutoff
  )

  acc_diff <- acc[[1]] - acc[[2]]
  acc_ratio <- acc[[1]] / acc[[2]]

  if(confint){
    se <- replicate(bootstraps, {
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                       replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                       replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])
      acc_boot <- get_acc(
        data = data_boot, outcome = outcome, group = group, probs = probs,
        digits = digits, cutoff = cutoff
      )
      return(c(acc_boot[[1]] - acc_boot[[2]], log(acc_boot[[1]] / acc_boot[[2]])))
    })

    lower_ci <- round(acc_diff - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm=TRUE), digits)
    upper_ci <- round(acc_diff + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm=TRUE), digits)
    lower_ratio_ci <- round(exp(log(acc_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ],na.rm=TRUE)), digits)
    upper_ratio_ci <- round(exp(log(acc_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ],na.rm=TRUE)), digits)

    result_df <- data.frame(
      "Accuracy",
      acc[[1]],
      acc[[2]],
      acc_diff,
      paste0("[", lower_ci, ", ", upper_ci, "]"),
      round(acc_ratio, digits),
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
            accuracy parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            accuracy parity.\n")
      }
    }
  }else{
    result_df <- data.frame(
      "Accuracy",
      acc[[1]],
      acc[[2]],
      acc_diff,
      round(acc_ratio, digits)
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
