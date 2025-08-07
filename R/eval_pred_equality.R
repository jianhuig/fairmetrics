#' Examine Predictive Equality of a Model
#'
#' This function evaluates predictive equality, a fairness metric that compares the
#' False Positive Rate (FPR) between groups defined by a binary protected attribute. It assesses
#' whether individuals from different groups are equally likely to be incorrectly flagged as
#' positive when they are, in fact, negative.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' binary protected attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the protected attribute. Must consist of only two groups.
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A list containing the following elements:
#' - FPR_Group1: False Positive Rate for the first group
#' - FPR_Group2: False Positive Rate for the second group
#' - FPR_Diff: Difference in False Positive Rate
#' - FPR_Ratio: Ratio in False Positive Rate
#' If confidence intervals are computed (`confint = TRUE`):
#' - FPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in False Positive Rate
#' - FPR_Ratio_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the ratio in False Positive Rate
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
#' # We will use sex as the protectedR attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Predictive Equality
#' eval_pred_equality(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @seealso \code{\link{eval_pos_pred_parity}},  \code{\link{eval_neg_pred_parity}}, \code{\link{eval_stats_parity}}
#' @export

eval_pred_equality <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
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


  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  fpr_dif <- fpr[[1]] - fpr[[2]]
  fpr_ratio <- fpr[[1]] / fpr[[2]]

  if(confint){
    se <- replicate(bootstraps, {
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                       replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                       replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])
      fpr_boot <- get_fpr(
        data = data_boot, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff
      )
      return(c(fpr_boot[[1]] - fpr_boot[[2]], log(fpr_boot[[1]] / fpr_boot[[2]])))
    })

    lower_ci <- round(fpr_dif - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    upper_ci <- round(fpr_dif + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    lower_ratio_ci <- round(exp(log(fpr_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)
    upper_ratio_ci <- round(exp(log(fpr_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)

    result_df <- data.frame(
      "False Positive Rate",
      fpr[[1]],
      fpr[[2]],
      fpr_dif,
      paste0("[", lower_ci, ", ", upper_ci, "]"),
      round(fpr_ratio, digits),
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
        cat("There is evidence that model does not satisfy predictive
            equality.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            predictive equality.\n")
      }
    }
  }else{
    result_df <- data.frame(
      "False Positive Rate",
      fpr[[1]],
      fpr[[2]],
      fpr_dif,
      round(fpr_ratio, digits)
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
