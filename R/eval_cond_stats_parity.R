#' Examine Conditional Statistical Parity of a Model
#'
#' This function evaluates *conditional statistical parity*, which measures fairness by comparing positive prediction rates across two groups defined by a binary protected attribute within a defined subgroup of the population. This is useful in scenarios where fairness should be evaluated in a more context-specific way—e.g., within a particular hospital unit or age bracket. Conditional statistical parity is a refinement of standard statistical parity. Instead of comparing prediction rates across groups in the entire dataset, it restricts the comparison
#' to a specified subset of the population, defined by a conditioning variable.
#'
#' The function supports both categorical and continuous conditioning variables. For continuous variables, you can supply a threshold expression like `"<50"` or `">=75"` to the \code{condition} parameter.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' binary protected attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the binary protected attribute. Must consist of only two groups.
#' @param group2 Name of the group to condition on
#' @param condition If the conditional group is categorical, the condition
#' supplied must be a character of the levels to condition on. If the conditional
#' group is continuous, the conditions supplied must be a character containing
#' the sign of the condition and the value to threshold the continuous variable
#' (e.g. "<50", ">50", "<=50", ">=50").
#' @param probs Name of the predicted outcome variable
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A list containing the following elements:
#'  - Conditions: The conditions used to calculate the conditional PPR
#'  - PPR_Group1: Positive Prediction Rate for the first group
#'  - PPR_Group2: Positive Prediction Rate for the second group
#'  - PPR_Diff: Difference in Positive Prediction Rate
#'  - PPR_Ratio: Ratio in Positive Prediction Rate
#'  If confidence intervals are computed (`confint = TRUE`):
#'  - PPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the difference in Positive Prediction
#'  Rate
#'  - PPR_Ratio_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the ratio in Positive Prediction
#'  Rate
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
#' # Evaluate Conditional Statistical Parity
#'
#' eval_cond_stats_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   group2 = "service_unit",
#'   condition = "MICU",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @seealso \code{\link{eval_stats_parity}}
#' @export

eval_cond_stats_parity <- function(data, outcome, group,
                                   group2, condition, probs,
                                   confint = TRUE,
                                   cutoff = 0.5,
                                   bootstraps = 2500, alpha = 0.05,
                                   message = TRUE,
                                   digits = 2) {
  # Check if outcome and groups are binary
  unique_values <- unique(data[[outcome]])
  groups <- unique(data[[group]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("`outcome` must be binary (containing only 0 and 1).")
  }
  if (!(length(groups) == 2)) {
    stop("`group` argument must only consist of two groups (i.e. `length(unique(data[[group]])) == 2`")
  }

  # check if the group2 is categorical or continuous
  if (is.numeric(data[[group2]])) {
    if (!grepl("^[<>=]", condition)) {
      stop("Condition must be a character containing the sign of the condition
           and the value to threshold the continuous variable
           (e.g. '<50', '>50', '<=50', '>=50').")
    } else {
      subset_data <- subset(
        data,
        eval(parse(text = paste0("data$", group2, condition)))
      )
      return(
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs, confint = confint,
          cutoff = cutoff, bootstraps = bootstraps, alpha = alpha,
          digits = digits, message = message
        )
      )
    }
  } else {
    data[[group2]] <- as.factor(data[[group2]])
    if (!condition %in% levels(data[[group2]])) {
      stop("Condition must be a character of the levels to condition on.")
    } else {
      subset_data <- subset(data, data[[group2]] == condition)
      return(
        eval_stats_parity(
          data = subset_data, outcome = outcome, group = group, probs = probs,confint = confint,
          cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
          digits = digits, message = message
        )
      )
    }
  }
}
