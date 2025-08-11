#' Examine Statistical Parity of a Model
#'
#' This function assesses *statistical parity* - also known as *demographic parity* - in the predictions of a binary classifier across two groups defined by a protected attribute. Statistical parity compares the rate at which different groups receive a positive prediction, irrespective of the true outcome. It reports the Positive Prediction Rate (PPR) for each group, their differences, ratios, and bootstrap-based confidence regions.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' protected attribute
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
#'   - PPR_Group1: Positive Prediction Rate for the first group
#'   - PPR_Group2: Positive Prediction Rate for the second group
#'   - PPR_Diff: Difference in Positive Prediction Rate
#'   - PPR_Ratio: The ratio in Positive Prediction Rate between the two groups.
#'   If confidence intervals are computed (`confint = TRUE`):
#'   - PPR_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'   of the 95% confidence interval for the difference in Positive Prediction
#'   Rate
#'   - PPR_Ratio_CI: A vector of length 2 containing the lower and upper bounds
#'   of the 95% confidence interval for the ratio in Positive Prediction
#'   Rate
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
#' # Evaluate Statistical Parity
#' eval_stats_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @export
eval_stats_parity <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                              bootstraps = 2500, alpha = 0.05, digits = 2,
                              message = TRUE) {
  # Check if outcome and groups are binary
  unique_values <- unique(data[[outcome]])
  groups <- unique(data[[group]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("`outcome` must be binary (containing only 0 and 1).")
  }
  if (!(length(groups) == 2)) {
    stop("`group` argument must only consist of two groups (i.e. `length(unique(data[[group]])) == 2`")
  }

  ppr <- get_ppr(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )


  ppr_diff <- ppr[[1]] - ppr[[2]]
  ppr_ratio <- ppr[[1]] / ppr[[2]]

  if(confint){
    # Calculate confidence interval
    se <- replicate(bootstraps, {
      indices1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                         replace = TRUE
      )
      indices2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                         replace = TRUE
      )
      boot_data <- rbind(data[indices1, ], data[indices2, ])
      ppr <- get_ppr(
        data = boot_data, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff, digits = digits
      )
      return(c(ppr[[1]] - ppr[[2]], log(ppr[[1]] / ppr[[2]])))
    })

    lower_ci <- round(ppr_diff - qnorm(1 - alpha / 2) * sd(unlist(se[1, ]), na.rm = TRUE), digits)
    upper_ci <- round(ppr_diff + qnorm(1 - alpha / 2) * sd(unlist(se[1, ]), na.rm = TRUE), digits)
    lower_ratio_ci <- round(exp(log(ppr_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)
    upper_ratio_ci <- round(exp(log(ppr_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)

    # Structure the results as a dataframe
    results_df <- data.frame(
      Metric = "Positive Prediction Rate",
      Group1 = ppr[[1]],
      Group2 = ppr[[2]],
      Difference = ppr_diff,
      CI = paste0("[", lower_ci, ", ", upper_ci, "]"),
      Ratio = round(ppr_ratio, digits),
      Ratio_CI = paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
    )

    colnames(results_df) <- c(
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Difference",
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )


    if (message) {
      if (lower_ci > 0 || upper_ci < 0) {
        cat("There is evidence that model does not satisfy statistical parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy
            statistical parity.\n")
      }
    }
  }else{
    # Structure the results as a dataframe
    results_df <- data.frame(
      Metric = "Positive Prediction Rate",
      Group1 = ppr[[1]],
      Group2 = ppr[[2]],
      Difference = ppr_diff,
      Ratio = round(ppr_ratio, digits)
    )

    colnames(results_df) <- c(
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Difference",
      "Ratio"
    )
  }

  return(results_df)
}
