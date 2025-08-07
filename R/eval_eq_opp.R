#' Evaluate Equal Opportunity Compliance of a Predictive Model
#'
#' This function evaluates the fairness of a predictive model with respect to
#' the Equal Opportunity criterion, which requires that the False Negative Rate
#' (FNR) be comparable across groups defined by a binary protected attribute. The
#' function quantifies disparities in FNR between two groups and provides both
#' the absolute difference and ratio, along with confidence intervals obtained
#' via bootstrapping.
#'
#' @param data A data frame containing the true binary outcomes, predicted
#' probabilities, and binary protected attribute.
#' @param outcome A string specifying the name of the binary outcome variable in
#' \code{data}.
#' @param group group Name of the binary protected attribute. Must consist of only two groups.
#' @param probs A string specifying the name of the variable containing
#' predicted probabilities or risk scores.
#' @param cutoff A numeric value used to threshold predicted probabilities into
#' binary decisions; defaults to 0.5.
#' @param confint Whether to compute 95% confidence interval, default is TRUE.
#' @param bootstraps An integer specifying the number of bootstrap resamples for
#' constructing confidence intervals; defaults to 2500.
#' @param alpha Significance level for constructing the (1 - \code{alpha})
#' confidence interval; defaults to 0.05.
#' @param digits Integer indicating the number of decimal places to round
#' results to; defaults to 2.
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A data frame summarizing FNR-based group disparity metrics with the
#' following columns:
#'
#' \itemize{
#'   \item \code{Metric} A label indicating the reported fairness criterion.
#'   \item \code{Group1} Estimated FNR and FPR for the first group.
#'   \item \code{Group2} Estimated FNR and FPR for the second group.
#'   \item \code{Difference} The difference in FNR between the two groups, computed as the FNR of Group1 minus the FNR of Group2.
#'   \item \code{1-alpha\% Diff CI} The (1 - \code{alpha}) confidence interval for the FNR difference.
#'   \item \code{Ratio} The ratio of FNRs between Group1 and Group2, computed as FNR for Group1 divided by FNR for Group2.
#'   \item \code{1-alpha\% Ratio CI} The corresponding confidence interval for the FNR ratio.
#' }
#'
#' @importFrom stats qnorm sd
#'
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
#' rf_model <- randomForest::randomForest(factor(day_28_flg) ~ .,
#'   data =
#'     train_data, ntree = 1000
#' )
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
#' # Evaluate Equal Opportunity Compliance
#' eval_eq_opp(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @export

eval_eq_opp <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                        bootstraps = 2500, alpha = 0.05, digits = 2,
                        message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  # Check if outcome and groups are binary
  unique_values <- unique(data[[outcome]])
  groups <- unique(data[[group]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("`outcome` must be binary (containing only 0 and 1).")
  }
  if (!(length(groups) == 2)) {
    stop("`group` argument must only consist of two groups (i.e. `length(unique(data[[group]])) == 2`")
  }
  fnr <- 1 - get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  fnr_diff <- fnr[[1]] - fnr[[2]]
  fnr_ratio <- fnr[1] / fnr[[2]]

  if(confint){
    # Calculate difference confidence interval
    se <- replicate(bootstraps, {
      # Bootstrap within each group
      group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
                       replace = TRUE
      )
      group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
                       replace = TRUE
      )
      data_boot <- rbind(data[group1, ], data[group2, ])

      fnr_boot <- 1 - get_tpr(
        data = data_boot, outcome = outcome, group = group,
        probs = probs, cutoff = cutoff
      )
      return(c(fnr_boot[[1]] - fnr_boot[[2]], log(fnr_boot[[1]] / fnr_boot[[2]])))
    })

    lower_ci <- round(fnr_diff - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    upper_ci <- round(fnr_diff + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
    lower_ratio_ci <- round(exp(log(fnr_ratio) - qnorm(1 - alpha / 2) *
                                  sd(se[2, ], na.rm=TRUE)), digits)
    upper_ratio_ci <- round(exp(log(fnr_ratio) + qnorm(1 - alpha / 2) *
                                  sd(se[2, ], na.rm=TRUE)), digits)

    # Create a dataframe for the results
    results_df <- data.frame(
      "False Negative Rate",
      fnr[[1]],
      fnr[[2]],
      fnr_diff,
      paste0("[", lower_ci, ", ", upper_ci, "]"),
      round(fnr_ratio, digits),
      paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
    )

    colnames(results_df) <- c(
      "Metric", paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Difference", 
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )

    # Print message if desired
    if (message) {
      if (lower_ci > 0 | upper_ci < 0) {
        cat("There is evidence that model does not satisfy equal opportunity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy equal
          opportunity.\n")
      }
    }
  } else{
    results_df <- data.frame(
      "False Negative Rate",
      fnr[[1]],
      fnr[[2]],
      fnr_diff,
      round(fnr_ratio, digits)
    )

    colnames(results_df) <- c(
      "Metric", paste0("Group", sort(unique(data[[group]]))[[1]]),
      paste0("Group", sort(unique(data[[group]]))[[2]]),
      "Difference", "Ratio"
    )
  }



  return(results_df)
}
