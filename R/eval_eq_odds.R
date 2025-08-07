#' Examine Equalized Odds of a Predictive Model
#'
#' This function evaluates whether a predictive model satisfies the Equalized
#' Odds criterion by comparing both False Negative Rates (FNR) and False
#' Positive Rates (FPR) across two groups defined by a binary protected
#' attribute. It reports the rate for each group, their differences, ratios, and
#' bootstrap-based confidence regions. A Bonferroni-corrected union test is used
#' to test whether the model violates the Equalized Odds criterion.
#'
#' @param data A data frame containing the true binary outcomes, predicted
#' probabilities, and binary protected attribute.
#' @param outcome A string specifying the name of the binary outcome variable in
#' \code{data}.
#' @param group Name of the binary protected attribute. Must consist of only two groups.
#' @param probs A string specifying the name of the variable containing
#' predicted probabilities or risk scores.
#' @param cutoff A numeric value used to threshold predicted probabilities into
#' binary predictions; defaults to 0.5.
#' @param confint Whether to compute 95% confidence interval, default is TRUE.
#' @param bootstraps An integer specifying the number of bootstrap resamples for
#' constructing confidence intervals; vdefaults to 2500.
#' @param alpha Significance level for the (1 - \code{alpha}) confidence
#' interval; defaults to 0.05.
#' @param digits Number of decimal places to round numeric results; defaults to
#' 2.
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
#' @return A data frame summarizing group disparities in both FNR and FPR with
#' the following columns:
#' \itemize{
#'   \item \code{Metric}: The reported metrics ("FNR; FPR").
#'   \item \code{Group1}: Estimated FNR and FPR for the first group.
#'   \item \code{Group2}: Estimated FNR and FPR for the second group.
#'   \item \code{Difference}: Differences in FNR and FPR, computed as Group1 -
#'   Group2.
#'   \item \code{95\% CR}: Bonferroni-adjusted confidence regions for the
#'   differences.
#'   \item \code{Ratio}: Ratios in FNR and FPR, computed as Group1 / Group2.
#'   \item \code{95\% CR}: Bonferroni-adjusted confidence regions for the ratios.
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
#'   data = train_data, ntree = 1000
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
#' # Evaluate Equalized Odds
#' eval_eq_odds(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @export
eval_eq_odds <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
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

  fnr <- 1 - get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  fnr_diff <- fnr[[1]] - fnr[[2]]
  fpr_diff <- fpr[[1]] - fpr[[2]]
  fnr_ratio <- fnr[[1]] / fnr[[2]]
  fpr_ratio <- fpr[[1]] / fpr[[2]]

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

      boot_fnr <- 1 - get_tpr(
        data = boot_data, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff, digits = digits
      )
      boot_fpr <- get_fpr(
        data = boot_data, outcome = outcome, group = group, probs = probs,
        cutoff = cutoff, digits = digits
      )
      c(
        boot_fnr[[1]] - boot_fnr[[2]], boot_fpr[[1]] - boot_fpr[[2]],
        log(boot_fnr[[1]] / boot_fnr[[2]]), log(boot_fpr[[1]] / boot_fpr[[2]])
      )
    })

    # Calculate confidence intervals
    fnr_lower <- round(fnr_diff - qnorm(1 - alpha / 4) * sd(se[1, ],na.rm=TRUE), digits)
    fnr_upper <- round(fnr_diff + qnorm(1 - alpha / 4) * sd(se[1, ],na.rm=TRUE), digits)
    fpr_lower <- round(fpr_diff - qnorm(1 - alpha / 4) * sd(se[2, ],na.rm=TRUE), digits)
    fpr_upper <- round(fpr_diff + qnorm(1 - alpha / 4) * sd(se[2, ],na.rm=TRUE), digits)
    fnr_ratio_lower <- round(exp(log(fnr_ratio) - qnorm(1 - alpha / 4) *
                                   sd(se[3, ],na.rm=TRUE)), digits)
    fnr_ratio_upper <- round(exp(log(fnr_ratio) + qnorm(1 - alpha / 4) *
                                   sd(se[3, ],na.rm=TRUE)), digits)
    fpr_ratio_lower <- round(exp(log(fpr_ratio) - qnorm(1 - alpha / 4) *
                                   sd(se[4, ],na.rm=TRUE)), digits)
    fpr_ratio_upper <- round(exp(log(fpr_ratio) + qnorm(1 - alpha / 4) *
                                   sd(se[4, ],na.rm=TRUE)), digits)

    # Structure the results as a dataframe
    results_df <- data.frame(
      Metric = c("FNR; FPR"),
      Group1 = paste0(fnr[[1]], "; ", fpr[[1]]),
      Group2 = paste0(fnr[[2]], "; ", fpr[[2]]),
      Difference = paste0(fnr_diff, "; ", fpr_diff),
      CI =
        paste0(
          "[", fnr_lower, ", ", fnr_upper, "]", "; ",
          "[", fpr_lower, ", ", fpr_upper, "]"
        ),
      Ratio = paste0(round(fnr_ratio, digits), "; ", round(fpr_ratio, digits)),
      Ratio_CI =
        paste0(
          "[", fnr_ratio_lower, ", ", fnr_ratio_upper, "]", "; ",
          "[", fpr_ratio_lower, ", ", fpr_ratio_upper, "]"
        )
    )

    colnames(results_df) <- c(
      "Metric",
      paste0("Group ", sort(unique(data[[group]]))[[1]]),
      paste0("Group ", sort(unique(data[[group]]))[[2]]),
      "Difference", 
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )

    # Print summary message if desired
    if (message) {
      if (any(fnr_lower > 0) || any(fnr_upper < 0) || any(fpr_lower > 0) ||
          any(fpr_upper < 0)) {
        cat("There is evidence that model does not satisfy equalized odds.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy the
          equalized odds criterion.\n")
      }
    }
  }else{

    # Structure the results as a dataframe
    results_df <- data.frame(
      Metric = c("FNR; FPR"),
      Group1 = paste0(fnr[[1]], "; ", fpr[[1]]),
      Group2 = paste0(fnr[[2]], "; ", fpr[[2]]),
      Difference = paste0(fnr_diff, "; ", fpr_diff),
      Ratio = paste0(round(fnr_ratio, digits), "; ", round(fpr_ratio, digits))
    )

    colnames(results_df) <- c(
      "Metric",
      paste0("Group ", sort(unique(data[[group]]))[[1]]),
      paste0("Group ", sort(unique(data[[group]]))[[2]]),
      "Difference", "Ratio"
    )
  }



  return(results_df)
}
