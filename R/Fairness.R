#' Evaluate Equal Opportunity Compliance of a Predictive Model
#'
#' This function evaluates the fairness of a predictive model with respect to
#' the Equal Opportunity criterion, which requires that the False Negative Rate
#' (FNR) be comparable across groups defined by a sensitive attribute. The
#' function quantifies disparities in FNR between two groups and provides both
#' the absolute difference and ratio, along with confidence intervals obtained
#' via bootstrapping.
#'
#' @param data A data frame containing the true binary outcomes, predicted
#' probabilities, and sensitive group membership.
#' @param outcome A string specifying the name of the binary outcome variable in
#' \code{data}.
#' @param group A string specifying the name of the sensitive attribute variable
#' (e.g., race, gender).
#' @param probs A string specifying the name of the variable containing
#' predicted probabilities or risk scores.
#' @param cutoff A numeric value used to threshold predicted probabilities into
#' binary decisions; defaults to 0.5.
#' @param bootstraps An integer specifying the number of bootstrap resamples for
#' constructing confidence intervals; defaults to 2500.
#' @param alpha Significance level for constructing the (1 - \code{alpha})
#' confidence interval; defaults to 0.05.
#' @param digits Integer indicating the number of decimal places to round
#' results to; defaults to 2.
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation.
#' @return A data frame summarizing FNR-based group disparity metrics with the
#' following columns:
#'
#' \itemize{
#'   \item \code{Metric} A label indicating the reported fairness criterion.
#'   \item \code{Group1} Estimated FNR and FPR for the first group.
#'   \item \code{Group2} Estimated FNR and FPR for the second group.
#'   \item \code{Difference} The difference in FNR between the two groups, computed as the FNR of Group1 minus the FNR of Group2.
#'   \item \code{95\% Diff CI} The (1 - \code{alpha}) confidence interval for the FNR difference.
#'   \item \code{Ratio} The ratio of FNRs between Group1 and Group2, computed as FNR for Group1 divided by FNR for Group2.
#'   \item \code{95\% Ratio CI} The corresponding confidence interval for the FNR ratio.
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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

eval_eq_opp <- function(data, outcome, group, probs, cutoff = 0.5,
                        bootstraps = 2500, alpha = 0.05, digits = 2,
                        message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fnr <- 1 - get_tpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  fnr_diff <- fnr[[1]] - fnr[[2]]
  fnr_ratio <- fnr[1] / fnr[[2]]

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

  lower_ci <- round(fnr_diff - qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  upper_ci <- round(fnr_diff + qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  lower_ratio_ci <- round(exp(log(fnr_ratio) - qnorm(1 - alpha / 2) *
    sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(fnr_ratio) + qnorm(1 - alpha / 2) *
    sd(se[2, ])), digits)

  # Create a dataframe for the results
  results_df <- data.frame(
    "FNR",
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
    "Difference", "95% Diff CI", "Ratio", "95% Ratio CI"
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

  return(results_df)
}


#' Examine Equalized Odds of a Predictive Model
#'
#' This function evaluates whether a predictive model satisfies the Equalized
#' Odds criterion by comparing both False Negative Rates (FNR) and False
#' Positive Rates (FPR) across two groups defined by a binary sensitive
#' attribute. It reports the rate for each group, their differences, ratios, and
#' bootstrap-based confidence regions. A Bonferroni-corrected union test is used
#' to test whether the model violates the Equalized Odds criterion.
#'
#' @param data A data frame containing the true binary outcomes, predicted
#' probabilities, and sensitive group membership.
#' @param outcome A string specifying the name of the binary outcome variable in
#' \code{data}.
#' @param group A string specifying the name of the binary sensitive attribute
#' variable (e.g., race, gender) used to define the comparison groups.
#' @param probs A string specifying the name of the variable containing
#' predicted probabilities or risk scores.
#' @param cutoff A numeric value used to threshold predicted probabilities into
#' binary predictions; defaults to 0.5.
#' @param bootstraps An integer specifying the number of bootstrap resamples for
#' constructing confidence intervals; vdefaults to 2500.
#' @param alpha Significance level for the (1 - \code{alpha}) confidence
#' interval; defaults to 0.05.
#' @param digits Number of decimal places to round numeric results; defaults to
#' 2.
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation.
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
eval_eq_odds <- function(data, outcome, group, probs, cutoff = 0.5,
                         bootstraps = 2500, alpha = 0.05, digits = 2,
                         message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
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
  fnr_lower <- round(fnr_diff - qnorm(1 - alpha / 4) * sd(se[1, ]), digits)
  fnr_upper <- round(fnr_diff + qnorm(1 - alpha / 4) * sd(se[1, ]), digits)
  fpr_lower <- round(fpr_diff - qnorm(1 - alpha / 4) * sd(se[2, ]), digits)
  fpr_upper <- round(fpr_diff + qnorm(1 - alpha / 4) * sd(se[2, ]), digits)
  fnr_ratio_lower <- round(exp(log(fnr_ratio) - qnorm(1 - alpha / 4) *
    sd(se[3, ])), digits)
  fnr_ratio_upper <- round(exp(log(fnr_ratio) + qnorm(1 - alpha / 4) *
    sd(se[3, ])), digits)
  fpr_ratio_lower <- round(exp(log(fpr_ratio) - qnorm(1 - alpha / 4) *
    sd(se[4, ])), digits)
  fpr_ratio_upper <- round(exp(log(fpr_ratio) + qnorm(1 - alpha / 4) *
    sd(se[4, ])), digits)

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
    "Difference", "95% CR", "Ratio", "95% CR"
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

  return(results_df)
}


#' Examine Statistical Parity of a Model
#'
#' This function assesses *statistical parity* - also known as *demographic parity* - in the predictions of a binary classifier across two groups defined by a sensitive attribute. Statistical parity compares the rate at which different groups receive a positive prediction, irrespective of the true outcome. It reports the Positive Prediction Rate (PPR) for each group, their differences, ratios, and bootstrap-based confidence regions.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
#' @seealso \code{\link{eval_cond_stats_parity}}
#' @export
eval_stats_parity <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                              bootstraps = 2500, alpha = 0.05, digits = 2,
                              message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppr <- get_ppr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )

  ppr_diff <- ppr[[1]] - ppr[[2]]
  ppr_ratio <- ppr[[1]] / ppr[[2]]

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

  lower_ci <- round(ppr_diff - qnorm(1 - alpha / 2) * sd(unlist(se[1, ])), digits)
  upper_ci <- round(ppr_diff + qnorm(1 - alpha / 2) * sd(unlist(se[1, ])), digits)
  lower_ratio_ci <- round(exp(log(ppr_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(ppr_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ])), digits)

  # Structure the results as a dataframe
  results_df <- data.frame(
    Metric = "PPR",
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
    "Difference", "95% Diff CI", "Ratio", "95% Ratio CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that model does not satisfy statistical parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            statistical parity.\n")
    }
  }

  return(results_df)
}

#' Examine Conditional Statistical Parity of a Model
#'
#' This function evaluates *conditional statistical parity*, which measures fairness by comparing positive prediction rates across sensitive groups within a defined subgroup of the population. This is useful in scenarios where fairness should be evaluated in a more context-specific way—e.g., within a particular hospital unit or age bracket. Conditional statistical parity is a refinement of standard statistical parity. Instead of comparing prediction rates across groups in the entire dataset, it restricts the comparison
#' to a specified subset of the population, defined by a conditioning variable.
#'
#' The function supports both categorical and continuous conditioning variables. For continuous variables, you can supply a threshold expression like `"<50"` or `">=75"` to the \code{condition} parameter.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param group2 Name of the group to condition on
#' @param condition If the conditional group is categorical, the condition
#' supplied must be a character of the levels to condition on. If the conditional
#' group is continuous, the conditions supplied must be a character containing
#' the sign of the condition and the value to threshold the continuous variable
#' (e.g. "<50", ">50", "<=50", ">=50").
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
                                   cutoff = 0.5,
                                   bootstraps = 2500, alpha = 0.05,
                                   message = TRUE,
                                   digits = 2) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
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
          data = subset_data, outcome = outcome, group = group, probs = probs,
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
          data = subset_data, outcome = outcome, group = group, probs = probs,
          cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
          digits = digits, message = message
        )
      )
    }
  }
}

#' Examine Predictive Parity of a Model
#'
#' This function evaluates *predictive parity (PP)*, a key fairness criterion that
#' compares the *Positive Predictive Value (PPV)* between groups defined by a sensitive attribute.
#' In other words, it assesses whether, among individuals predicted to be positive,
#' the probability of being truly positive is equal across subgroups.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#'  - PPV_Group1: Positive Predictive Value for the first group
#'  - PPV_Group2: Positive Predictive Value for the second group
#'  - PPV_Diff: Difference in Positive Predictive Value
#'  - PPV_Ratio: Ratio in Positive Predictive Value
#'  If confidence intervals are computed (`confint = TRUE`):
#'  - PPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the difference in Positive Predictive
#'  Value
#'  - PPV_Ratio_CI: A vector of length 2 containing the lower and upper bounds
#'  of the 95% confidence interval for the ratio in Positive Predictive
#'  Value
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Predictive Parity
#' eval_pred_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @export

eval_pred_parity <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                             bootstraps = 2500, alpha = 0.05,
                             digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- sort(unique(data[[outcome]]))
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_dif <- ppv[[1]] - ppv[[2]]
  ppv_ratio <- ppv[[1]] / ppv[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == sort(unique(data[[group]]))[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == sort(unique(data[[group]]))[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    ppv_boot <- get_ppv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    return(c(ppv_boot[[1]] - ppv_boot[[2]], log(ppv_boot[[1]] / ppv_boot[[2]])))
  })

  lower_ci <- round(ppv_dif - qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  upper_ci <- round(ppv_dif + qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  lower_ratio_ci <- round(exp(log(ppv_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(ppv_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ])), digits)

  result_df <- data.frame(
    "PPV",
    ppv[[1]],
    ppv[[2]],
    ppv_dif,
    paste0("[", lower_ci, ", ", upper_ci, "]"),
    round(ppv_ratio, digits),
    paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
  )
  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that model does not satisfy predictive parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            predictive parity.\n")
    }
  }

  return(result_df)
}

#' Examine Predictive Equality of a Model
#'
#' This function evaluates predictive equality, a fairness metric that compares the
#' False Positive Rate (FPR) between groups defined by a sensitive attribute. It assesses
#' whether individuals from different groups are equally likely to be incorrectly flagged as
#' positive when they are, in fact, negative.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
#' @seealso \code{\link{eval_pred_parity}}, \code{\link{eval_stats_parity}}
#' @export

eval_pred_equality <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  fpr <- get_fpr(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff
  )

  fpr_dif <- fpr[[1]] - fpr[[2]]
  fpr_ratio <- fpr[[1]] / fpr[[2]]

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

  lower_ci <- round(fpr_dif - qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  upper_ci <- round(fpr_dif + qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  lower_ratio_ci <- round(exp(log(fpr_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(fpr_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ])), digits)

  result_df <- data.frame(
    "FPR",
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
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
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

  return(result_df)
}

#' Examine Conditional Use Accuracy Equality of a Model
#'
#' This function evaluates *Conditional Use Accuracy Equality*, a fairness criterion
#' that requires predictive performance to be similar across groups when a model
#' makes positive or negative predictions.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the sensitive attribute
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - PPV_Group1: Positive Predictive Value for the first group
#' - PPV_Group2: Positive Predictive Value for the second group
#' - PPV_Diff: Difference in Positive Predictive Value
#' - NPV_Group1: Negative Predictive Value for the first group
#' - NPV_Group2: Negative Predictive Value for the second group
#' - NPV_Diff: Difference in Negative Predictive Value
#' If confidence intervals are computed (`confint = TRUE`):
#' - PPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in Positive Predictive
#' Value
#' - NPV_Diff_CI: A vector of length 2 containing the lower and upper bounds
#' of the 95% confidence interval for the difference in Negative Predictive
#' Value
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Conditional Use Accuracy Equality
#' eval_cond_acc_equality(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @seealso \code{\link{eval_acc_parity}}
#' @export

eval_cond_acc_equality <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                                   alpha = 0.05, bootstraps = 2500,
                                   digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  npv <- get_npv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_diff <- ppv[[1]] - ppv[[2]]
  npv_diff <- npv[[1]] - npv[[2]]

  # Calculate confidence interval
  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    ppv_boot <- get_ppv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    npv_boot <- get_npv(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      cutoff = cutoff, digits = digits
    )
    c(ppv_boot[[1]] - ppv_boot[[2]], npv_boot[[1]] - npv_boot[[2]])
  })

  ppv_lower_ci <- round(ppv_diff - qnorm(1 - alpha / 4) * sd(se[1, ]), digits)
  ppv_upper_ci <- round(ppv_diff + qnorm(1 - alpha / 4) * sd(se[1, ]), digits)
  npv_lower_ci <- round(npv_diff - qnorm(1 - alpha / 4) * sd(se[2, ]), digits)
  npv_upper_ci <- round(npv_diff + qnorm(1 - alpha / 4) * sd(se[2, ]), digits)

  result_df <- data.frame(
    Metric = c("PPV; NPV"),
    Group1 = paste0(ppv[[1]], "; ", npv[[1]]),
    Group2 = paste0(ppv[[2]], "; ", npv[[2]]),
    Difference = paste0(ppv_diff, "; ", npv_diff),
    CI =
      paste0(
        "[", ppv_lower_ci, ", ", ppv_upper_ci, "]", "; ",
        "[", npv_lower_ci, ", ", npv_upper_ci, "]"
      )
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% CI"
  )

  if (message) {
    if (ppv_lower_ci > 0 || ppv_upper_ci < 0 || npv_lower_ci > 0 ||
      npv_upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            conditional use accuracy equality.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            conditional use accuracy equality.\n")
    }
  }

  return(result_df)
}

#' Examine Accuracy Parity of a Model
#'
#' This function assesses *Accuracy Parity*, a fairness criterion that evaluates whether
#' the overall accuracy of a predictive model is consistent across different groups.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  acc <- get_acc(
    data = data, outcome = outcome, group = group, probs = probs,
    digits = digits, cutoff = cutoff
  )

  acc_diff <- acc[[1]] - acc[[2]]
  acc_ratio <- acc[[1]] / acc[[2]]

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

  lower_ci <- round(acc_diff - qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  upper_ci <- round(acc_diff + qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  lower_ratio_ci <- round(exp(log(acc_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(acc_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ])), digits)

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
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
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
  return(result_df)
}

#' Examine Brier Score Parity of a Model
#'
#' This function evaluates *Brier Score Parity*, a fairness measure that checks whether the Brier score
#' (a measure of the calibration of probabilistic predictions) is similar across different groups. Brier score
#' parity ensures that the model's predicted probabilities are equally well calibrated across subpopulations.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Brier Score for Group 1
#' - Brier Score for Group 2
#' - Difference in Brier Score
#' - Ratio in Brier Score
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in Brier Score
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the ratio in Brier Score
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#'
#' # Evaluate Brier Score Parity
#' eval_bs_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred"
#' )
#' }
#'
#' @seealso \code{\link{eval_acc_parity}}, \code{\link{eval_cond_acc_equality}}, \code{\link{eval_pred_parity}}
#' @export

eval_bs_parity <- function(data, outcome, group, probs, confint = TRUE,
                           alpha = 0.05, bootstraps = 2500,
                           digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  bs <- get_brier_score(
    data = data, outcome = outcome, group = group, probs = probs,
    digits = digits
  )

  bs_diff <- bs[[1]] - bs[[2]]
  bs_ratio <- bs[[1]] / bs[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    bs_boot <- get_brier_score(
      data = data_boot, outcome = outcome, group = group, probs = probs,
      digits = digits
    )
    return(c(bs_boot[[1]] - bs_boot[[2]], log(bs_boot[[1]] / bs_boot[[2]])))
  })

  lower_ci <- round(bs_diff - qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  upper_ci <- round(bs_diff + qnorm(1 - alpha / 2) * sd(se[1, ]), digits)
  lower_ratio_ci <- round(exp(log(bs_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ])), digits)
  upper_ratio_ci <- round(exp(log(bs_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ])), digits)

  result_df <- data.frame(
    "Brier Score",
    bs[[1]],
    bs[[2]],
    bs_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]"),
    round(bs_ratio, digits),
    paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            Brier Score parity.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            Brier Score parity.\n")
    }
  }

  return(result_df)
}

#' Examine Treatment Equality of a Model
#'
#' This function evaluates *Treatment Equality*, a fairness criterion that assesses whether the
#' ratio of false negatives to false positives is similar across groups (e.g., based on gender or race).
#' Treatment Equality ensures that the model does not disproportionately favor or disadvantage any group
#' in terms of the relative frequency of missed detections (false negatives) versus false alarms (false positives).
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param cutoff Cutoff value for the predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
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
#' @seealso \code{\link{eval_acc_parity}}, \code{\link{eval_bs_parity}}, \code{\link{eval_pred_parity}}
#' @export

eval_treatment_equality <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                                    alpha = 0.05, bootstraps = 2500,
                                    digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  err_ratio <- get_err_ratio(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  err_ratio_diff <- err_ratio[[1]] - err_ratio[[2]]
  err_ratio_ratio <- err_ratio[[1]] / err_ratio[[2]]

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
    "FN/FP Ratio",
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
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
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

  return(result_df)
}

#' Examine Balance for the Positive Class of a Model
#'
#' This function evaluates *Balance for the Positive Class*, a fairness criterion
#' that checks whether the model assigns similar predicted probabilities across groups
#' among individuals whose true outcome is positive (i.e., \(Y = 1\)).
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Average predicted probability for Group 1
#' - Average predicted probability for Group 2
#' - Difference in average predicted probability
#' - Ratio in average predicted probability
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in average predicted probability
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the ratio in average predicted probability
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#'
#' # Evaluate Balance for Positive Class
#' eval_pos_class_bal(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred"
#' )
#' }
#' @seealso \code{\link{eval_neg_class_bal}}
#' @export

eval_pos_class_bal <- function(data, outcome, group, probs, confint = TRUE,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  pos_data <- data[data[[outcome]] == 1, ]
  avg_prob <- get_avg_prob(
    data = pos_data, group = group, probs = probs, digits = digits
  )

  avg_prob_diff <- avg_prob[[1]] - avg_prob[[2]]
  avg_prob_ratio <- avg_prob[[1]] / avg_prob[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    pos_data_boot <- data_boot[data_boot[[outcome]] == 1, ]
    avg_prob_boot <- get_avg_prob(
      data = pos_data_boot, group = group, probs = probs
    )
    return(c(
      avg_prob_boot[[1]] - avg_prob_boot[[2]],
      log(avg_prob_boot[[1]] / avg_prob_boot[[2]])
    ))
  })

  lower_ci <- round(avg_prob_diff - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
  upper_ci <- round(avg_prob_diff + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
  lower_ratio_ci <- round(exp(log(avg_prob_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)
  upper_ratio_ci <- round(exp(log(avg_prob_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)

  result_df <- data.frame(
    "Avg. Predicted Prob.",
    avg_prob[[1]],
    avg_prob[[2]],
    avg_prob_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]"),
    round(avg_prob_ratio, digits),
    paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is evidence that the model does not satisfy
            balance for positive class.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            balance for positive class.\n")
    }
  }

  return(result_df)
}

#' Examine Balance for Negative Class of a Model
#'
#' This function evaluates *Balance for the Negative Class*, a fairness criterion
#' that checks whether the model assigns similar predicted probabilities across groups
#' among individuals whose true outcome is negative (i.e., \(Y = 0\)).
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' sensitive attribute
#' @param outcome Name of the outcome variable
#' @param group Name of the sensitive attribute
#' @param probs Predicted probabilities
#' @param confint Logical indicating whether to calculate confidence intervals
#' @param bootstraps Number of bootstraps to use for confidence intervals
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Whether to print the results, default is TRUE
#' @return A list containing the following elements:
#' - Average predicted probability for Group 1
#' - Average predicted probability for Group 2
#' - Difference in average predicted probability
#' - Ratio in average predicted probability
#' If confidence intervals are computed (`confint = TRUE`):
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the difference in average predicted probability
#' - A vector of length 2 containing the lower and upper bounds of the 95%
#' confidence interval for the ratio in average predicted probability
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
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#'
#' # Evaluate Balance for Negative Class
#' eval_neg_class_bal(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred"
#' )
#' }
#' @seealso \code{\link{eval_neg_class_bal}}
#' @export

eval_neg_class_bal <- function(data, outcome, group, probs, confint = TRUE,
                               alpha = 0.05, bootstraps = 2500,
                               digits = 2, message = TRUE) {
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 && all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  neg_data <- data[data[[outcome]] == 0, ]
  avg_prob <- get_avg_prob(
    data = neg_data, group = group, probs = probs, digits = digits
  )

  avg_prob_diff <- avg_prob[[1]] - avg_prob[[2]]
  avg_prob_ratio <- avg_prob[[1]] / avg_prob[[2]]

  se <- replicate(bootstraps, {
    group1 <- sample(which(data[[group]] == unique(data[[group]])[1]),
      replace = TRUE
    )
    group2 <- sample(which(data[[group]] == unique(data[[group]])[2]),
      replace = TRUE
    )
    data_boot <- rbind(data[group1, ], data[group2, ])
    neg_data_boot <- data_boot[data_boot[[outcome]] == 0, ]
    avg_prob_boot <- get_avg_prob(
      data = neg_data_boot, group = group, probs = probs
    )
    return(c(
      avg_prob_boot[[1]] - avg_prob_boot[[2]],
      log(avg_prob_boot[[1]] / avg_prob_boot[[2]])
    ))
  })

  lower_ci <- round(avg_prob_diff - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
  upper_ci <- round(avg_prob_diff + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm = TRUE), digits)
  lower_ratio_ci <- round(exp(log(avg_prob_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)
  upper_ratio_ci <- round(exp(log(avg_prob_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm = TRUE)), digits)

  result_df <- data.frame(
    "Avg. Predicted Prob.",
    avg_prob[[1]],
    avg_prob[[2]],
    avg_prob_diff,
    paste0("[", lower_ci, ", ", upper_ci, "]"),
    round(avg_prob_ratio, digits),
    paste0("[", lower_ratio_ci, ", ", upper_ratio_ci, "]")
  )

  colnames(result_df) <- c(
    "Metric",
    paste0("Group", sort(unique(data[[group]]))[1]),
    paste0("Group", sort(unique(data[[group]]))[2]),
    "Difference",
    "95% Diff CI",
    "Ratio",
    "95% Ratio CI"
  )

  if (message) {
    if (lower_ci > 0 || upper_ci < 0) {
      cat("There is enough evidence that the model does not satisfy
            balance for negative class.\n")
    } else {
      cat("There is not enough evidence that the model does not satisfy
            balance for negative class.\n")
    }
  }

  return(result_df)
}
