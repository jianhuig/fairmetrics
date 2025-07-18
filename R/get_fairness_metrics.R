#' Compute Fairness Metrics for Binary Classification
#'
#' Computes a comprehensive set of fairness metrics for binary classification models, disaggregated by a sensitive attribute (e.g., race, gender). Optionally, conditional fairness can be evaluated using a second attribute and a specified condition. The function also computes corresponding performance metrics used in the fairness calculations.
#'
#' The results are returned as a list of two data frames:
#' - `performance`: Contains performance metrics (e.g., TPR, FPR, PPV) by group.
#' - `fairness`: Contains group-level fairness metrics (e.g., disparities or ratios), confidence intervals (if specified).
#'
#' ### Fairness Metrics Included:
#' - **Statistical Parity**: Difference in positive prediction rates across groups.
#' - **Conditional Statistical Parity** *(if group2 and condition are specified)*:
#'   Parity conditioned on a second group and value.
#' - **Equal Opportunity**: Difference in true positive rates (TPR) across groups.
#' - **Predictive Equality**: Difference in false positive rates (FPR) across groups.
#' - **Balance for Positive Class**: Checks whether the predicted probability distributions for
#'   positive outcomes are similar across groups.
#' - **Balance for Negative Class**: Same as above, but for negative outcomes.
#' - **Positive Predictive Parity**: Difference in positive predictive values (precision) across groups.
#' - **Negative Predictive Parity**: Difference in negative predictive values across groups.
#' - **Brier Score Parity**: Difference in Brier scores across groups.
#' - **Overall Accuracy Parity**: Difference in overall accuracy across groups.
#' - **Treatment Equality**: Ratio of false negatives to false positives across groups.
#'
#' @param data A data frame containing the outcome, group, and predicted probabilities.
#' @param outcome The name of the column containing the true binary outcome.
#' @param group The name of the column representing the sensitive attribute (e.g., race, gender).
#' @param group2 Define if conditional statistical parity is desired. Name of a secondary group variable used for conditional fairness analysis.
#' @param condition Define if conditional statistical parity is desired. If the conditional group is categorical, the condition
#' supplied must be a character of the levels to condition on. If the conditional
#' group is continuous, the conditions supplied must be a character containing
#' the sign of the condition and the value to threshold the continuous variable
#' (e.g. "<50", ">50", "<=50", ">=50").
#' @param probs The name of the column with predicted probabilities.
#' @param confint Logical indicating whether to calculate confidence intervals.
#' @param cutoff Numeric threshold for classification. Default is 0.5.
#' @param bootstraps Number of bootstrap samples. Default is 2500.
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param digits Number of digits to round the metrics to. Default is 2.
#'
#' @return A list containing:
#' \describe{
#'   \item{performance}{Data frame with performance metrics by group.}
#'   \item{fairness}{Data frame with computed fairness metrics and optional confidence intervals.}
#' }
#'
#' @examples
#' \donttest{
#' library(fairmetrics)
#' library(dplyr)
#' library(randomForest)
#' library(magrittr)
#' data("mimic_preprocessed")
#' set.seed(123)
#' train_data <- mimic_preprocessed %>%
#'   dplyr::filter(dplyr::row_number() <= 700)
#' # Fit a random forest model
#' rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
#' # Test the model on the remaining data
#' test_data <- mimic_preprocessed %>%
#'   dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female"))%>%
#'   dplyr::filter(dplyr::row_number() > 700)
#'
#' test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
#'
#' # Fairness evaluation
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Get Fairness Metrics
#' get_fairness_metrics(
#'  data = test_data,
#'  outcome = "day_28_flg",
#'  group = "gender",
#'  group2 = "age",
#'  condition = ">=60",
#'  probs = "pred",
#'  confint = TRUE,
#'  cutoff = 0.41,
#'  alpha = 0.05
#' )
#' }
#'
#'
#' @export
get_fairness_metrics <- function(data,
                                 outcome,
                                 group,
                                 group2 = NULL,
                                 condition = NULL,
                                 probs,
                                 confint = TRUE,
                                 cutoff = 0.5,
                                 bootstraps = 2500,
                                 alpha = 0.05,
                                 digits = 2) {
  stats_parity <- eval_stats_parity(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )

  if (!(is.null(group2) & is.null(condition))) {
    if (!group2 %in% names(data)) {
      stop("`group2` not found in data.")
    }
    cond_stats_parity <- eval_cond_stats_parity(
      data = data,
      outcome = outcome,
      group = group,
      group2 = group2,
      condition = condition,
      probs = probs,
      confint = confint,
      cutoff = cutoff,
      bootstraps = bootstraps,
      alpha = alpha,
      digits = digits,
      message = FALSE
    )
  }
  eq_opp <- eval_eq_opp(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )
  pred_equality <- eval_pred_equality(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )
  pos_class_bal <- eval_pos_class_bal(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )
  neg_class_bal <- eval_neg_class_bal(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )

  pos_pred_parity <- eval_pos_pred_parity(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )

  neg_pred_parity <- eval_neg_pred_parity(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )

  bs_parity <- eval_bs_parity(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )
  acc_parity <- eval_acc_parity(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )
  treatment_equality <- eval_treatment_equality(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    confint = confint,
    cutoff = cutoff,
    bootstraps = bootstraps,
    alpha = alpha,
    digits = digits,
    message = FALSE
  )

  # Model performance summary
  if (!(is.null(group2) & is.null(condition))) {
    summary <- rbind(stats_parity, cond_stats_parity,
                     eq_opp, pred_equality, pos_class_bal,
                     neg_class_bal, pos_pred_parity, neg_pred_parity, bs_parity,
                     acc_parity, treatment_equality)
    fairness_summary <- summary[, -c(1:3)]
    fairness_summary <- cbind( "Metric" = c("Statistical Parity",
                                            "Conditional Statistical Parity",
                                            "Equal Opportunity",
                                            "Predictive Equality",
                                            "Balance for Positive Class",
                                            "Balance for Negative Class",
                                            "Positive Predictive Parity",
                                            "Negative Predictive Parity",
                                            "Brier Score Parity",
                                            "Overall Accuracy Parity",
                                            "Treatment Equality"),
                               fairness_summary)
  } else{
    summary <- rbind(stats_parity, eq_opp, pred_equality, pos_class_bal,
                     neg_class_bal, pos_pred_parity, neg_pred_parity, bs_parity,
                     acc_parity, treatment_equality)
    fairness_summary <- summary[, -c(1:3)]
    fairness_summary <- cbind( "Metric" = c("Statistical Parity",
                                            "Equal Opportunity",
                                            "Predictive Equality",
                                            "Balance for Positive Class",
                                            "Balance for Negative Class",
                                            "Positive Predictive Parity",
                                            "Negative Predictive Parity",
                                            "Brier Score Parity",
                                            "Overall Accuracy Parity",
                                            "Treatment Equality"),
                               fairness_summary)
  }

  model_summary <- summary[,1:3]

  result <- list(
    performance = model_summary,
    fairness = fairness_summary
  )
  return(result)
}
