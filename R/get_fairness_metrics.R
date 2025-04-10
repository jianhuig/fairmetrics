#' Compute Fairness Metrics for Binary Classification
#'
#' This function evaluates various fairness metrics on a binary classification model
#' across different groups. The metrics are returned in a single data frame.
#'
#' @param data A data frame containing the outcome, group, and predicted probabilities.
#' @param outcome The name of the column containing the true binary outcome.
#' @param group The name of the column representing the sensitive attribute (e.g., race, gender).
#' @param probs The name of the column with predicted probabilities.
#' @param cutoff Numeric threshold for classification. Default is 0.5.
#' @param confint Logical; whether to compute bootstrap confidence intervals. Default is TRUE.
#' @param bootstraps Number of bootstrap samples. Default is 2500.
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param digits Number of digits to round the metrics to. Default is 2.
#'
#' @return A data frame with the evaluated fairness metrics.
#'
#' @examples
#' \donttest{
#' library(FairnessTutorial)
#' library(dplyr)
#' library(randomForest)
#' data("mimic_preprocessed")
#' set.seed(123)
#' train_data <- mimic_preprocessed |>
#'   dplyr::filter(dplyr::row_number() <= 700)
#' # Fit a random forest model
#' rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
#' # Test the model on the remaining data
#' test_data <- mimic_preprocessed |>
#'   dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female"))|>
#'   dplyr::filter(dplyr::row_number() > 700)
#'
#' test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
#'
#' # Fairness evaluation
#' # We will use sex as the sensitive attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Accuracy Parity
#' get_fairness_metrics(
#'   dat = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#'
#'
#' @export
get_fairness_metrics <- function(data, outcome, group, probs, cutoff = 0.5,
                                 bootstraps = 2500, alpha = 0.05, digits = 2) {

  acc_parity <- eval_acc_parity(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  bs_parity <- eval_bs_parity(
    data = data, outcome = outcome, group = group, probs = probs, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )


  eq_opp <- eval_eq_opp(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  neg_class_bal <- eval_neg_class_bal(
    data = data, outcome = outcome, group = group, probs = probs,
    alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  pos_class_bal <- eval_pos_class_bal(
    data = data, outcome = outcome, group = group, probs = probs, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  pred_equality <- eval_pred_equality(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, confint = confint, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  stats_parity <- eval_stats_parity(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  treatment_equality <- eval_pred_equality(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, alpha = alpha, bootstraps = bootstraps,
    digits = digits, message = FALSE
  )

  results <- list(
    acc_parity,
    bs_parity,
    eq_opp,
    neg_class_bal,
    pos_class_bal,
    pred_equality,
    stats_parity,
    treatment_equality
  )
  return(results)
}
