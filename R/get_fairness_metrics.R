#' Compute Fairness Metrics for Binary Classification
#'
#' Computes a comprehensive set of fairness metrics for binary classification models, disaggregated by a binary protected attribute. The function also computes corresponding performance metrics used in the fairness calculations.
#'
#' The results are returned as a list of two data frames:
#' - `performance`: Contains performance metrics (e.g., TPR, FPR, PPV) by group.
#' - `fairness`: Contains group-level fairness metrics (e.g., disparities or ratios), confidence intervals (if specified).
#'
#' ### Fairness Metrics Included:
#' - **Statistical Parity**: Difference in positive prediction rates across groups.
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
#' **NOTE:** Statistical inference from bootstrapped confidence intervals should be interpreted with caution. A confidence interval crossing 0 (for differences) or 1 (for ratios) means the evidence is inconclusive rather than proving absence of unfairness. Apparent violations may reflect sampling variability rather than systematic bias. Always complement these results with domain knowledge, sensitivity analyses, and additional fairness diagnostics before drawing strong conclusions about a specific fairness assessment.
#'
#' @param data A data frame containing the outcome, group, and predicted probabilities.
#' @param outcome The name of the column containing the true binary outcome.
#' @param group The name of the column representing the binary protected attribute (e.g., race, gender).
#' @param probs The name of the column with predicted probabilities.
#' @param confint Logical indicating whether to calculate confidence intervals.
#' @param cutoff Numeric threshold for classification. Default is 0.5.
#' @param bootstraps Number of bootstrap samples. Default is 2500.
#' @param alpha Significance level for confidence intervals. Default is 0.05.
#' @param digits Number of digits to round the metrics to. Default is 2.
#'
#' @return A dataframe containing fairness assessments, the performance metrics they use and the evaluated results for each (binary) group (specified by the `group` parameter) along with the difference and ratio between them. If `confint` is  set to `TRUE`, then the estimated `(1-alpha)*100%` bootstrap confidence intervals are returned as well.
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
#' # We will use sex as the protected attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Get Fairness Metrics
#' get_fairness_metrics(
#'  data = test_data,
#'  outcome = "day_28_flg",
#'  group = "gender",
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
                                 probs,
                                 confint = TRUE,
                                 cutoff = 0.5,
                                 bootstraps = 2500,
                                 alpha = 0.05,
                                 digits = 2) {
  # Check if outcome and groups are binary
  unique_values <- unique(data[[outcome]])
  groups <- unique(data[[group]])
  if (!(length(unique_values) == 2 &&
        all(unique_values %in% c(0, 1)))) {
    stop("`outcome` must be binary (containing only 0 and 1).")
  }
  if (!(length(groups) == 2)) {
    stop(
      "`group` argument must only consist of two groups (i.e. `length(unique(data[[group]])) == 2`"
    )
  }


  pos_data <- data[data[[outcome]] == 1, ]
  neg_data <- data[data[[outcome]] == 0, ]

  # Stats Parity
  ppr <- get_ppr(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )

  # Equal Opportunity
  fnr <- 1 - get_tpr(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )

  # Pred Equality
  fpr <- get_fpr(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff
  )

  # Pos Class Bal
  avg_prob_pos <- get_avg_prob(
    data = pos_data,
    group = group,
    probs = probs,
    digits = digits
  )

  # Neg Class Bal
  avg_prob_neg <- get_avg_prob(
    data = neg_data,
    group = group,
    probs = probs,
    digits = digits
  )

  # Pos Pred Parity
  ppv <- get_ppv(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )

  # Neg Pred Parity
  npv <- get_npv(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )

  # Brier-Score Parity
  bs <- get_brier_score(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    digits = digits
  )

  # Accuracy Parity
  acc <- get_acc(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    digits = digits,
    cutoff = cutoff
  )

  # Treatment Equality

  err_ratio <- get_err_ratio(
    data = data,
    outcome = outcome,
    group = group,
    probs = probs,
    cutoff = cutoff,
    digits = digits
  )




    metrics <- list(ppr,
                    fnr,
                    fpr,
                    avg_prob_pos,
                    avg_prob_neg,
                    ppv,
                    npv,
                    bs,
                    acc,
                    err_ratio)


  diff_vals <- sapply(metrics, function(x)
    x[[1]] - x[[2]])
  ratio_vals <- sapply(metrics, function(x)
    x[[1]] / x[[2]])

  if (confint) {
    # Calculate confidence interval
    se <- replicate(bootstraps, {
      indices1 <- sample(which(data[[group]] == unique(data[[group]])[1]), replace = TRUE)
      indices2 <- sample(which(data[[group]] == unique(data[[group]])[2]), replace = TRUE)
      boot_data <- rbind(data[indices1, ], data[indices2, ])
      neg_data_boot <- boot_data[boot_data[[outcome]] == 0, ]
      pos_data_boot <- boot_data[boot_data[[outcome]] == 1, ]

      # Stats Parity
      ppr <- get_ppr(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff,
        digits = digits
      )

      # Equal Opportunity
      fnr <- 1 - get_tpr(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff,
        digits = digits
      )

      # Pred Equality
      fpr <- get_fpr(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff
      )

      # Pos Class Bal
      avg_prob_pos <- get_avg_prob(
        data = pos_data_boot,
        group = group,
        probs = probs,
        digits = digits
      )

      # Neg Class Bal
      avg_prob_neg <- get_avg_prob(
        data = neg_data_boot,
        group = group,
        probs = probs,
        digits = digits
      )

      # Pos Pred Parity
      ppv <- get_ppv(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff,
        digits = digits
      )

      # Neg Pred Parity
      npv <- get_npv(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff,
        digits = digits
      )

      # Brier-Score Parity
      bs <- get_brier_score(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        digits = digits
      )

      # Accuracy Parity
      acc <- get_acc(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        digits = digits,
        cutoff = cutoff
      )

      # Treatment Equality

      err_ratio <- get_err_ratio(
        data = boot_data,
        outcome = outcome,
        group = group,
        probs = probs,
        cutoff = cutoff,
        digits = digits
      )

      # Getting Diff and Ratio Vals
      diff_vals <- list(
        ppr_diff = ppr[[1]] - ppr[[2]],
        fnr_diff = fnr[[1]] - fnr[[2]],
        fpr_diff = fpr[[1]] - fpr[[2]],
        avg_prob_pos_diff = avg_prob_pos[[1]] - avg_prob_pos[[2]],
        avg_prob_diff = avg_prob_neg[[1]] - avg_prob_neg[[2]],
        ppv_diff = ppv[[1]] - ppv[[2]],
        npv_diff = npv[[1]] - npv[[2]],
        bs_diff = bs[[1]] - bs[[2]],
        acc_diff = acc[[1]] - acc[[2]],
        err_ratio_diff = err_ratio[[1]] - err_ratio[[2]]
      )

      ratio_vals <- list(
        ppr_ratio = log(ppr[[1]] / ppr[[2]]),
        fnr_ratio = log(fnr[1] / fnr[[2]]),
        fpr_ratio = log(fpr[[1]] / fpr[[2]]),
        avg_prob_pos_ratio = log(avg_prob_pos[[1]] / avg_prob_pos[[2]]),
        avg_prob_neg_ratio = log(avg_prob_neg[[1]] / avg_prob_neg[[2]]),
        ppv_ratio = log(ppv[[1]] / ppv[[2]]),
        npv_ratio = log(npv[[1]] / npv[[2]]),
        bs_ratio = log(bs[[1]] / bs[[2]]),
        acc_ratio = log(acc[[1]] / acc[[2]]),
        err_ratio_ratio = log(err_ratio[[1]] / err_ratio[[2]])
      )


      return(list(append(diff_vals, ratio_vals)))
    })

    se_matrix <- do.call(rbind, lapply(se, function (x)
      unlist(x)))


      diff_sds <-  apply(se_matrix[, 1:10], 2, function (x)
        sd(x, na.rm = TRUE))
      ratio_sds <- apply(se_matrix[, 11:20], 2, function (x)
        sd(x, na.rm = TRUE))


    lower_cis <- round(diff_vals - qnorm(1 - alpha / 2) * diff_sds, digits)
    upper_cis <- round(diff_vals + qnorm(1 - alpha / 2) * diff_sds, digits)

    lower_ratio_cis <-  round(exp(log(ratio_vals) - qnorm(1 - alpha / 2) * ratio_sds), digits)
    upper_ratio_cis <-  round(exp(log(ratio_vals) + qnorm(1 - alpha / 2) * ratio_sds), digits)


    fairness_summary <- data.frame(
      Fairness_Assesment = c(
        "Statistical Parity",
        "Equal Opportunity",
        "Predictive Equality",
        "Balance for Positive Class",
        "Balance for Negative Class",
        "Positive Predictive Parity",
        "Negative Predictive Parity",
        "Brier Score Parity",
        "Overall Accuracy Parity",
        "Treatment Equality"
      ),
      Metric = c(
        "Positive Prediction Rate",
        "False Negative Rate",
        "False Positive Rate",
        "Avg. Predicted Positive Prob.",
        "Avg. Predicted Negative Prob.",
        "Positive Predictive Value",
        "Negative Predictive Value",
        "Brier Score",
        "Accuracy",
        "(False Negative)/(False Positive) Ratio"
      ),
      Group1 = sapply(metrics, function(x)
        x[[1]]),
      Group2 = sapply(metrics, function(x)
        x[[2]]),
      Difference = diff_vals,
      Diff_CI  = paste0("[", lower_cis, ", ", upper_cis, "]"),
      Ratio = round(ratio_vals, digits),
      Ratio_CI = paste0("[", lower_ratio_cis, ", ", upper_ratio_cis, "]"),
      row.names = NULL

    )

    colnames(fairness_summary) <- c(
      "Fairness Assesment",
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[1]),
      paste0("Group", sort(unique(data[[group]]))[2]),
      "Difference",
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )

  } else{
    fairness_summary <- data.frame(
      Fairness_Assesment = c(
        "Statistical Parity",
        "Equal Opportunity",
        "Predictive Equality",
        "Balance for Positive Class",
        "Balance for Negative Class",
        "Positive Predictive Parity",
        "Negative Predictive Parity",
        "Brier Score Parity",
        "Overall Accuracy Parity",
        "Treatment Equality"
      ),
      Metric = c(
        "Positive Prediction Rate",
        "False Negative Rate",
        "False Positive Rate",
        "Avg. Predicted Positive Prob.",
        "Avg. Predicted Negative Prob.",
        "Positive Predictive Value",
        "Negative Predictive Value",
        "Brier Score",
        "Accuracy",
        "(False Negative)/(False Positive) Ratio"
      ),
      Group1 = sapply(metrics, function(x)
        x[[1]]),
      Group2 = sapply(metrics, function(x)
        x[[2]]),
      Difference = diff_vals,
      Ratio = round(ratio_vals, digits),
      row.names = NULL

    )

    colnames(fairness_summary) <- c(
      "Fairness Assesment",
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[1]),
      paste0("Group", sort(unique(data[[group]]))[2]),
      "Difference",
      "Ratio"
    )
  }
  return(fairness_summary)
}
