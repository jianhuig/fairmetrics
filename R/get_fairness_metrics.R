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
  # Check if outcome is binary
  unique_values <- unique(data[[outcome]])
  if (!(length(unique_values) == 2 &&
        all(unique_values %in% c(0, 1)))) {
    stop("Outcome must be binary (containing only 0 and 1).")
  }

  pos_data <- data[data[[outcome]] == 1, ]
  neg_data <- data[data[[outcome]] == 0, ]


  # Need to work on conditional stats parity. Skipping for now
  if (!(is.null(group2) & is.null(condition))) {
    if (!group2 %in% names(data)) {
      stop("`group2` not found in data.")
    }

    # check if the group2 is categorical or continuous
    if (is.numeric(data[[group2]])) {
      if (!grepl("^[<>=]", condition)) {
        stop(
          "Condition must be a character containing the sign of the condition
           and the value to threshold the continuous variable
           (e.g. '<50', '>50', '<=50', '>=50')."
        )
      } else {
        subset_data <- subset(data, eval(parse(
          text = paste0("data$", group2, condition)
        )))
      }
    } else {
      data[[group2]] <- as.factor(data[[group2]])
      if (!condition %in% levels(data[[group2]])) {
        stop("Condition must be a character of the levels to condition on.")
      } else {
        subset_data <- subset(data, data[[group2]] == condition)
      }
    }
    cond_ppr <- get_ppr(
      data = subset_data,
      outcome = outcome,
      group = group,
      probs = probs,
      cutoff = cutoff,
      digits = digits
    )

    cond_ppr_diff <- cond_ppr[[1]] - cond_ppr[[2]]
    cond_ppr_ratio <- log(cond_ppr[[1]] / cond_ppr[[2]])

  }

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




  if (!(is.null(group2) & is.null(condition))) {
    # Metrics
    metrics <- list(ppr,
                    cond_ppr,
                    fnr,
                    fpr,
                    avg_prob_pos,
                    avg_prob_neg,
                    ppv,
                    npv,
                    bs,
                    acc,
                    err_ratio)
  } else{
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
  }

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

      # Need to work on conditional stats parity. Skipping for now
      if (!(is.null(group2) & is.null(condition))) {
        subset_boot_data <- subset(boot_data, eval(parse(
          text = paste0("boot_data$", group2, condition)
        )))

        cond_ppr <- get_ppr(
          data = subset_boot_data,
          outcome = outcome,
          group = group,
          probs = probs,
          cutoff = cutoff,
          digits = digits
        )
      }

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

      if (!(is.null(group2) & is.null(condition))) {
        # Getting Diff and Ratio Vals
        diff_vals <- list(
          ppr_diff = ppr[[1]] - ppr[[2]],
          cond_ppr_diff = cond_ppr[[1]] - cond_ppr[[2]],
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
          cond_ppr_ratio = log(cond_ppr[[1]] / cond_ppr[[2]]),
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

      } else{
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
      }


      return(list(append(diff_vals, ratio_vals)))
    })

    se_matrix <- do.call(rbind, lapply(se, function (x)
      unlist(x)))

    if (!(is.null(group2) & is.null(condition))) {
      diff_sds <-  apply(se_matrix[, 1:11], 2, function (x)
        sd(x, na.rm = TRUE))
      ratio_sds <- apply(se_matrix[, 12:22], 2, function (x)
        sd(x, na.rm = TRUE))
    } else{
      diff_sds <-  apply(se_matrix[, 1:10], 2, function (x)
        sd(x, na.rm = TRUE))
      ratio_sds <- apply(se_matrix[, 11:20], 2, function (x)
        sd(x, na.rm = TRUE))
    }


    lower_cis <- round(diff_vals - qnorm(1 - alpha / 2) * diff_sds, digits)
    upper_cis <- round(diff_vals + qnorm(1 - alpha / 2) * diff_sds, digits)

    lower_ratio_cis <-  round(exp(log(ratio_vals) - qnorm(1 - alpha / 2) * ratio_sds), digits)
    upper_ratio_cis <-  round(exp(log(ratio_vals) + qnorm(1 - alpha / 2) * ratio_sds), digits)

    if (!(is.null(group2) & is.null(condition))) {
      model_summary <- data.frame(
        Metric = c(
          "Positive Prediction Rate",
          "Conditional Positive Prediction Rate",
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
        row.names = NULL
      )

      fairness_summary <- data.frame(
        Metric = c(
          "Statistical Parity",
          "Conditional Statistical Parity",
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

        Difference = diff_vals,
        Diff_CI = mapply(
          function (x, y)
            paste0("[", x, ", ", y, "]"),
          x = lower_cis,
          y = upper_cis
        ),
        Ratio = round(ratio_vals, digits),
        Ratio_CI = mapply(
          function (x, y)
            paste0("[", x, ", ", y, "]"),
          x = lower_ratio_cis,
          y = upper_ratio_cis
        ),
        row.names = NULL
      )


    } else{
      model_summary <- data.frame(
        Metric = c(
          "Positive Prediction Rate",
          "Conditional Positive Prediction Rate",
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
        row.names = NULL

      )

      fairness_summary <- data.frame(
        Metric = c(
          "Statistical Parity",
          "Conditional Statistical Parity",
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

        Difference = diff_vals,
        Ratio = round(ratio_vals, digits),
        row.names = NULL

      )

    }
  } else{
    if (!(is.null(group2) & is.null(condition))) {
      model_summary <- data.frame(
        Metric = c(
          "Positive Prediction Rate",
          "Conditional Positive Prediction Rate",
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
        row.names = NULL

      )

      fairness_summary <- data.frame(
        Metric = c(
          "Statistical Parity",
          "Conditional Statistical Parity",
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

        Difference = diff_vals,
        Ratio = round(ratio_vals, digits),
        row.names = NULL

      )


    } else{
      model_summary <- data.frame(
        Metric = c(
          "Positive Prediction Rate",
          "Conditional Positive Prediction Rate",
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
        row.names = NULL

      )

      fairness_summary <- data.frame(
        Metric = c(
          "Statistical Parity",
          "Conditional Statistical Parity",
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

        Difference = diff_vals,
        Ratio = round(ratio_vals, 2),
        row.names = NULL

      )

    }
  }
  return(list(performance = model_summary, fairness = fairness_summary))
}
