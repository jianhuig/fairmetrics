#' Examine Positive Predictive Parity of a Model
#'
#' This function evaluates *positive predictive predictive parity*, a key fairness criterion that
#' compares the *Positive Predictive Value (PPV)* between groups defined by a binary protected attribute.
#' In other words, it assesses whether, among individuals predicted to be positive,
#' the probability of being truly positive is equal across subgroups.
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
#' # We will use sex as the protected attribute and day_28_flg as the outcome.
#' # We choose threshold = 0.41 so that the overall FPR is around 5%.
#'
#' # Evaluate Positive Predictive Parity
#' eval_pos_pred_parity(
#'   data = test_data,
#'   outcome = "day_28_flg",
#'   group = "gender",
#'   probs = "pred",
#'   cutoff = 0.41
#' )
#' }
#' @seealso \code{\link{eval_neg_pred_parity}}
#' @export

eval_pos_pred_parity <- function(data, outcome, group, probs, cutoff = 0.5, confint = TRUE,
                                 bootstraps = 2500, alpha = 0.05,
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


  ppv <- get_ppv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_dif <- ppv[[1]] - ppv[[2]]
  ppv_ratio <- ppv[[1]] / ppv[[2]]

  if(confint){
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

    lower_ci <- round(ppv_dif - qnorm(1 - alpha / 2) * sd(se[1, ], na.rm=TRUE), digits)
    upper_ci <- round(ppv_dif + qnorm(1 - alpha / 2) * sd(se[1, ], na.rm=TRUE), digits)
    lower_ratio_ci <- round(exp(log(ppv_ratio) - qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)
    upper_ratio_ci <- round(exp(log(ppv_ratio) + qnorm(1 - alpha / 2) * sd(se[2, ], na.rm=TRUE)), digits)

    result_df <- data.frame(
      "Positive Predictive Value",
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
      paste0((1-alpha)*100, "% Diff CI"),
      "Ratio",
      paste0((1-alpha)*100, "% Ratio CI")
    )

    if (message) {
      if (lower_ci > 0 || upper_ci < 0) {
        cat("There is evidence that model does not satisfy positive predictive parity.\n")
      } else {
        cat("There is not enough evidence that the model does not satisfy positive predictive parity.\n")
      }
    }
  }else{
    result_df <- data.frame(
      "Positive Predictive Value",
      ppv[[1]],
      ppv[[2]],
      ppv_dif,
      round(ppv_ratio, digits)
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


