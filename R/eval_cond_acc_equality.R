#' Examine Conditional Use Accuracy Equality of a Model
#'
#' This function evaluates *Conditional Use Accuracy Equality*, a fairness criterion that requires predictive performance to be similar across across two groups - defined by a binary protected attribute - when a model makes positive or negative predictions.
#'
#' @param data Data frame containing the outcome, predicted outcome, and
#' binary protected attribute
#' @param outcome Name of the outcome variable, it must be binary
#' @param group Name of the binary protected attribute. Must consist of only two groups.
#' @param probs Name of the predicted outcome variable
#' @param cutoff Threshold for the predicted outcome, default is 0.5
#' @param confint Whether to compute 95% confidence interval, default is TRUE
#' @param bootstraps Number of bootstrap samples, default is 2500
#' @param alpha The 1 - significance level for the confidence interval, default is 0.05
#' @param digits Number of digits to round the results to, default is 2
#' @param message Logical; if TRUE (default), prints a textual summary of the
#' fairness evaluation. Only works if `confint` is TRUE.
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
#' # We will use sex as the protected attribute and day_28_flg as the outcome.
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
  npv <- get_npv(
    data = data, outcome = outcome, group = group, probs = probs,
    cutoff = cutoff, digits = digits
  )
  ppv_diff <- ppv[[1]] - ppv[[2]]
  npv_diff <- npv[[1]] - npv[[2]]

  if(confint){
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

    ppv_lower_ci <- round(ppv_diff - qnorm(1 - alpha / 4) * sd(se[1, ], na.rm =TRUE), digits)
    ppv_upper_ci <- round(ppv_diff + qnorm(1 - alpha / 4) * sd(se[1, ], na.rm =TRUE), digits)
    npv_lower_ci <- round(npv_diff - qnorm(1 - alpha / 4) * sd(se[2, ], na.rm =TRUE), digits)
    npv_upper_ci <- round(npv_diff + qnorm(1 - alpha / 4) * sd(se[2, ], na.rm =TRUE), digits)

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
  }else{
    result_df <- data.frame(
      Metric = c("PPV; NPV"),
      Group1 = paste0(ppv[[1]], "; ", npv[[1]]),
      Group2 = paste0(ppv[[2]], "; ", npv[[2]]),
      Difference = paste0(ppv_diff, "; ", npv_diff),
    )

    colnames(result_df) <- c(
      "Metric",
      paste0("Group", sort(unique(data[[group]]))[1]),
      paste0("Group", sort(unique(data[[group]]))[2]),
      "Difference"
    )
  }

  return(result_df)
}
