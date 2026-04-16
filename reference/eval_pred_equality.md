# Examine Predictive Equality of a Model

This function evaluates predictive equality, a fairness metric that
compares the False Positive Rate (FPR) between groups defined by a
binary protected attribute. It assesses whether individuals from
different groups are equally likely to be incorrectly flagged as
positive when they are, in fact, negative.

## Usage

``` r
eval_pred_equality(
  data,
  outcome,
  group,
  probs,
  cutoff = 0.5,
  confint = TRUE,
  alpha = 0.05,
  bootstraps = 2500,
  digits = 2,
  message = TRUE
)
```

## Arguments

- data:

  Data frame containing the outcome, predicted outcome, and binary
  protected attribute

- outcome:

  Name of the outcome variable, it must be binary

- group:

  Name of the protected attribute. Must consist of only two groups.

- probs:

  Name of the predicted outcome variable

- cutoff:

  Threshold for the predicted outcome, default is 0.5

- confint:

  Whether to compute 95% confidence interval, default is TRUE

- alpha:

  The 1 - significance level for the confidence interval, default is
  0.05

- bootstraps:

  Number of bootstrap samples, default is 2500

- digits:

  Number of digits to round the results to, default is 2

- message:

  Logical; if TRUE (default), prints a textual summary of the fairness
  evaluation. Only works if `confint` is TRUE.

## Value

A list containing the following elements:

- FPR_Group1: False Positive Rate for the first group

- FPR_Group2: False Positive Rate for the second group

- FPR_Diff: Difference in False Positive Rate

- FPR_Ratio: Ratio in False Positive Rate If confidence intervals are
  computed (`confint = TRUE`):

- FPR_Diff_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the difference in False
  Positive Rate

- FPR_Ratio_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the ratio in False Positive
  Rate

## See also

[`eval_pos_pred_parity`](eval_pos_pred_parity.md),
[`eval_neg_pred_parity`](eval_neg_pred_parity.md),
[`eval_stats_parity`](eval_stats_parity.md)

## Examples

``` r
# \donttest{
library(fairmetrics)
library(dplyr)
library(magrittr)
library(randomForest)
data("mimic_preprocessed")
set.seed(123)
train_data <- mimic_preprocessed %>%
  dplyr::filter(dplyr::row_number() <= 700)
# Fit a random forest model
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) %>%
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Fairness evaluation
# We will use sex as the protectedR attribute and day_28_flg as the outcome.
# We choose threshold = 0.41 so that the overall FPR is around 5%.

# Evaluate Predictive Equality
eval_pred_equality(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is evidence that model does not satisfy predictive
#>             equality.
#>                Metric GroupFemale GroupMale Difference  95% Diff CI Ratio
#> 1 False Positive Rate        0.08      0.03       0.05 [0.02, 0.08]  2.67
#>   95% Ratio CI
#> 1 [1.38, 5.15]
# }
```
