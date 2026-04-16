# Examine Treatment Equality of a Model

This function evaluates *Treatment Equality*, a fairness criterion that
assesses whether the ratio of false negatives to false positives is
similar across groups defined by a binary protected attribute. Treatment
Equality ensures that the model does not disproportionately favor or
disadvantage any group in terms of the relative frequency of missed
detections (false negatives) versus false alarms (false positives).

## Usage

``` r
eval_treatment_equality(
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

  Name of the outcome variable

- group:

  group Name of the binary protected attribute. Must consist of only two
  groups.

- probs:

  Predicted probabilities

- cutoff:

  Cutoff value for the predicted probabilities

- confint:

  Logical indicating whether to calculate confidence intervals

- alpha:

  The 1 - significance level for the confidence interval, default is
  0.05

- bootstraps:

  Number of bootstraps to use for confidence intervals

- digits:

  Number of digits to round the results to, default is 2

- message:

  Logical; if TRUE (default), prints a textual summary of the fairness
  evaluation. Only works if `confint` is TRUE.

## Value

A list containing the following elements:

- False Negative / False Positive ratio for Group 1

- False Negative / False Positive ratio for Group 2

- Difference in False Negative / False Positive ratio

- Ratio in False Negative / False Positive ratio If confidence intervals
  are computed (`confint = TRUE`):

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the difference in False Negative / False
  Positive ratio

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the ratio in False Negative / False Positive
  ratio

## See also

[`eval_acc_parity`](eval_acc_parity.md),
[`eval_bs_parity`](eval_bs_parity.md),
[`eval_pos_pred_parity`](eval_pos_pred_parity.md),
[`eval_neg_pred_parity`](eval_neg_pred_parity.md)

## Examples

``` r
# \donttest{
library(fairmetrics)
library(dplyr)
library(magrittr)
library(randomForest)
# Data for tests
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
# We will use sex as the protected attribute and day_28_flg as the outcome.

# Evaluate Treatment Equality
eval_treatment_equality(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  confint = TRUE,
  alpha = 0.05,
  bootstraps = 2500,
  digits = 2,
  message = FALSE
)
#>                                    Metric GroupFemale GroupMale Difference
#> 1 (False Negative)/(False Positive) Ratio        1.03      3.24      -2.21
#>     95% Diff CI Ratio 95% Ratio CI
#> 1 [-4.44, 0.02]  0.32  [0.14, 0.7]
# }
```
