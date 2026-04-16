# Examine Brier Score Parity of a Model

This function evaluates *Brier Score Parity*, a fairness measure that
checks whether the Brier score (a measure of the calibration of
probabilistic predictions) is similar across across two groups defined
by a binary protected attribute. Brier score parity ensures that the
model's predicted probabilities are equally well calibrated across
subpopulations.

## Usage

``` r
eval_bs_parity(
  data,
  outcome,
  group,
  probs,
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

  Name of the binary protected attribute. Must consist of only two
  groups.

- probs:

  Predicted probabilities

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

- Brier Score for Group 1

- Brier Score for Group 2

- Difference in Brier Score

- Ratio in Brier Score If confidence intervals are computed
  (`confint = TRUE`):

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the difference in Brier Score

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the ratio in Brier Score

## See also

[`eval_acc_parity`](eval_acc_parity.md),
[`eval_cond_acc_equality`](eval_cond_acc_equality.md),
[`eval_pos_pred_parity`](eval_pos_pred_parity.md),
[`eval_neg_pred_parity`](eval_neg_pred_parity.md)

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
# We will use sex as the protected attribute and day_28_flg as the outcome.

# Evaluate Brier Score Parity
eval_bs_parity(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred"
)
#> There is not enough evidence that the model does not satisfy
#>             Brier Score parity.
#>        Metric GroupFemale GroupMale Difference   95% Diff CI Ratio 95% Ratio CI
#> 1 Brier Score        0.09      0.08       0.01 [-0.01, 0.03]  1.12 [0.89, 1.43]
# }
```
