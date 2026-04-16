# Examine Positive Predictive Parity of a Model

This function evaluates *positive predictive predictive parity*, a key
fairness criterion that compares the *Positive Predictive Value (PPV)*
between groups defined by a binary protected attribute. In other words,
it assesses whether, among individuals predicted to be positive, the
probability of being truly positive is equal across subgroups.

## Usage

``` r
eval_pos_pred_parity(
  data,
  outcome,
  group,
  probs,
  cutoff = 0.5,
  confint = TRUE,
  bootstraps = 2500,
  alpha = 0.05,
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

- bootstraps:

  Number of bootstrap samples, default is 2500

- alpha:

  The 1 - significance level for the confidence interval, default is
  0.05

- digits:

  Number of digits to round the results to, default is 2

- message:

  Logical; if TRUE (default), prints a textual summary of the fairness
  evaluation. Only works if `confint` is TRUE.

## Value

A list containing the following elements:

- PPV_Group1: Positive Predictive Value for the first group

- PPV_Group2: Positive Predictive Value for the second group

- PPV_Diff: Difference in Positive Predictive Value

- PPV_Ratio: Ratio in Positive Predictive Value If confidence intervals
  are computed (`confint = TRUE`):

- PPV_Diff_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the difference in Positive
  Predictive Value

- PPV_Ratio_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the ratio in Positive
  Predictive Value

## See also

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
# We choose threshold = 0.41 so that the overall FPR is around 5%.

# Evaluate Positive Predictive Parity
eval_pos_pred_parity(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is not enough evidence that the model does not satisfy positive predictive parity.
#>                      Metric GroupFemale GroupMale Difference   95% Diff CI
#> 1 Positive Predictive Value        0.62      0.66      -0.04 [-0.21, 0.13]
#>   Ratio 95% Ratio CI
#> 1  0.94 [0.72, 1.23]
# }
```
