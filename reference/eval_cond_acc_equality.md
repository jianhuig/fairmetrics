# Examine Conditional Use Accuracy Equality of a Model

This function evaluates *Conditional Use Accuracy Equality*, a fairness
criterion that requires predictive performance to be similar across
across two groups - defined by a binary protected attribute - when a
model makes positive or negative predictions.

## Usage

``` r
eval_cond_acc_equality(
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

  Name of the binary protected attribute. Must consist of only two
  groups.

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

- PPV_Group1: Positive Predictive Value for the first group

- PPV_Group2: Positive Predictive Value for the second group

- PPV_Diff: Difference in Positive Predictive Value

- NPV_Group1: Negative Predictive Value for the first group

- NPV_Group2: Negative Predictive Value for the second group

- NPV_Diff: Difference in Negative Predictive Value If confidence
  intervals are computed (`confint = TRUE`):

- PPV_Diff_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the difference in Positive
  Predictive Value

- NPV_Diff_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the difference in Negative
  Predictive Value

## See also

[`eval_acc_parity`](eval_acc_parity.md)

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

# Evaluate Conditional Use Accuracy Equality
eval_cond_acc_equality(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is not enough evidence that the model does not satisfy
#>             conditional use accuracy equality.
#>     Metric GroupFemale GroupMale  Difference                       95% CI
#> 1 PPV; NPV  0.62; 0.92 0.66; 0.9 -0.04; 0.02 [-0.24, 0.16]; [-0.02, 0.06]
# }
```
