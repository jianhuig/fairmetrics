# Examine Statistical Parity of a Model

This function assesses *statistical parity* - also known as *demographic
parity* - in the predictions of a binary classifier across two groups
defined by a protected attribute. Statistical parity compares the rate
at which different groups receive a positive prediction, irrespective of
the true outcome. It reports the Positive Prediction Rate (PPR) for each
group, their differences, ratios, and bootstrap-based confidence
regions.

## Usage

``` r
eval_stats_parity(
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

  Data frame containing the outcome, predicted outcome, and protected
  attribute

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

- PPR_Group1: Positive Prediction Rate for the first group

- PPR_Group2: Positive Prediction Rate for the second group

- PPR_Diff: Difference in Positive Prediction Rate

- PPR_Ratio: The ratio in Positive Prediction Rate between the two
  groups. If confidence intervals are computed (`confint = TRUE`):

- PPR_Diff_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the difference in Positive
  Prediction Rate

- PPR_Ratio_CI: A vector of length 2 containing the lower and upper
  bounds of the 95% confidence interval for the ratio in Positive
  Prediction Rate

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

# Evaluate Statistical Parity
eval_stats_parity(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is evidence that model does not satisfy statistical parity.
#>                     Metric GroupFemale GroupMale Difference  95% Diff CI Ratio
#> 1 Positive Prediction Rate        0.17      0.08       0.09 [0.05, 0.13]  2.12
#>   95% Ratio CI
#> 1 [1.48, 3.05]
# }
```
