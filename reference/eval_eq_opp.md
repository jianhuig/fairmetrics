# Evaluate Equal Opportunity Compliance of a Predictive Model

This function evaluates the fairness of a predictive model with respect
to the Equal Opportunity criterion, which requires that the False
Negative Rate (FNR) be comparable across groups defined by a binary
protected attribute. The function quantifies disparities in FNR between
two groups and provides both the absolute difference and ratio, along
with confidence intervals obtained via bootstrapping.

## Usage

``` r
eval_eq_opp(
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

  A data frame containing the true binary outcomes, predicted
  probabilities, and binary protected attribute.

- outcome:

  A string specifying the name of the binary outcome variable in `data`.

- group:

  group Name of the binary protected attribute. Must consist of only two
  groups.

- probs:

  A string specifying the name of the variable containing predicted
  probabilities or risk scores.

- cutoff:

  A numeric value used to threshold predicted probabilities into binary
  decisions; defaults to 0.5.

- confint:

  Whether to compute 95% confidence interval, default is TRUE.

- bootstraps:

  An integer specifying the number of bootstrap resamples for
  constructing confidence intervals; defaults to 2500.

- alpha:

  Significance level for constructing the (1 - `alpha`) confidence
  interval; defaults to 0.05.

- digits:

  Integer indicating the number of decimal places to round results to;
  defaults to 2.

- message:

  Logical; if TRUE (default), prints a textual summary of the fairness
  evaluation. Only works if `confint` is TRUE.

## Value

A data frame summarizing FNR-based group disparity metrics with the
following columns:

- `Metric` A label indicating the reported fairness criterion.

- `Group1` Estimated FNR and FPR for the first group.

- `Group2` Estimated FNR and FPR for the second group.

- `Difference` The difference in FNR between the two groups, computed as
  the FNR of Group1 minus the FNR of Group2.

- `1-alpha% Diff CI` The (1 - `alpha`) confidence interval for the FNR
  difference.

- `Ratio` The ratio of FNRs between Group1 and Group2, computed as FNR
  for Group1 divided by FNR for Group2.

- `1-alpha% Ratio CI` The corresponding confidence interval for the FNR
  ratio.

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
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ .,
  data =
    train_data, ntree = 1000
)
# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) %>%
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Fairness evaluation
# We will use sex as the protected attribute and day_28_flg as the outcome.
# We choose threshold = 0.41 so that the overall FPR is around 5%.

# Evaluate Equal Opportunity Compliance
eval_eq_opp(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is evidence that model does not satisfy equal opportunity.
#>                Metric GroupFemale GroupMale Difference    95% Diff CI Ratio
#> 1 False Negative Rate        0.38      0.62      -0.24 [-0.39, -0.09]  0.61
#>   95% Ratio CI
#> 1 [0.44, 0.86]
# }
```
