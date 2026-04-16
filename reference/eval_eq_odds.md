# Examine Equalized Odds of a Predictive Model

This function evaluates whether a predictive model satisfies the
Equalized Odds criterion by comparing both False Negative Rates (FNR)
and False Positive Rates (FPR) across two groups defined by a binary
protected attribute. It reports the rate for each group, their
differences, ratios, and bootstrap-based confidence regions. A
Bonferroni-corrected union test is used to test whether the model
violates the Equalized Odds criterion.

## Usage

``` r
eval_eq_odds(
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

  Name of the binary protected attribute. Must consist of only two
  groups.

- probs:

  A string specifying the name of the variable containing predicted
  probabilities or risk scores.

- cutoff:

  A numeric value used to threshold predicted probabilities into binary
  predictions; defaults to 0.5.

- confint:

  Whether to compute 95% confidence interval, default is TRUE.

- bootstraps:

  An integer specifying the number of bootstrap resamples for
  constructing confidence intervals; vdefaults to 2500.

- alpha:

  Significance level for the (1 - `alpha`) confidence interval; defaults
  to 0.05.

- digits:

  Number of decimal places to round numeric results; defaults to 2.

- message:

  Logical; if TRUE (default), prints a textual summary of the fairness
  evaluation. Only works if `confint` is TRUE.

## Value

A data frame summarizing group disparities in both FNR and FPR with the
following columns:

- `Metric`: The reported metrics ("FNR; FPR").

- `Group1`: Estimated FNR and FPR for the first group.

- `Group2`: Estimated FNR and FPR for the second group.

- `Difference`: Differences in FNR and FPR, computed as Group1 - Group2.

- `95% CR`: Bonferroni-adjusted confidence regions for the differences.

- `Ratio`: Ratios in FNR and FPR, computed as Group1 / Group2.

- `95% CR`: Bonferroni-adjusted confidence regions for the ratios.

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
  data = train_data, ntree = 1000
)
# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) %>%
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Fairness evaluation
# We will use sex as the protected attribute and day_28_flg as the outcome.
# We choose threshold = 0.41 so that the overall FPR is around 5%.

# Evaluate Equalized Odds
eval_eq_odds(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is evidence that model does not satisfy equalized odds.
#>     Metric Group Female Group Male  Difference                  95% Diff CI
#> 1 FNR; FPR   0.38; 0.08 0.62; 0.03 -0.24; 0.05 [-0.41, -0.07]; [0.01, 0.09]
#>        Ratio              95% Ratio CI
#> 1 0.61; 2.67 [0.42, 0.9]; [1.26, 5.66]
# }
```
