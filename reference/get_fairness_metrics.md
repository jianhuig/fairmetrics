# Compute Fairness Metrics for Binary Classification

Computes a comprehensive set of fairness metrics for binary
classification models, disaggregated by a binary protected attribute.
The function also computes corresponding performance metrics used in the
fairness calculations.

## Usage

``` r
get_fairness_metrics(
  data,
  outcome,
  group,
  probs,
  confint = TRUE,
  cutoff = 0.5,
  bootstraps = 2500,
  alpha = 0.05,
  digits = 2
)
```

## Arguments

- data:

  A data frame containing the outcome, group, and predicted
  probabilities.

- outcome:

  The name of the column containing the true binary outcome.

- group:

  The name of the column representing the binary protected attribute
  (e.g., race, gender).

- probs:

  The name of the column with predicted probabilities.

- confint:

  Logical indicating whether to calculate confidence intervals.

- cutoff:

  Numeric threshold for classification. Default is 0.5.

- bootstraps:

  Number of bootstrap samples. Default is 2500.

- alpha:

  Significance level for confidence intervals. Default is 0.05.

- digits:

  Number of digits to round the metrics to. Default is 2.

## Value

A dataframe containing fairness assessments, the performance metrics
they use and the evaluated results for each (binary) group (specified by
the `group` parameter) along with the difference and ratio between them.
If `confint` is set to `TRUE`, then the estimated `(1-alpha)*100%`
bootstrap confidence intervals are returned as well.

## Details

The results are returned as a list of two data frames:

- `performance`: Contains performance metrics (e.g., TPR, FPR, PPV) by
  group.

- `fairness`: Contains group-level fairness metrics (e.g., disparities
  or ratios), confidence intervals (if specified).

### Fairness Metrics Included:

- **Statistical Parity**: Difference in positive prediction rates across
  groups.

- **Equal Opportunity**: Difference in true positive rates (TPR) across
  groups.

- **Predictive Equality**: Difference in false positive rates (FPR)
  across groups.

- **Balance for Positive Class**: Checks whether the predicted
  probability distributions for positive outcomes are similar across
  groups.

- **Balance for Negative Class**: Same as above, but for negative
  outcomes.

- **Positive Predictive Parity**: Difference in positive predictive
  values (precision) across groups.

- **Negative Predictive Parity**: Difference in negative predictive
  values across groups.

- **Brier Score Parity**: Difference in Brier scores across groups.

- **Overall Accuracy Parity**: Difference in overall accuracy across
  groups.

- **Treatment Equality**: Ratio of false negatives to false positives
  across groups.

**NOTE:** Statistical inference from bootstrapped confidence intervals
should be interpreted with caution. A confidence interval crossing 0
(for differences) or 1 (for ratios) means the evidence is inconclusive
rather than proving absence of unfairness. Apparent violations may
reflect sampling variability rather than systematic bias. Always
complement these results with domain knowledge, sensitivity analyses,
and additional fairness diagnostics before drawing strong conclusions
about a specific fairness assessment.

## Examples

``` r
# \donttest{
library(fairmetrics)
library(dplyr)
library(randomForest)
library(magrittr)
data("mimic_preprocessed")
set.seed(123)
train_data <- mimic_preprocessed %>%
  dplyr::filter(dplyr::row_number() <= 700)
# Fit a random forest model
rf_model <- randomForest::randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female"))%>%
  dplyr::filter(dplyr::row_number() > 700)

test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

# Fairness evaluation
# We will use sex as the protected attribute and day_28_flg as the outcome.
# We choose threshold = 0.41 so that the overall FPR is around 5%.

# Get Fairness Metrics
get_fairness_metrics(
 data = test_data,
 outcome = "day_28_flg",
 group = "gender",
 probs = "pred",
 confint = TRUE,
 cutoff = 0.41,
 alpha = 0.05
)
#>           Fairness Assessment                                  Metric
#> 1          Statistical Parity                Positive Prediction Rate
#> 2           Equal Opportunity                     False Negative Rate
#> 3         Predictive Equality                     False Positive Rate
#> 4  Balance for Positive Class           Avg. Predicted Positive Prob.
#> 5  Balance for Negative Class           Avg. Predicted Negative Prob.
#> 6  Positive Predictive Parity               Positive Predictive Value
#> 7  Negative Predictive Parity               Negative Predictive Value
#> 8          Brier Score Parity                             Brier Score
#> 9     Overall Accuracy Parity                                Accuracy
#> 10         Treatment Equality (False Negative)/(False Positive) Ratio
#>    GroupFemale GroupMale Difference    95% Diff CI Ratio 95% Ratio CI
#> 1         0.17      0.08       0.09   [0.05, 0.13]  2.12 [1.48, 3.05]
#> 2         0.38      0.62      -0.24 [-0.39, -0.09]  0.61 [0.44, 0.86]
#> 3         0.08      0.03       0.05   [0.02, 0.08]  2.67 [1.38, 5.15]
#> 4         0.46      0.37       0.09   [0.04, 0.14]  1.24 [1.09, 1.42]
#> 5         0.15      0.10       0.05   [0.03, 0.07]  1.50 [1.29, 1.75]
#> 6         0.62      0.66      -0.04  [-0.21, 0.13]  0.94 [0.71, 1.24]
#> 7         0.92      0.90       0.02  [-0.02, 0.06]  1.02 [0.98, 1.07]
#> 8         0.09      0.08       0.01  [-0.01, 0.03]  1.12 [0.89, 1.43]
#> 9         0.87      0.88      -0.01  [-0.05, 0.03]  0.99 [0.94, 1.04]
#> 10        1.03      3.24      -2.21  [-4.44, 0.02]  0.32  [0.14, 0.7]
# }

```
