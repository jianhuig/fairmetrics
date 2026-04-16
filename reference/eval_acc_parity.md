# Examine Accuracy Parity of a Model

This function assesses *Accuracy Parity*, a fairness criterion that
evaluates whether the overall accuracy of a predictive model is
consistent across two groups defined by a binary protected attribute.

## Usage

``` r
eval_acc_parity(
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

  Name of the binary protected attribute. Must consist of only two
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

- Accuracy for Group 1

- Accuracy for Group 2

- Difference in accuracy

- Ratio in accuracy If confidence intervals are computed
  (`confint = TRUE`):

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the difference in accuracy

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the ratio in accurac

## See also

[`eval_cond_acc_equality`](eval_cond_acc_equality.md)

## Examples

``` r
# \donttest{
library(fairmetrics)
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(magrittr)
library(randomForest)
#> randomForest 4.7-1.2
#> Type rfNews() to see new features/changes/bug fixes.
#> 
#> Attaching package: ‘randomForest’
#> The following object is masked from ‘package:dplyr’:
#> 
#>     combine
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

# Evaluate Accuracy Parity
eval_acc_parity(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
)
#> There is not enough evidence that the model does not satisfy
#>             accuracy parity.
#>     Metric GroupFemale GroupMale Difference   95% Diff CI Ratio 95% Ratio CI
#> 1 Accuracy        0.87      0.88      -0.01 [-0.05, 0.03]  0.99 [0.94, 1.04]
# }
```
