# Examine Balance for Negative Class of a Model

This function evaluates *Balance for the Negative Class*, a fairness
criterion that checks whether the model assigns similar predicted
probabilities among individuals whose true outcome is negative (i.e. \\Y
= 0\\) accross groups defined by a binary protected attribute.

## Usage

``` r
eval_neg_class_bal(
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
  protected attribute attribute

- outcome:

  Name of the outcome variable

- group:

  Name of the protected attribute. Must consist of only two groups.

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

- Average predicted probability for Group 1

- Average predicted probability for Group 2

- Difference in average predicted probability

- Ratio in average predicted probability If confidence intervals are
  computed (`confint = TRUE`):

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the difference in average predicted
  probability

- A vector of length 2 containing the lower and upper bounds of the 95%
  confidence interval for the ratio in average predicted probability

## See also

`eval_neg_class_bal`

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

# Evaluate Balance for Negative Class
eval_neg_class_bal(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred"
)
#> There is enough evidence that the model does not satisfy
#>             balance for negative class.
#>                 Metric GroupFemale GroupMale Difference  95% Diff CI Ratio
#> 1 Avg. Predicted Prob.        0.15       0.1       0.05 [0.03, 0.07]   1.5
#>   95% Ratio CI
#> 1 [1.29, 1.75]
# }
```
