# Assessing Model Fairness Across Binary Protected Attributes

## Introduction

This vingette demonstrates how to obtain, report and interpret model
fairness metrics for binary protected attributes with the `fairmetrics`
package. We illustrate this through a case study based on a preprocessed
version of the MIMIC-II clinical database \[1\], which has been
previously studied to explore the relationship between indwelling
arterial catheters in hemodynamically stable patients and respiratory
failure in relation to mortality outcomes \[2\]. The original,
unprocessed dataset is publicly available through
[PhysioNet](https://physionet.org/content/mimic2-iaccd/1.0/) \[3\]. A
preprocessed version of this dataset is included in the `fairmetrics`
package as the `mimic_preprocessed` and is used in this vingette.

## Data Split and Model Construction

In this setting, we construct a model that will predict 28-day mortality
(`day_28_flg`). To do this, we split the dataset into a training and
testing sets and fit a random forest model. The first 700 patients are
used as the training set and the remaining patients are used as the
testing set. After the model is fit, it is used to predict 28-day
mortality. The predicted probabilities are saved as new column in the
testing data and are used to assess model fairness.

``` r
# Load required libraries
library(dplyr)
library(fairmetrics)
library(randomForest)
# Set seed for reproducibility
set.seed(1)
# Use 700 labels to train on the mimic_preprocessed dataset
train_data <- mimic_preprocessed %>%
  filter(row_number() <= 700)

# Test the model on the remaining data
test_data <- mimic_preprocessed %>%
  filter(row_number() > 700)

# Fit a random forest model
rf_model <- randomForest(factor(day_28_flg) ~ ., data = train_data, ntree = 1000)
# Save model prediction
test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")[, 2]
```

## Fairness Evaluation

The `fairmetrics` package is used to assess model fairness across binary
protected attributes. This means that the number of unique values in the
protected attribute column need to be exactly two. To evaluate fairness
of the random forest model which we fit, we examine patient gender as
the binary protected attribute.

``` r
# Recode gender variable explicitly for readability: 
test_data <- test_data %>%
  mutate(gender = ifelse(gender_num == 1, "Male", "Female"))
```

Since many fairness metrics require binary predictions, we threshold the
predicted probabilities using a fixed cutoff. We set a threshold of 0.41
to maintain the overall false positive rate (FPR) at approximately 5%.
To evaluate specific fairness metrics, its possible to do so with the
`eval_*` functions (for a list of the functions contained in
`fairmetrics` see
[here](https://jianhuig.github.io/fairmetrics/reference/index.html)).
For example, if we are interested in calculating the statistical parity
of our model across gender (here assumed to be binary), we write:

``` r
eval_stats_parity(
  data = test_data, 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  message = TRUE
)
#> There is evidence that model does not satisfy statistical parity.
#>                     Metric GroupFemale GroupMale Difference  95% Diff CI Ratio
#> 1 Positive Prediction Rate        0.17      0.09       0.08 [0.04, 0.12]  1.89
#>   95% Ratio CI
#> 1 [1.35, 2.64]
```

The dataframe returned gives the positive prediction rate between the
groups defined by the binary protected attribute (`GroupFemale` and
`GroupMale` in this case), the difference and ratios between the groups
and the bootstrap calculated confidence intervals for the estimated
difference and ratios. For inference, it can be considered that
confidence interval which contains 0 in its range for difference or 1
for ratio as insignificant.

The above syntax can be extended to the other `eval_*` functions as
well. Additionally, if one wishes to calculate various fairness metrics
(and their confidence intervals) for the model simultaneously, we pass
our test data with its predicted results into the `get_fairness_metrics`
function.

``` r
get_fairness_metrics(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
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
#> 1         0.17      0.09       0.08   [0.04, 0.12]  1.89 [1.35, 2.64]
#> 2         0.38      0.57      -0.19 [-0.34, -0.04]  0.67 [0.47, 0.94]
#> 3         0.08      0.03       0.05   [0.02, 0.08]  2.67  [1.4, 5.08]
#> 4         0.46      0.37       0.09   [0.04, 0.14]  1.24 [1.09, 1.42]
#> 5         0.15      0.11       0.04   [0.02, 0.06]  1.36 [1.17, 1.59]
#> 6         0.62      0.68      -0.06  [-0.23, 0.11]  0.91 [0.71, 1.18]
#> 7         0.92      0.91       0.01  [-0.03, 0.05]  1.01 [0.97, 1.05]
#> 8         0.09      0.08       0.01  [-0.01, 0.03]  1.12 [0.88, 1.43]
#> 9         0.87      0.89      -0.02  [-0.06, 0.02]  0.98 [0.93, 1.02]
#> 10        1.03      2.78      -1.75    [-3.6, 0.1]  0.37 [0.17, 0.79]
```

With the `get_fairness_metrics` function, it’s possible to examine the
bootstrapped confidence intervals to determine which fairness criterion
show evidence of violation. From the above output, Statistical Parity,
Equal Opportunity, Predictive Equality, Balance for Positive Class and
Balance for Negative Class show evidence of being violated, while
Positive Predictive Parity, Negative Predictive Parity, Brier Score
Parity and Overall Accuracy Parity do not. Treatment Equality remains
ambiguous: when assessed by difference, the 95% bootstrapped confidence
interval crosses 0 - indicating that there is no evidence indicating
that the fairness criterion is violated, but when assessed by ratio, the
interval crosses 1 - which indicates that there is.

> **NOTE:**
>
> Statistical inference from bootstrapped confidence intervals should be
> interpreted with caution. A confidence interval crossing 0 (for
> differences) or 1 (for ratios) means the evidence is inconclusive
> rather than proving absence of unfairness. Apparent violations may
> reflect sampling variability rather than systematic bias. Always
> complement these results with domain knowledge, sensitivity analyses,
> and additional fairness diagnostics before drawing strong conclusions
> about a specific fairness assessment.

While `fairmetrics` focuses only on assessing fairness of models across
binary protected attributes, it is possible to work with protected
attributes which consist of more than two groups by using “one-vs-all”
comparisons and a little bit of data wrangling to create the appropriate
columns.

## Conditional Fairness Evaluation

To evaluate a model’s fairness for a specific subgroup, simply subset
the data. For example, to evaluate the conditional statistical parity
among subjects aged 60 and older using the previously fitted random
forest model, subsetting the test data and and passing it through the
`eval_stats_parity` is all that’s required.

``` r
eval_stats_parity(
  data = subset(test_data, age >= 60), 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  message = TRUE
)
#> There is not enough evidence that the model does not satisfy
#>             statistical parity.
#>                     Metric GroupFemale GroupMale Difference 95% Diff CI Ratio
#> 1 Positive Prediction Rate        0.34      0.25       0.09   [0, 0.18]  1.36
#>   95% Ratio CI
#> 1 [1.01, 1.84]
```

From the bootstrapped confidence intervals for the difference and the
ratio, we note that there is evidence that statistical parity is
violated based on the difference and ratio confidence intervals between
the male and female subjects aged 60 and over. A similar example can be
constructed with `get_fairness_metrics` for a more comprehensive view of
the conditional fairness of the model.

``` r
get_fairness_metrics(
  data = subset(test_data, age >= 60), 
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41
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
#>    GroupFemale GroupMale Difference   95% Diff CI Ratio 95% Ratio CI
#> 1         0.34      0.25       0.09  [0.01, 0.17]  1.36 [1.01, 1.83]
#> 2         0.30      0.46      -0.16    [-0.32, 0]  0.65 [0.41, 1.04]
#> 3         0.18      0.11       0.07 [-0.01, 0.15]  1.64 [0.91, 2.96]
#> 4         0.50      0.41       0.09  [0.04, 0.14]  1.22 [1.08, 1.38]
#> 5         0.26      0.22       0.04  [0.01, 0.07]  1.18 [1.03, 1.36]
#> 6         0.63      0.69      -0.06  [-0.22, 0.1]  0.91 [0.71, 1.17]
#> 7         0.86      0.81       0.05 [-0.03, 0.13]  1.06 [0.96, 1.17]
#> 8         0.14      0.16      -0.02 [-0.05, 0.01]  0.88 [0.71, 1.07]
#> 9         0.78      0.78       0.00 [-0.08, 0.08]  1.00  [0.91, 1.1]
#> 10        0.71      1.88      -1.17  [-2.54, 0.2]  0.38 [0.16, 0.89]
```

Conditioning on subjects aged 60 and older, Statistical Parity, Balance
for Positive Class and Balance for Negative Class posses evidence for
being violated based on 95% bootstrapped confidence intervals. While
Predictive Equality, Positive Predictive Parity, Negative Predictive
Parity, Brier Score Parity and Overall Accuracy Parity do not. Similar
to the unconditioned case, Treatment Equality remains ambiguous based on
the contrast between the difference and ratio confidence intervals. If
difference is considered, there is evidence of Treatment equality being
violated. If ratio is considered, there does not.

## Appendix: Confidence Interval Construction

The function
[`get_fairness_metrics()`](../reference/get_fairness_metrics.md)
computes Wald-type confidence intervals for both group-specific and
disparity metrics using nonparametric bootstrap. To illustrate the
construction of confidence intervals (CIs), we use the following example
involving the false positive rate ($FPR$).

Let ${\widehat{\text{FPR}}}_{a}$ and $\text{FPR}_{a}$ denote the
estimated and true FPR in group $A = a$. Then the difference
${\widehat{\Delta}}_{\text{FPR}} = {\widehat{\text{FPR}}}_{a_{1}} - {\widehat{\text{FPR}}}_{a_{0}}$
satisfies (e.g., [Gronsbell et al.,
2018](https://doi.org/10.1093/jrsssb/qkad107)):

$$\sqrt{n}\left( {\widehat{\Delta}}_{\text{FPR}} - \Delta_{\text{FPR}} \right)\overset{d}{\rightarrow}\mathcal{N}\left( 0,\sigma^{2} \right)$$

We estimate the standard error of ${\widehat{\Delta}}_{\text{FPR}}$
using bootstrap resampling within groups, and form a Wald-style
confidence interval:

$${\widehat{\Delta}}_{\text{FPR}} \pm z_{1 - \alpha/2} \cdot \widehat{\text{se}}\left( {\widehat{\Delta}}_{\text{FPR}} \right)$$

For **ratios**, such as
${\widehat{\rho}}_{\text{FPR}} = {\widehat{\text{FPR}}}_{a_{1}}/{\widehat{\text{FPR}}}_{a_{0}}$,
we apply a log transformation and use the delta method:

$$\log\left( {\widehat{\rho}}_{\text{FPR}} \right) \pm z_{1 - \alpha/2} \cdot \widehat{\text{se}}\left\lbrack \log\left( {\widehat{\rho}}_{\text{FPR}} \right) \right\rbrack$$

Exponentiation of the bounds yields a confidence interval for the ratio
on the original scale:

$$\left\lbrack \exp\left\{ \log\left( {\widehat{\rho}}_{\text{FPR}} \right) - z_{1 - \alpha/2} \cdot \widehat{\text{se}}\left\lbrack \log\left( {\widehat{\rho}}_{\text{FPR}} \right) \right\rbrack \right\},\ \exp\left\{ \log\left( {\widehat{\rho}}_{\text{FPR}} \right) + z_{1 - \alpha/2} \cdot \widehat{\text{se}}\left\lbrack \log\left( {\widehat{\rho}}_{\text{FPR}} \right) \right\rbrack \right\} \right\rbrack.$$

## References

1.  Raffa, J. (2016). Clinical data from the MIMIC-II database for a
    case study on indwelling arterial catheters (version 1.0).
    PhysioNet. <https://doi.org/10.13026/C2NC7F>.

2.  Raffa J.D., Ghassemi M., Naumann T., Feng M., Hsu D. (2016) Data
    Analysis. In: Secondary Analysis of Electronic Health Records.
    Springer, Cham

3.  Goldberger, A., Amaral, L., Glass, L., Hausdorff, J., Ivanov, P. C.,
    Mark, R., … & Stanley, H. E. (2000). PhysioBank, PhysioToolkit, and
    PhysioNet: Components of a new research resource for complex
    physiologic signals. Circulation \[Online\]. 101 (23),
    pp. e215–e220.

4.  Gao, J. et al. What is Fair? Defining Fairness in Machine Learning
    for Health. arXiv.org <https://arxiv.org/abs/2406.09307> (2024).

5.  Gronsbell, J. L. & Cai, T. Semi-Supervised approaches to efficient
    evaluation of model prediction performance. Journal of the Royal
    Statistical Society Series B (Statistical Methodology) 80, 579–594
    (2017).

6.  Hort, M., Chen, Z., Zhang, J. M., Harman, M. & Sarro, F. Bias
    Mitigation for Machine Learning Classifiers: A Comprehensive survey.
    arXiv.org <https://arxiv.org/abs/2207.07068> (2022).

7.  Hsu, D. J. et al. The association between indwelling arterial
    catheters and mortality in hemodynamically stable patients with
    respiratory failure. CHEST Journal 148, 1470–1476 (2015).

8.  Efron, B. & Tibshirani, R. Bootstrap methods for standard errors,
    confidence intervals, and other measures of statistical accuracy.
    Statistical Science 1, (1986).
