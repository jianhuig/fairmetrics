---
title: '{fairmetrics}: An R package for fairness evaluation metrics with confidence intervals'
tags:
  - R
  - Fairness
  - Machine Learning
  - Software
authors:
  - name: Benjamin Smith
    orcid: 0009-0007-2206-0177
    affiliation: 1
  - name: Jianhui Gao
    orcid: 0000-0003-0915-1473
    affiliation: 1
  - name: Benson Chou
    orcid: 0009-0007-0265-033X
    affiliation: 1
  - name: Jessica Gronsbell
    orcid: 0000-0002-5360-5869
    affiliation: 1
affiliations:
  - name: "University of Toronto"
    index: 1
date: "2025-03-08"
bibliography: paper.bib
output:
  rticles::joss_article
  # md_document:
  #   preserve_yaml: TRUE
  #   variant: "markdown_strict"
journal: JOSS
---

# Summary

{fairmetrics} is an R package designed to evaluate the fairness of machine learning models through a range of specialized metrics for which a model can be classified as "fair". It supports fairness assessments of popular group-based criteron, such as independence, separation, sufficency and others. The package enables statistical inference on fairness metrics through calculation of bootstrap confidence intervals (CIs). In addition, {fairmetrics} offers convenient wrapper functions to compute multiple metrics simultaneously and includes datasets derived from the MIMIC-II clinical database [@goldberger2000physiobank; @raffa2016clinical] for illustrating its use.
 

# Statement of Need

Machine learning (ML) offers significant potential for predictive modelling in biomedical research [@rajpurkarAIHealthMedicine2022]. Despite its promise, there is substantial evidence that, without appropriate forethought and planning, ML models can introduce or exacerbate health inequities by making less accurate decisions for certain groups or individuals [@grote2022enabling]. While existing software can compute fairness metrics, none provide out-of-the-box statistical inference, leaving practitioners without guidance on the uncertainty around those metrics. As ML becomes increasingly embedded in healthcare systems, ensuring equitable model performance across diverse populations is essential[@Gao_Chou_McCaw_Thurston_Varghese_Hong_Gronsbell_2024]. The {fairmetrics} R package fills this gap by offering a suite of popular group-fairness metrics along with bootstrap-based confidence intervals, enabling more rigorous and interpretable assessments of fairness in biomedical ML.

# Fairness Criteria

An ML model can be evaluated as being "fair" through three major criteron: group fairness, individual fairness and causal fairness. Group fairness deems a model fair if its predictions are similarly accurate or callibrated across a predefined set of groups, individual fairness insists that similar individuals should receive similar outcomes, and causal fairness leverages causal models that groups do not have an unjust influence on model predictions  [@Gao_Chou_McCaw_Thurston_Varghese_Hong_Gronsbell_2024]. {fairmetrics} focuses on calculating group fairness metrics as they are commonly used in biomedical settings. The groups in question are most often defined by protected attributes, such as age or race [@mehrabiSurveyBiasFairness2021].

Group fairness criteria are commonly categorized into three main types: independence, separation, and sufficiency [@barocas2023fairness; @Berk_Heidari_Jabbari_Kearns_Roth_2018; @Castelnovo_Crupi_Greco_Regoli_Penco_Cosentini_2022]. Independence requires that an ML model's predictions be statistically independent of the protected attribute. Separation demands that the model's predictions be independent of the protected attribute conditional on the true outcome class (i.e., within the positive and negative classes). Sufficiency requires that, given a model's prediction, the likelihood of the true outcome is independent of the protected attribute—aiming to equalize error rates across groups for similar prediction score. The {fairmetrics} package computes a range of group fairness metrics along with bootstrap-based confidence intervals. These metrics are grouped below according to the three core fairness frameworks described above.

## Independence

-   **Statistical Parity:** Compares the overall rate of positive predictions between groups, irrespective of the true outcome.

-   **Conditional Statistical Parity:** Restricts the comparison of positive prediction rates to a specific subgroup (e.g., within a hospital unit or age bracket), offering a more context-specific fairness assessment.


## Separation

-   **Equal Opportunity:** Focuses on disparities in false negative rates (FNR) between two groups, quantifying any difference in missed positive cases.

-   **Predictive Equality:** Compares false positive rates (FPR) between groups, ensuring that no group is disproportionately flagged as positive when the true outcome is negative.

-   **Positive Class Balance:** Checks whether, among individuals whose true outcome is positive, the distribution of predicted probabilities is comparable across groups.

-   **Negative Class Balance:** Checks whether, among individuals whose true outcome is negative, the distribution of predicted probabilities is comparable across groups.


## Sufficiency

-   **Predictive Parity:** Compares positive predictive values (PPV) across groups, assessing whether the precision of positive predictions is equivalent.

## Other Criteria

-   **Brier Score Parity:** Assesses whether the Brier score—the mean squared error of probabilistic predictions—is similar across groups, indicating comparable calibration.

-   **Accuracy Parity:** Measures whether the overall accuracy of a predictive model is equivalent across different groups.

-   **Treatment Equality:** Compares the ratio of false negatives to false positives across groups, ensuring the balance of missed detections versus false alarms is consistent.

# Evaluating Fairness Criteria

The {fairmetrics} package requires that a model has been trained and validated with data that has been split appropriately. The primary required input is a data frame or tibble which containing the model predictions, true outcomes, and protected attributes. \ref{workflow} shows the workflow for using {fairmetrics}. It is possible to evaluate a model for a specific or multiple group fairness metrics. 

![Workflow for using {fairmetrics} to evaluate model fairness across multiple criteria. \label{workflow}](fairmetrics-workflow.png)

More concretely, an example of how to use the {fairmetrics} package is shown below. The example uses the `mimic_preprocessed` dataset, which is a preprocessed version of the MIMIC-II database [@goldberger2000physiobank; @raffa2016clinical].

Consider a random forest model trained to predict the risk of heart failure in patients. Suppose we are interested in evaluating the model's fairness with respect to patient gender with predictive equality.

```r
library(fairmetrics)
library(dplyr)
library(magrittr)
library(randomForest)

# Load the example dataset
data("mimic_preprocessed")  

# Split the data into training and test sets
train_data <- mimic_preprocessed %>%
  dplyr::filter(dplyr::row_number() <= 700)

test_data <- mimic_preprocessed %>%
  dplyr::mutate(gender = ifelse(gender_num == 1, "Male", "Female")) %>%
  dplyr::filter(dplyr::row_number() > 700)

# Train a random forest model
rf_model <- randomForest::randomForest(
  factor(day_28_flg) ~ ., 
  data = train_data, 
  ntree = 1000
  )
  
# Make predictions on the test set
test_data$pred <- predict(rf_model, newdata = test_data, type = "prob")

# Evaluate predictive equality
# (Setting message=FALSE to avoid cluttering the output)

eval_pred_equality(
  data = test_data,
  outcome = "day_28_flg",
  group = "gender",
  probs = "pred",
  cutoff = 0.41,
  message = FALSE
)

#>   Metric GroupFemale GroupMale Difference  95% Diff CI Ratio 95% Ratio CI
#> 1    FPR        0.08      0.03       0.05 [0.02, 0.08]  2.67 [1.38, 5.15]


```
This case it can be seen from both the difference and ratio of the false positive rates (FPR) that the model is biased, with a 2.67 times higher chance of a false positive prediction for heart failure for females compared to males. The 95% bootstrap confidence intervals for both the difference and the ratio of the FPR are also shown, which can be used to further assess the statistical significance of the difference, confirming the initial evaluation of the FPR for female and male patients.

# Related Work

Other R packages similar to {fairmetrics} include {fairness}[@fairness_package] and {fairmodels}[@wisniewski2022fairmodels]. The differences between {fairmetrics} and these other packages is twofold. The primary difference between is that {fairmetrics} allows for the calculation of estimated confidence intervals of fairness metrics via bootstrap, which allows for more meaningful inferences about the fairness metrics calculated. Additionally, the {fairness} package has fewer dependencies and a lower memory footprint, making the for a more environment agnostic tool that can be used with modest hardware.

<!--
Jesse: Should I add the table which I genereated in README here?
--->

For python users, the {fairlearn} library [@Weerts_Dudík_Edgar_Jalali_Lutz_Madaio_2023] provides a broader set of fairness metrics and algorithms. The {fairmetrics} package is designed for seemless integration with R workflows, making it a more convenient choice for R-based ML applications.

# Licensing and Availability

The {fairmetrics} package is under the MIT license. It is available on CRAN and can be installed by using `install.packages("fairmetrics")`. A more in-depth tutorial can be accessed at: https://jianhuig.github.io/fairmetrics/articles/fairmetrics.html. All code is open-source and hosted on
GitHub. All bugs and inquiries can be reported at https://github.com/jianhuig/fairmetrics/issues/.


# References
