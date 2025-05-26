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

## Rewritten 
Fairness is a growing area of machine learning (ML) that focuses on ensuring models do not produce systematically biased outcomes for certain groups, particularly those defined by protected attributes such as race, gender, or age. Evaluating fairness is a critical aspect of model development, as biased models can perpetuate or exacerbate existing social inequalities.  The {fairmetrics} R package offers a user-friendly framework for rigorously evaluating numerous group-based fairness criteria, including metrics based on independence (e.g., statistical parity), separation (e.g., equalized odds), and sufficiency (e.g., predictive parity). These criteria assess whether a model is equally accurate or well-calibrated across predefined groups so that appropriate bias mitigation strategies can be implemented. {fairmetrics} provides both point and interval estimates for multiple metrics through convenient wrapper functions, and includes example an dataset derived from the Medical Information Mart for Intensive Care, version II (MIMIC-II) database [@goldberger2000physiobank; @raffa2016clinical].


# Statement of Need

## Jianhui - this is too similar to the original paper still and I suggest we emphasize that the package isn't restricted to biomedical data.  We can of course use it as an example but this isn't the need for the package. Can you rewrite this to just be about the general use of fairness?  i.e., point 1 - ml increasing integrated into different areas, point 2 - give a few examples, point 3 - need for the package in light of other work.  I don't think interpetable is the correct word here, the key difference is allowing for uncertainty quantification, we also provide ratio + difference estimates, i dont think R fairness package does, I would emphasize   
# These are other R packages I am aware of so need to cite + compare: https://mlr3fairness.mlr-org.com, https://cran.r-project.org/web/packages/fairness/vignettes/fairness.html
Machine learning (ML) offers significant potential for predictive modelling in biomedical research [@rajpurkarAIHealthMedicine2022]. Despite its promise, there is substantial evidence that, without appropriate forethought and planning, ML models can introduce or exacerbate health inequities by making less accurate decisions for certain groups or individuals [@grote2022enabling]. While existing software can compute fairness metrics, none provide out-of-the-box statistical inference, leaving practitioners without guidance on the uncertainty around those metrics. As ML becomes increasingly embedded in healthcare systems, ensuring equitable model performance across diverse populations is essential[@Gao_Chou_McCaw_Thurston_Varghese_Hong_Gronsbell_2024]. The {fairmetrics} R package fills this gap by offering a suite of popular group-fairness metrics along with bootstrap-based confidence intervals, enabling more rigorous and interpretable assessments of fairness in biomedical ML.

# Fairness Criteria

## Both - Deleted first paragaph as it is not relevant to your package. Also restructured. 
## This section needs to be cleaned up for clarity - see comments below.
Group fairness criteria are typically classified into three main categories: independence, separation, and sufficiency [@barocas2023fairness; @Berk_Heidari_Jabbari_Kearns_Roth_2018; @Castelnovo_Crupi_Greco_Regoli_Penco_Cosentini_2022]. The {fairmetrics} package computes a range of group fairness metrics together with bootstrap-based confidence intervals for uncertainty quantification.  The metrics implemented in the package are briefly described below.

## Jianhui - The scope of the package is not defined in the above paragprah so one would not understand the definitons below (i.e., "positive classification" is never defined).  Specifically, you need to say you consider binary classification and a binary protected attribute - I do not believe your package handles more than.  Explain why this is done and then update the definitions below to reflect this. 

## Jianhui - why do you use bootstrap for this package rather than IF?  Is it because you don't have IF for all metrics?

## Jianhui - please update the initial sentences describing the 3 categories to be more intuitive, akin to what we have in the paper.  

## Independence
Independence requires that an ML model's predictions be statistically independent of the protected attribute. 

-   **Statistical Parity:** Compares the overall rate of positive predictions between groups, irrespective of the true outcome.

-   **Conditional Statistical Parity:** Restricts the comparison of positive prediction rates to a specific subgroup (e.g., within a hospital unit or age bracket), offering a more context-specific fairness assessment.

## Use consistent language throughout - alternating between checks/compares/focuses/assess/measure/etc is confusing as all functions do the same thing.  Suggest use "Compares" for all. 

## Separation
Separation demands that the model's predictions be independent of the protected attribute conditional on the true outcome class (i.e., within the positive and negative classes). 

-   **Equal Opportunity:** Focuses on disparities in false negative rates (FNR) between two groups, quantifying any difference in missed positive cases.

## Do you need the acronyms for FNR and FPR? If you don't use them later, they don't need to be defined. 

-   **Predictive Equality:** Compares false positive rates (FPR) between groups, ensuring that no group is disproportionately flagged as positive when the true outcome is negative.

-   **Positive Class Balance:** Checks whether, among individuals whose true outcome is positive, the distribution of predicted probabilities is comparable across groups.

-   **Negative Class Balance:** Checks whether, among individuals whose true outcome is negative, the distribution of predicted probabilities is comparable across groups.


## Sufficiency
Sufficiency requires that, given a model's prediction, the likelihood of the true outcome is independent of the protected attribute—aiming to equalize error rates across groups for similar prediction score.

-   **Predictive Parity:** Compares positive predictive values (PPV) across groups, assessing whether the precision of positive predictions is equivalent.

## Other Criteria

-   **Brier Score Parity:** Assesses whether the Brier score—the mean squared error of probabilistic predictions—is similar across groups, indicating comparable calibration.

-   **Accuracy Parity:** Measures whether the overall accuracy of a predictive model is equivalent across different groups.

-   **Treatment Equality:** Compares the ratio of false negatives to false positives across groups, ensuring the balance of missed detections versus false alarms is consistent.

# Evaluating Fairness Criteria

## Rewritten
## Ben - The package doesn't require data splitting, some people may want to use the package and incorporate CV within training.  Also, sometimes people evaluate fairness metrics on validation data. So suggest you simplify text + figure. Just start with the input.  Also, it doesn't seem like multiple should be a separate node?  doesnt it just include all the 4 boxes (sep, suff, ind, other)? can this be depicted? Also suggest you make the names more descriptive "Separation-based Metrics", "Independence-based Metrics", etc.

The input to the {fairmetrics} package is a data frame or tibble which containing the model's predictions, true outcomes, and the protected attributes (attribute? doesn't package only handle 1?). \hyperref[workflow]{Figure ~\ref*{workflow}}
 shows the workflow for using {fairmetrics}. It is possible to evaluate a model for a specific or multiple group fairness metrics. 

![Workflow for using {fairmetrics} to evaluate model fairness across multiple criteria. \label{workflow}](fairmetrics-workflow.png)
## Edited 
A simple example of how to use the {fairmetrics} package is shown below. The example uses the `mimic_preprocessed` dataset, which is a pre-processed version of the MIMIC-II database [@goldberger2000physiobank; @raffa2016clinical]. 

## Ben - need to add detail on what the detail is in the above paragraph. What does the data contaon, where did it come from, what is the sample size, etc.   

## Ben - suggest you show more of the functions in the package - say that, while the choice of metric is context dependent, we show all metrics to show the full range of the package. Also include interpretation of the various results.  Simple way to do this is show, the multiple output then show an example of one of the separation-based criterion or other to show users they can specify what they want.  



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

## Both - you are missing this one: https://mlr3fairness.mlr-org.com
## Do these metrics give difference and ratio based criteria? Are there any other differences? 
Other R packages similar to {fairmetrics} include {fairness}[@fairness_package] and {fairmodels} [@wisniewski2022fairmodels]. The differences between {fairmetrics} and these other packages is twofold. The primary difference between is that {fairmetrics} allows for the calculation of estimated confidence intervals of fairness metrics via bootstrap, which allows for more meaningful inferences about the fairness metrics calculated. Additionally, in contrast to the {fairmodels} and {fairness} packages, the {fairmetrics} package has zero library dependencies and a lower memory footprint, resulting in an environment agnostic tool that can be used with modest hardware and older systems. \hyperref[tab:memory_dep_usage]{Table~\ref*{tab:memory_dep_usage}} shows the comparison of memory used and dependencies required when loading each library. 

\begin{table}[ht]
\centering
\begin{tabular}{l r r}
\hline
\textbf{Package} & \textbf{Memory (MB)} & \textbf{Dependencies} \\
\hline
fairmodels  & 17.02  & 29\\
fairness    & 117.61 & 141\\
fairmetrics & 0.05   & 0\\
\hline
\end{tabular}
\caption{Memory usage and dependencies of {fairmetrics} vs similar packages (MB)}
\label{tab:memory_dep_usage}
\end{table}

For python users, the {fairlearn} library [@fairlearn_paper] provides a broader set of fairness metrics and algorithms. The {fairmetrics} package is designed for seemless integration with R workflows, making it a more convenient choice for R-based ML applications.

# Licensing and Availability

The {fairmetrics} package is under the MIT license. It is available on CRAN and can be installed by using `install.packages("fairmetrics")`. A more in-depth tutorial can be accessed at: https://jianhuig.github.io/fairmetrics/articles/fairmetrics.html. All code is open-source and hosted on
GitHub. All bugs and inquiries can be reported at https://github.com/jianhuig/fairmetrics/issues/.


# References
