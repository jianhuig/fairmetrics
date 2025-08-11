---
title: 'fairmetrics: An R package for group fairness evaluation'
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
  - name: "Department of Statistical Science, University of Toronto"
    index: 1
date: "2025-06-10"
bibliography: paper.bib
output:
  # rticles::joss_article:
  #   keep_tex: true
  md_document:
     #keep_tex: true
     preserve_yaml: TRUE
     variant: "markdown_strict"
journal: JOSS
---

# Summary

Fairness is a growing area of machine learning (ML) that focuses on
ensuring that models do not produce systematically biased outcomes
across groups defined by protected attributes, such as race, gender, or
age. The `fairmetrics` R package provides a user-friendly framework for
rigorously evaluating group-based fairness criteria, including
independence (e.g., statistical parity), separation (e.g., equalized
odds), and sufficiency (e.g., predictive parity) for binary protected
attributes. The package provides both point and interval estimates for a
variety of commonly used criteria. `fairmetrics` also includes an
example dataset derived from the Medical Information Mart for Intensive
Care, version II (MIMIC-II) database (Goldberger et al. 2000; J. Raffa
2016) to demonstrate its use.

# Statement of Need

ML models are increasingly used in high-stakes domains such as criminal
justice, healthcare, finance, employment, and education (Mehrabi et al.
2021; Mattu 2016; Gao et al. 2024). Existing fairness evaluation
software report point estimates and/or visualizations, without any
measures of uncertainty. This limits users’ ability to determine whether
observed disparities are statistically significant. `fairmetrics`
addresses this limitation by including confidence intervals for both
difference and ratio based fairness metrics to enable more robust and
statistically grounded fairness assessments.

# Fairness Criteria

`fairmetrics` is designed to evaluate fairness of binary classification
models across binary protected attributes. The package supports the
evaluation of metrics belonging to three major group fairness criteria:

-   **Independence:** Statistical Parity (compares the overall rate of
    positive predictions between groups).

-   **Separation:** Equal Opportunity (compares false negative rates
    between groups), Predictive Equality (compares false positive rates
    between groups), Balance for Positive Class (compares the average
    predicted probabilities among individuals whose true outcome is
    positive across groups), and Balance for Negative Class (compares
    the average predicted probabilities among individuals whose true
    outcome is negative across groups).

-   **Sufficiency:** Positive Predictive Parity (compares the positive
    predictive values across groups), Negative Predictive Parity
    (compares the negative predictive values across groups).

The package also includes additional metrics, such as the Brier Score
Parity (compares the Brier score across groups), Accuracy Parity
(compares the overall accuracy across groups), and Treatment Equality
(compares the ratio of false negatives to false positives across
groups).

# Evaluating Fairness Criteria

The input required to evaluate model fairness with the `fairmetrics`
package is a `data.frame` or `tibble` containing the model’s predicted
probabilities, the true outcomes, and the protected attribute. shows the
workflow for using `fairmetrics`.

<figure>
<img src="fairmetrics-workflow.png"
alt="Workflow for using fairmetrics to evaluate model fairness across multiple criteria. " />
<figcaption aria-hidden="true">Workflow for using
<code>fairmetrics</code> to evaluate model fairness across multiple
criteria. </figcaption>
</figure>

A simple example of how to use the `fairmetrics` package is illustrated
below. The example makes use of the `mimic_preprocessed` dataset, a
pre-processed version of the Indwelling Arterial Catheter (IAC) Clinical
dataset, from the MIMIC-II clinical database (Goldberger et al. 2000; J.
Raffa 2016; J. D. Raffa et al. 2016).

While the choice of fairness metric used is context dependent, we show
all criteria available with the `get_fairness_metrics()` function for
illustrative purposes. In this example, we evaluate the model’s fairness
with respect to the binary protected attribute `gender`. The model is
trained on a subset of the data and the predictions are made and
evaluated on a test set. A statistically significant difference across
groups at a given level of significance is indicated when the confidence
interval for a difference-based metric does not include zero or when the
interval for a ratio-based metric does not include one.

    # Train a classification model (e.g., random forest).
    # Add the vector of predicted probabilities to the test data
    # to evaluate fairness.
    library(fairmetrics)
    # Setting alpha=0.05 for 95% confidence intervals
    get_fairness_metrics(
     data = test_data,
     outcome = "day_28_flg",
     group = "gender",
     probs = "pred",
     cutoff = 0.41, 
     alpha = 0.05
    )

    $performance
                                        Metric GroupFemale GroupMale
    1                 Positive Prediction Rate        0.17      0.08
    2                      False Negative Rate        0.38      0.62
    3                      False Positive Rate        0.08      0.03
    4            Avg. Predicted Positive Prob.        0.46      0.37
    5            Avg. Predicted Negative Prob.        0.15      0.10
    6                Positive Predictive Value        0.62      0.66
    7                Negative Predictive Value        0.92      0.90
    8                              Brier Score        0.09      0.08
    9                                Accuracy        0.87      0.88
    10 (False Negative)/(False Positive) Ratio        1.03      3.24

    $fairness
                               Metric Difference    95% Diff CI Ratio 95% Ratio CI
    1              Statistical Parity       0.09   [0.05, 0.13]  2.12 [1.48, 3.05]
    2               Equal Opportunity      -0.24 [-0.39, -0.09]  0.61 [0.44, 0.86]
    3             Predictive Equality       0.05   [0.02, 0.08]  2.67 [1.38, 5.15]
    4      Balance for Positive Class       0.09   [0.04, 0.14]  1.24 [1.09, 1.42]
    5      Balance for Negative Class       0.05   [0.03, 0.07]  1.50 [1.29, 1.75]
    6      Positive Predictive Parity      -0.04  [-0.21, 0.13]  0.94 [0.71, 1.24]
    7      Negative Predictive Parity       0.02  [-0.02, 0.06]  1.02 [0.98, 1.07]
    8              Brier Score Parity       0.01  [-0.01, 0.03]  1.12 [0.89, 1.43]
    9         Overall Accuracy Parity      -0.01  [-0.05, 0.03]  0.99 [0.94, 1.04]
    10             Treatment Equality      -2.21  [-4.44, 0.02]  0.32  [0.14, 0.7]

Users can also compute individual metrics using functions like
`eval_eq_opp()` to test specific fairness conditions. Full usage
examples are provided in the package documentation.

# Related Work

Other R packages similar to `fairmetrics` include `fairness` (Kozodoi
and V. Varga 2021), `fairmodels` (Wiśniewski and Biecek 2022) and
`mlr3fairness` (Pfisterer, Siyi, and Lang 2024). `fairmetrics` differs
from these packages in two ways. The first difference is that
`fairmetrics` calculates ratio and difference-based group fairness
metrics and their corresponding confidence intervals, allowing for more
meaningful inferences about the fairness criteria. The second difference
is that `fairmetrics` does not possess any external dependencies and has
a lower memory footprint. shows the comparison of memory used and
dependencies required when loading each library.

For Python users, the `fairlearn` library (Weerts et al. 2023) provides
additional fairness metrics and algorithms. The `fairmetrics` package is
designed for seamless integration with R workflows, making it a more
convenient choice for R users.

# Licensing and Availability

The `fairmetrics` package is under the MIT license. It is available on
CRAN and can be installed by using `install.packages("fairmetrics")`.
Full documentation and its examples are available at:
<https://jianhuig.github.io/fairmetrics/articles/fairmetrics.html>.
Source code and issue tracking are hosted on GitHub:
<https://github.com/jianhuig/fairmetrics/>.

# References

Gao, Jianhui, Benson Chou, Zachary R. McCaw, Hilary Thurston, Paul
Varghese, Chuan Hong, and Jessica Gronsbell. 2024. “What Is Fair?
Defining Fairness in Machine Learning for Health.” *arXiv.org*.
<https://doi.org/10.48550/arXiv.2406.09307>.

Goldberger, Ary L., Luis A. N. Amaral, Leon Glass, Jeffrey M. Hausdorff,
Plamen Ch. Ivanov, Roger G. Mark, Joseph E. Mietus, George B. Moody,
Chung-Kang Peng, and H. Eugene Stanley. 2000. “PhysioBank,
PhysioToolkit, and PhysioNet: Components of a New Research Resource for
Complex Physiologic Signals.” *Circulation \[Online\]* 101 (23):
e215–20. <https://doi.org/10.1161/01.CIR.101.23.e215>.

Kozodoi, Nikita, and Tibor V. Varga. 2021. *Fairness: Algorithmic
Fairness Metrics*. <https://doi.org/10.32614/cran.package.fairness>.

Mattu, Lauren Kirchner, Jeff Larson. 2016. “Machine Bias.” *ProPublica*.
https://www.propublica.org/article/machine-bias-risk-assessments-in-criminal-sentencing.

Mehrabi, Ninareh, Fred Morstatter, Nripsuta Saxena, Kristina Lerman, and
Aram Galstyan. 2021. “A Survey on Bias and Fairness in Machine
Learning.” *ACM Comput. Surv.* 54 (6).
<https://doi.org/10.1145/3457607>.

Pfisterer, Florian, Wei Siyi, and Michel Lang. 2024. *Mlr3fairness:
Fairness Auditing and Debiasing for ’Mlr3’*.
<https://doi.org/10.32614/cran.package.mlr3fairness>.

Raffa, Jesse. 2016. “Clinical Data from the MIMIC-II Database for a Case
Study on Indwelling Arterial Catheters (Version 1.0).”
<https://doi.org/10.13026/C2NC7F>. <https://doi.org/10.13026/C2NC7F>.

Raffa, Jesse D., Mohammad Ghassemi, Tristan Naumann, Mengling Feng, and
Daniel J. Hsu. 2016. “Data Analysis.” In *Secondary Analysis of
Electronic Health Records*, 109–22. Springer, Cham.
<https://doi.org/10.1007/978-3-319-43742-2_9>.

Weerts, Hilde, Miroslav Dudík, Richard Edgar, Adrin Jalali, Roman Lutz,
and Michael Madaio. 2023. “FairLearn: Assessing and Improving Fairness
of AI Systems.” *arXiv.org*.
<https://doi.org/10.48550/arXiv.2303.16626>.

Wiśniewski, Jakub, and Przemysław Biecek. 2022. “Fairmodels: A Flexible
Tool for Bias Detection, Visualization, and Mitigation in Binary
Classification Models.” *The R Journal* 14 (1): 227–43.
<https://doi.org/10.32614/RJ-2022-019>.
