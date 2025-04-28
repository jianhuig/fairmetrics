---
title: 'FairnessEval: Fairness evaluation metrics with confidence intervals'
tags:
  - R
  - Fairness
  - Machine Learning
  - Software
authors:
  - name: Jianhui Gao
    orcid: 0000-0003-0915-1473
    affiliation: 1
  - name: Benjamin Smith
    orcid: 0009-0007-2206-0177
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
  # rticles::joss_article
  md_document:
    preserve_yaml: TRUE
    variant: "markdown_strict"
journal: JOSS
---

# Background

Machine learning (ML) offers significant potential for predictive modelling in biomedical research. In health-related contexts, predictive modelling is often used to  understand the roles of prognostic factors rather than simply to classify new cases [1]. Despite its promise, there is substantial evidence that, without appropriate forethought and planning, ML models can introduce or exacerbate health inequities by making less accurate decisions for certain groups or individuals [2]. An example of such inequities can be seen in, in medical imaging where state-of-the-art ML models used for disease diagnosis, risk prediction, and triage management are known to underperform within minority groups defined by protected attributes, such as sex, race, and ethnicity [3–13]. As ML becomes increasingly embedded in healthcare systems, ensuring equitable model performance across diverse populations is essential. 

Broadly, an ML model is said to be fair if it does not discriminate against an individual or group [17]. To facilitate the assessment of such disparities, we developed {FairnessEval}. {FairnessEval} is a lightweight R package designed to evaluate fairness metrics in ML and statistical models which incorporate protected attributes. Released under the MIT open-source license and available on CRAN, {FairnessEval} is accessible for both academic and commercial use within the R ecosystem.

# Summary 

Existing definitions of fairness primarily fall into three categories: group fairness, individual fairness, and causal fairness. Group fairness criteria are commonly used in health anddeem a model as fair if its predictions are similarly accurate or calibrated across a predefined set of groups. These groups are most often defined by a protected attribute(s) such as age or race [34]. Individual fairness is a less commonly used framework and requires that the model provide similar predictions to similar individuals based on a user-defined similarity metric [24, 31]. Lastly, causal fairness criteria utilize causal estimands to quantify unfairness and link observed disparities in model performance to their underlying cause [73, 74]. Causality-based fairness definitions are particularly attractive for health-focused applications as they enable practitioners to interrogate biases

# Related Works

# References
