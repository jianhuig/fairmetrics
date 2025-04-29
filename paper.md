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

# Summary

Machine learning (ML) offers significant potential for predictive modelling in biomedical research. In health-related contexts, predictive modelling is often used to  understand the roles of prognostic factors rather than simply to classify new cases [@Ueda_Kakinuma_Fujita_Kamagata_Fushimi_Ito_Matsui_Nozaki_Nakaura_Fujima_2023]. Despite its promise, there is substantial evidence that, without appropriate forethought and planning, ML models can introduce or exacerbate health inequities by making less accurate decisions for certain groups or individuals [@Yfantidou_Constantinides_Spathis_Vakali_Quercia_Kawsar_2023]. As ML becomes increasingly embedded in healthcare systems, ensuring equitable model performance across diverse populations is essential. 

The {FairnessEval} R package allows ML researchers and practitioners to evaluate the fairness of ML models through a suite of metrics and provides estimated confidence intervals of tem. Designed to be lightweight with few dependencies and backwards compatablity, the {FairnessEval} package can diagnose the fairness of ML models through its suite of easy to use functions. 

# Background 

Existing definitions of fairness primarily fall into three categories: group fairness, individual fairness, and causal fairness. Group fairness criteria are commonly used in health anddeem a model as fair if its predictions are similarly accurate or calibrated across a predefined set of groups. These groups are most often defined by a protected attribute(s) such as age or race [34]. Individual fairness is a less commonly used framework and requires that the model provide similar predictions to similar individuals based on a user-defined similarity metric [24, 31]. Lastly, causal fairness criteria utilize causal estimands to quantify unfairness and link observed disparities in model performance to their underlying cause [73, 74]. Causality-based fairness definitions are particularly attractive for health-focused applications as they enable practitioners to interrogate biases

# Related Work

# References
