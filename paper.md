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

# Statement of Need

Machine learning (ML) offers significant potential for predictive modelling in biomedical research. In health-related contexts, predictive modelling is often used to  understand the roles of prognostic factors rather than simply to classify new cases [@Ueda_Kakinuma_Fujita_Kamagata_Fushimi_Ito_Matsui_Nozaki_Nakaura_Fujima_2023]. Despite its promise, there is substantial evidence that, without appropriate forethought and planning, ML models can introduce or exacerbate health inequities by making less accurate decisions for certain groups or individuals [@Yfantidou_Constantinides_Spathis_Vakali_Quercia_Kawsar_2023]. As ML becomes increasingly embedded in healthcare systems, ensuring equitable model performance across diverse populations is essential. 

The {FairnessEval} R package allows ML researchers and practitioners to evaluate the fairness of ML models through a suite of metrics and provides estimated confidence intervals of them. Designed to be lightweight with few dependencies and backwards compatablity, the {FairnessEval} package can diagnose the fairness of ML models through its suite of easy to use functions. 

# Fairness in Machine Learning - A Brief Overview

Fairness of a ML model can be assessed primarily through three criteron: group fairness, individual fairness, and causal fairness. Group fairness criteria are commonly used in health and deem a model as fair if its predictions are similarly accurate or calibrated across a predefined set of groups. The groups in question are most often defined by a protected attribute(s) such as age or race [@Fazelpour_Danks_2021]. Individual fairness criteria is a less commonly used and require that a model provides similar predictions to similar individuals based on a user-defined similarity metric [@Dwork_Hardt_Moritz_Pitassi_Toniann_Reingold_Omer_Zemel_Richard_2012; @Anderson_Visweswaran_2024]. Lastly, causal fairness criteria utilize causal estimands to quantify unfairness and link observed disparities in model performance to their underlying cause [@Plecko_Bareinboim_2022; @Makhlouf_Zhioua_Palamidessi_2024]. Causality-based fairness definitions are particularly attractive with health-focused applications as they enable practitioners to examine biases.The functions available in the {FairnessEval} focus on diagnosing ML models via group fairness critera as they are the most popular fairness criteron for health focused applications [@Awasthi_Cortes_Mansour_Mohri_2020; @Berk_Heidari_Jabbari_Kearns_Roth_2018]. 

Group fairness critera can be broadly classified into three categories: _independence_, _separation_, and _sufficiency_ [@The_MIT_Press_2024; @Berk_Heidari_Jabbari_Kearns_Roth_2018]. In the indepence category of group fairness criteria, metrics such as statistical (demographic) parity and its conditional variant requires that a model classifies individuals into the positive class at the same rate for each group in question [24; 83; 84; 85]. In the seperation category, a ML model’s decisions are required to not depend on the protected attribute within the positive and negative classifications. This implies that the rate of making a positive (or negative) decision is consistent across groups among individuals in a positive (or negative) class. Common separation-based metrics therefore aim to equalize error rates across the groups, including the false negative rate (FNR, known as equal opportunity), false positive rate (FPR, known as predictive equality), or both (known as equalized odds) [@Gao_Chou_McCaw_Thurston_Varghese_Hong_Gronsbell_2024]. In the sufficiency category, error rates of a ML models among individuals with similar decisions are diagnosed for possible inequaities accross a given protected attribute used as an input. Common sufficiency-based metrics focus on equalizing the positive predictive value (PPV, known as _predictive parity_), both the PPV and negative predictive value (NPV, known as _conditional use accuracy equality_), and calibration (known as _well-calibration_)[45; 81; 87; 88].

# Licensing and Availability

# Related Work



# References
