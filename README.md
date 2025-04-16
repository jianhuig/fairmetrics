# FairnessEval: Fairness evaluation metrics with confidence intervals  <a href='https://github.com/jianhuig/FairnessTutorial'><img src='utils/png/hex_sticker.png' align="right" height="300" /></a>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/FairnessEval)](https://www.r-pkg.org/badges/version/FairnessEval)
[![License](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![Downloads](https://cranlogs.r-pkg.org/badges/FairnessEval)](https://cran.rstudio.com/web/packages/FairnessEval/index.html)

A collection of functions for computing fairness metrics for machine learning and statistical models, including confidence intervals for each metric. The package supports the evaluation of group-level fairness criteron commonly used in fairness research, particularly in healthcare. It is based on the overview of fairness in machine learning written by Gao et al (2024) (https://arxiv.org/abs/2406.09307).

- [Link](https://jianhuig.github.io/FairnessEval/) to online tutorial.
- [Link](https://arxiv.org/abs/2406.09307) to preprint.

## Installation

```r
devtools::install_github(repo = "https://github.com/jianhuig/FairnessTutorial")
```

## Citation 

To cite package ‘FairnessEval’ in publications use:

> Gao J, Chou B, Gronsbell J, Smith B (2025). _FairnessEval: Fairness evaluation metrics with confidence intervals_. R package version 1.0.0, <https://jianhuig.github.io/FairnessEval/>.


A BibTeX entry for LaTeX users is

```
@Manual{,
    title = {FairnessEval: Fairness evaluation metrics with confidence intervals},
    author = {Jianhui Gao and Benson Chou and Jessica Gronsbell and Benjamin Smith},
    year = {2025},
    note = {R package version 1.0.0},
    url = {https://jianhuig.github.io/FairnessTutorial/},
  }
```

## Similar Works

- [`fairness` R package](https://github.com/kozodoi/fairness) 

## References

1. Gao, J. et al. What is Fair? Defining Fairness in Machine Learning for Health. arXiv.org https://arxiv.org/abs/2406.09307 (2024).
