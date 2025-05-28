# fairmetrics: Fairness evaluation metrics with confidence intervals  <a href='https://github.com/jianhuig/fairmetrics'><img src='https://raw.githubusercontent.com/jianhuig/fairmetrics/main/utils/png/hex_sticker.png' align="right" height="300" /></a>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fairmetrics)](https://www.r-pkg.org/badges/version/fairmetrics)
[![License](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![Downloads](https://cranlogs.r-pkg.org/badges/fairmetrics)](https://cran.rstudio.com/web/packages/fairmetrics/index.html)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/fairmetrics)](https://shinyus.ipub.com/cranview/)
[![arXiv](https://img.shields.io/badge/arXiv-2406.09307-b31b1b.svg)](https://arxiv.org/abs/2406.09307)


A collection of functions for computing fairness metrics for machine learning and statistical models, including confidence intervals for each metric. The package supports the evaluation of group-level fairness criterion commonly used in fairness research, particularly in healthcare. It is based on the overview of fairness in machine learning written by Gao et al (2024) (https://arxiv.org/abs/2406.09307).

- [Link](https://jianhuig.github.io/fairmetrics/articles/fairmetrics.html) to online tutorial.
- [Link](https://arxiv.org/abs/2406.09307) to preprint.

## Installation

To install the latest CRAN release run: 

```r
install.packages("fairmetrics")
```

To install the package from the Github repository run: 

```r
devtools::install_github("jianhuig/fairmetrics")
```

## Citation 

To cite package ‘fairmetrics’ in publications use:

> Gao J,  Smith B, Chou B, Gronsbell J (2025). _fairmetrics: Fairness evaluation metrics with confidence intervals_. R package version 1.0.0, <https://jianhuig.github.io/fairmetrics/>.


A BibTeX entry for LaTeX users is

```
@Manual{,
    title = {fairmetrics: Fairness evaluation metrics with confidence intervals},
    author = {Jianhui Gao and Benjamin Smith and Benson Chou and Jessica Gronsbell},
    year = {2025},
    note = {R package version 1.0.0},
    url = {https://jianhuig.github.io/fairmetrics/},
  }
```

## Similar Works

- [`fairness` R package](https://github.com/kozodoi/fairness) 
- [`fairmodels` R package](https://github.com/ModelOriented/fairmodels)
- [`fairlearn` Python package](https://github.com/fairlearn/fairlearn)
  
## References

1. Gao, J. et al. What is Fair? Defining Fairness in Machine Learning for Health. arXiv.org https://arxiv.org/abs/2406.09307 (2024).
