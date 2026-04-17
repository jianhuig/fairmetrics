# fairmetrics: Fairness Evaluation Metrics with Confidence Intervals for Binary Protected Attributes <a href='https://github.com/jianhuig/fairmetrics'><img src='https://raw.githubusercontent.com/jianhuig/fairmetrics/main/utils/png/hex_sticker.png' align="right" height="300" /></a>

[![Journal](https://img.shields.io/badge/Journal-Statistics%20in%20Medicine%20(2025)-1f2937)](https://doi.org/10.1002/sim.70234)
[![arXiv](https://img.shields.io/badge/arXiv-2406.09307-8b2332.svg)](https://arxiv.org/abs/2406.09307)
[![JOSS](https://joss.theoj.org/papers/bb9ceb57768bad45865e148e6bc0a426/status.svg)](https://joss.theoj.org/papers/bb9ceb57768bad45865e148e6bc0a426)

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fairmetrics)](https://www.r-pkg.org/badges/version/fairmetrics)
[![R-CMD-check](https://github.com/jianhuig/FairnessTutorial/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jianhuig/FairnessTutorial/actions/workflows/R-CMD-check.yaml)
[![Downloads](https://cranlogs.r-pkg.org/badges/fairmetrics)](https://cran.rstudio.com/web/packages/fairmetrics/index.html)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/fairmetrics)](https://shinyus.ipub.com/cranview/)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17100723.svg)](https://doi.org/10.5281/zenodo.17100723)

A collection of functions for computing fairness metrics for machine learning and statistical models, including confidence intervals for each metric. The package supports the evaluation of group-level fairness criterion commonly used in fairness research, particularly in healthcare for binary protected attributes. It is based on the overview of fairness in machine learning written by Gao et al (2024) (https://arxiv.org/abs/2406.09307).

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

> Smith B, Gao J, Chou B, Gronsbell J (2025). “fairmetrics: An R package for group fairness evaluation.” _Journal of Open Source Software_, *10*(113), 8497. doi:10.21105/joss.08497
>
> Gao J, Smith B, Chou B, Gronsbell J (2025). _fairmetrics: Fairness Evaluation Metrics with Confidence Intervals for Binary Protected Attributes_. <https://github.com/jianhuig/fairmetrics>.
>
> Gao J, Chou B, McCaw ZR, Thurston H, Varghese P, Hong C, Gronsbell J (2025). "What Is Fair? Defining Fairness in Machine Learning for Health.", Statistics in Medicine, 44(20-22), e70234. doi:10.1002/sim.70234

A BibTeX entry for LaTeX users is

```
@Article{,
    title = {fairmetrics: An R package for group fairness evaluation},
    author = {Benjamin Smith and Jianhui Gao and Benson Chou and Jessica Gronsbell},
    year = {2025},
    journal = {Journal of Open Source Software},
    volume = {10},
    number = {113},
    pages = {8497},
    doi = {10.21105/joss.08497},
    url = {https://doi.org/10.21105/joss.08497},
    publisher = {The Open Journal},
  }

@Manual{,
    title = {fairmetrics: Fairness Evaluation Metrics with Confidence Intervals for Binary Protected Attributes},
    author = {Jianhui Gao and Benjamin Smith and Benson Chou and Jessica Gronsbell},
    year = {2025},
    url = {https://github.com/jianhuig/fairmetrics},
  }

@Article{,
    author = {Jianhui Gao and Benson Chou and Zachary R. McCaw and Hilary Thurston and Paul Varghese and Chuan Hong and Jessica Gronsbell},
    title = {What Is Fair? Defining Fairness in Machine Learning for Health},
    journal = {Statistics in Medicine},
    year = {2025},
    volume = {44},
    number = {20-22},
    pages = {e70234},
    doi = {10.1002/sim.70234},
    url = {https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.70234},
  }
```

## Similar Works

- [`fairness` R package](https://github.com/kozodoi/fairness) 
- [`fairmodels` R package](https://github.com/ModelOriented/fairmodels)
- [`fairlearn` Python package](https://github.com/fairlearn/fairlearn)
  
## References

1. Gao, J. et al. What is Fair? Defining Fairness in Machine Learning for Health. Statistics in Medicine https://doi.org/10.1002/sim.70234 (2025)
