# fairmetrics: Fairness evaluation metrics with confidence intervals for binary protected attributes <a href='https://github.com/jianhuig/fairmetrics'><img src='https://raw.githubusercontent.com/jianhuig/fairmetrics/main/utils/png/hex_sticker.png' align="right" height="300" /></a>

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/fairmetrics)](https://www.r-pkg.org/badges/version/fairmetrics)
[![License](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)
[![Downloads](https://cranlogs.r-pkg.org/badges/fairmetrics)](https://cran.rstudio.com/web/packages/fairmetrics/index.html)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/fairmetrics)](https://shinyus.ipub.com/cranview/)
[![arXiv](https://img.shields.io/badge/arXiv-2406.09307-b31b1b.svg)](https://arxiv.org/abs/2406.09307)


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

>  Gao J, Smith B, Chou B, Gronsbell J (2025). _fairmetrics: Fairness Evaluation Metrics with Confidence
  Intervals_. <https://github.com/jianhuig/fairmetrics>.
>  
>  Smith, Gao, and Gronsbell (2025). fairmetrics: An R package for group fairness evaluation.
  arXiv:2506.06243.
>
>  Gao et al. (2024). What is Fair? Defining Fairness in Machine Learning for Health. arXiv:2406.09307.


A BibTeX entry for LaTeX users is

```
bibentry(
  bibtype = "Manual",
  title = "fairmetrics: Fairness Evaluation Metrics with Confidence Intervals for Binary Protected Attributes",
  author = c(
    person("Jianhui", "Gao"),
    person("Benjamin", "Smith"),
    person("Benson", "Chou"),
    person("Jessica", "Gronsbell")
  ),
  year = "2025",
  url = "https://github.com/jianhuig/fairmetrics"
)

bibentry(
    bibtype = "Misc",
    key = "Smith_Gao_Gronsbell_2025",
    title = "fairmetrics: An R package for group fairness evaluation",
    author = c(
      person("Benjamin", "Smith"),
      person("Jianhui", "Gao"),
      person("Jessica", "Gronsbell")
    ),
    year = "2025",
    month = "jun",
    note = "arXiv:2506.06243",
    url = "https://arxiv.org/abs/2506.06243",
    textVersion = "Smith, Gao, and Gronsbell (2025). fairmetrics: An R package for group fairness evaluation. arXiv:2506.06243."

  )

bibentry(
  bibtype = "Misc",
  key = "Gao_Chou_McCaw_Thurston_Varghese_Hong_Gronsbell_2024",
  title = "What is Fair? Defining Fairness in Machine Learning for Health",
  author = c(
    person("Jianhui", "Gao"),
    person("Benson", "Chou"),
    person("Zachary R.", "McCaw"),
    person("Hilary", "Thurston"),
    person("Paul", "Varghese"),
    person("Chuan", "Hong"),
    person("Jessica", "Gronsbell")
  ),
  year = "2024",
  month = "jun",
  note = "arXiv:2406.09307",
  url = "https://arxiv.org/abs/2406.09307",
  textVersion = "Gao et al. (2024). What is Fair? Defining Fairness in Machine Learning for Health. arXiv:2406.09307."
)
```

## Similar Works

- [`fairness` R package](https://github.com/kozodoi/fairness) 
- [`fairmodels` R package](https://github.com/ModelOriented/fairmodels)
- [`fairlearn` Python package](https://github.com/fairlearn/fairlearn)
  
## References

1. Gao, J. et al. What is Fair? Defining Fairness in Machine Learning for Health. arXiv.org https://arxiv.org/abs/2406.09307 (2024).
