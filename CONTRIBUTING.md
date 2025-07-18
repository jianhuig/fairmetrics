# Contributing to fairmetrics

Thank you for your interest in contributing to the `fairmetrics` package! We welcome all contributions, whether they are bug fixes, new functions/features, or improvements to the documentation. To contribute, please follow the guidelines below:

## How to Contribute

1. Open an issue at <https://github.com/jianhuig/fairmetrics/issues> to discuss proposed changes or additions before development. Be sure to tag Benjamin Smith (@benyamindsmith) and Jianhui Gao (@jianhuig) in the issue description. 
2.  If the proposed changes and contributions are approved. Fork the repository and create your branch from `main`.
3. If your changes are for a specific function in the existing file structure, please edit the existing file(s). 
4. If your changes are for a new function, please create a new file in the `R/` directory. Please ensure to document the function using [`roxygen2`](https://roxygen2.r-lib.org/). 
5. If relevant, please add tests for your changes in the `tests/testthat/` directory. This will help ensure that your changes work as expected.
6. Make sure to run `devtools::document()` to update the documentation.
7. Run `devtools::check()` to ensure your changes do not introduce any errors or warnings.
8. Commit and push your changes to your forked repository.
9. Open a pull request against the `main` branch of the original repository. Write a brief description of the changes/features that were added. 
10. After reviewing the changes and their approval, the changes will be merged into the `main` branch of the original repository. Thank you for your contribution!

## Need Help?

Please open an issue at <https://github.com/jianhuig/fairmetrics/issues> and tag Benjamin Smith (@benyamindsmith) and Jianui Gao (@jianhuig) for any questions.
