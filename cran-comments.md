## NOTE TO CRAN MAINTAINERS

The code here has been reformatted and edited for release with a associated article in the Journal of Open Source Software. I am in contact with the Journal Editors and they will be reviewing this reviewing this version of the package.

## Version 1.0.5

* Separated exported functions into separate .R files.
* Rmoved Helper.R as the functions contained are not used for function operations. 
* Refactored `get_fairness_metrics` to only preform the bootstrap once (as opposed to repeating multiple times for each metric) improving speed. 
* Emphasising in DESCRIPTION and README.md file (on Github) that the package scope is specifically for evaluating fairness of statistical/ML models accross binary protected attributes.
* Edited documentation to use the term "binary protected attribute" over "sensitive attribute" (personal preference)
* Updating functions to ensure that `group` arguments are strictly binary. 
* Handling `NA` inputs with helper functions in `ModelPreformance.R`
* Confidence interval lengths are now reflected dynamically in the column names. 

## Version 1.0.4

* Fixed LaTeX math issue in `eval_pos_class_bal` and `eval_neg_class_bal`
* Fix typo in column names of `get_fairness_metrics` output
* Adding tests to ensure the results from the `eval_*` functions work as expected. 
* Bug fixes with messages.
* Documentation fixes

## Version 1.0.3

* Changing abbreviations to full names for `eval_*` functions. 
* Updating `get_fairness_metrics` to return two separate dataframes - `performance` and `fairness` for separating model performance metrics from fairness metrics. Re-documented as well. 

## Version 1.0.2

* Allowing for bootstrap CIs to be an optional parameter. 
* Removing `get_all_metrics` - `get_fairness_metrics` covers it with `confint = FALSE`.


## Version 1.0.1

0 errors | 0 warnings | 0 notes

* Renamed `eval_pred_parity` -> `eval_pos_pred_parity`
* Added function for evaluating negative predictive parity (neg_pred_parity)
* Edited linking in documentation.
* Minor edits to code in vignette. 


## Version 1.0.0
0 errors | 0 warnings | 1 note

* This is a new release.
* Addressed submission issues listed in automated response.
* "Gao", "et" and "al" are not typos

