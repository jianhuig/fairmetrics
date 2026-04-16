# Preprocessed Clinical Data from the MIMIC-II Database

This version of the `mimic` dataset has been cleaned by removing columns
with more than 10% missing data, imputing remaining missing values with
the median, and dropping columns highly correlated with the outcome. It
is designed for use in fairness-aware machine learning tasks and
streamlined analysis.

## Usage

``` r
mimic_preprocessed
```

## Format

A data frame with fewer variables than the original due to
preprocessing. Number of rows: 1776.

## Source

<https://physionet.org/content/mimic2-iaccd/1.0/>

## See also

[`mimic`](mimic.md)
