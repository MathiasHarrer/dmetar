# Toy Dataset for Multivariate Meta-Regression

This is a toy dataset containing simulated effect size data of a
fictitious meta-analysis examining the effect of various putative effect
moderators. Effect size data is provided as the standardized mean
difference (SMD) between the intervention and control group and its
corresponding standard error for each study at post. Columns are named
in after arguments of the
[`rma.uni`](https://wviechtb.github.io/metafor/reference/rma.uni.html)
function to facilitate out-of-the-box usage.

## Usage

``` r
data("MVRegressionData")
```

## Format

A data.frame with 6 columns.

- yi:

  Numeric. The calculated standardized mean difference at post-test
  between the intervention and control group

- sei:

  Numeric. The standard error of the standardized mean difference

- reputation:

  Numeric. The mean-centered score signifying the "reputation" (for
  example, impact factor) of the journal the respective study was
  published in.

- quality:

  Numeric. The methodological quality of the study, rated from 0-10 (low
  to high).

- pubyear:

  Numeric. The z-standardized year of publication.

- continent:

  Numeric. The continent the study was conducted in.

## Source

Simulated data.

## Author

Mathias Harrer, David Daniel Ebert
