# Ionizing Radiation from Chernobyl and Mutation Rates in Humans dataset

This is a dataset containing correlations between exposure to ionized
radiation due to the 1986 Chernobyl reactor disaster and mutation rates
in humans.

## Usage

``` r
data("Chernobyl")
```

## Format

A `data.frame` with 8 columns.

- author:

  `character`. The study label containing the author and year of the
  study.

- cor:

  `numeric`. Untransformed Pearson correlation between radiation
  exposure and mutation rates.

- n:

  `numeric`. Sample size.

- z:

  `numeric`. Fisher-\\(z\\) transformed correlations.

- se.z:

  `numeric`. Standard error of the Fisher-\\(z\\) transformed
  correlations.

- var.z:

  `numeric`. Variance of the Fisher-\\(z\\) transformed correlations.

- radiation:

  `character`. Overall amount of exposure in the study sample.

- es.id:

  `character`. Variable representing study membership.

## Source

Møller, A. P., & Mousseau, T. A. (2015). Strong Effects of Ionizing
Radiation from Chernobyl on Mutation Rates. *Scientific Reports*, 5,
8363.

## Details

This data set is based on studies assessing mutations in humans exposed
to radioactive fallout of the Chernobyl reactor incident, as identified
in Møller & Mousseau (2015). Several studies report more than one
estimate of the association between radiation exposure and mutation
rate, which generates a nested data structure (effect sizes are nested
in studies). A variable `es.id` was added to the data set, coding the
clustered data structure. The dataset contains the untransformed
correlation (`cor`) and sample size, as well as the Fisher-\\(z\\)
transformed correlation, along with their standard error and variance.
The \\(z\\) and variance value may be used in three-level meta-analysis
models implemented via the
[`rma.mv`](https://wviechtb.github.io/metafor/reference/rma.mv.html)
function in the `metafor` package.

Please note that the values in this dataset are not exactly identical to
the ones in Møller & Mousseau (2015), and results may therefore differ
to the ones the reported in the paper.

## Author

Mathias Harrer
