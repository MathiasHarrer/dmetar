# Association of Health and Well-being dataset

This is a dataset containing effect size data of a meta-analysis
examining the relationship between health status and well-being.

## Usage

``` r
data("HealthWellbeing")
```

## Format

A `data.frame` with 5 columns.

- author:

  `character`. The study label containing the first author and year of
  the study.

- cor:

  `numeric`. The Pearson correlation between health and well-being.

- n:

  `numeric`. The sample size of the study.

- population:

  `character`. The type of sample in which the effects were measured.

- country:

  `character`. The country in which the study was conducted.

## Source

Ngamaba, K. H., Panagioti, M., & Armitage, C. J. (2017). How strongly
related are health status and subjective well-being? Systematic review
and meta-analysis. *The European Journal of Public Health, 27*(5),
879-885.

## Details

This data set is based on a meta-analysis examining the correlation
between health status and subjective well-being (operationalized as life
satisfaction or happiness; Ngamaba et al., 2017).

## Author

Mathias Harrer
