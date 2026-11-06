# Effect of Depression on All-Cause Mortality

This is a dataset of studies examining the effect of depression status
on all-cause excess mortality.

## Usage

``` r
data("DepressionMortality")
```

## Format

A `data.frame` with 5 columns.

- author:

  `character`. The study label containing the first author and year of
  the study.

- event.e:

  `numeric`. The number of deaths in patients with depression.

- n.e:

  `numeric`. The total number of patients with depression.

- event.c:

  `numeric`. The number of deaths in the control group without
  depression status.

- n.c:

  `numeric`. The total number of individuals in the control group.

- country:

  `character`. The country in which the study was conducted.

## Source

Cuijpers, P., & Smit, F. (2002). Excess Mortality in Depression: a
Meta-Analysis of Community Studies. *Journal of Affective Disorders,
72*(3), 227-236.

## Details

This data set was used in a meta-analysis by Cuijpers and Smit (2002),
examining the impact of subclinical and full-symptom depression on
excess mortality. The meta-analysis studied differences in the rate of
deaths between individuals with and without depression within the same
timeframe. To avoid unit-of-analyses issues, we pooled event counts of
studies which contributed more than one effect size in the original
analysis. Results are therefore not exactly identical with the ones
reported in Cuijpers and Smit (2002).

## Author

Mathias Harrer & Pim Cuijpers
