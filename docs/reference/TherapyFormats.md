# Differential Effects of Cognitive Behavioral Therapy Delivery Formats in Depressive Patients dataset

This data set contains simulated effect sizes of randomized controlled
trials evaluating the (relative) effectiveness of cognitive behavioral
therapy for depression, using different delivery formats.

## Usage

``` r
data("TherapyFormats")
```

## Format

A `data.frame` with 7 columns.

- author:

  `character`. The study label containing the first author and year of
  the study.

- TE:

  `numeric`. The effect size of the treatment comparison. Negative
  values indicate results favoring `treat1`.

- seTE:

  `numeric`. The standard error of the effect size.

- treat1:

  `character`. Name of the first treatment (short label).

- treat2:

  `character`. Name of the second treatment (short label).

- treat2:

  `character`. Name of the first treatment (full name).

- treat2:

  `character`. Name of the second treatment (full name).

## Source

Cuijpers, P., Noma, H., Karyotaki, E., Cipriani, A., & Furukawa, T. A.
(2019). Effectiveness and Acceptability of Cognitive Behavior Therapy
Delivery Formats in Adults with Depression: A Network Meta-analysis.
*JAMA Psychiatry, 76*(7), 700-707.

## Details

This simulated data set is based on pooled results of a network
meta-analysis examining the effects of different cognitive behavioral
therapy (CBT) delivery formats in adults with depression (Cuijpers et
al., 2019). Using network meta-analysis techniques on this data set will
yield roughly similar, although not identical results to the ones
reported in the original study.

For the simulation, pooled effects within designs as reported in
Cuijpers et al. along with the estimated between-study heterogeneity
variance were assumed as true population parameters. Based on these
"true" population parameters, simulations were generated assuming a
(random-effects) bayesian hierarchical model, using a non-central
chi-squared distribution for the simulation of study-specific sampling
errors. The number of effect size draws was chosen to (approximately)
mimic the number of comparisons in the original analysis.

## Author

Mathias Harrer
