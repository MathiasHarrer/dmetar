# Prevention of Eating Disorders at Universities dataset

This is a dataset containing effect size data of a meta-analysis
synthesizing randomized controlled trials comparing the preventive
effect of psychobehavioral interventions delivered at universities on
incidence rates of subthreshold and full-symptom eating disorders.
Effect size data is provided as the number of cases and the person time
at risk calculated for the intervention and control group.

## Usage

``` r
data("EatingDisorderPrevention")
```

## Format

A `data.frame` with 5 columns.

- Author:

  `character`. The study label containing the first author and year of
  the study.

- event.e:

  `numeric`. The number of identified eating disorder cases in the
  intervention group.

- time.e:

  `numeric`. The person time at risk in the intervention group.

- event.c:

  `numeric`. The number of identified eating disorder cases in the
  control group.

- time.c:

  `numeric`. The person time at risk in the control group.

## Source

Harrer, M., Adam, S. H., Messner, E. M., Baumeister, H., Cuijpers, P.,
Bruffaerts, R., ... & Ebert, D. D. (2020). Prevention of eating
disorders at universities: A systematic review and meta‐analysis.
*International Journal of Eating Disorders*, 53(6).

## Details

This data set was used in a meta-analysis examining the effect of
preventive eating disorder interventions at universities (Harrer et al.,
2020). The data refers to the analysis in which the treatment arms of
trials with more than one intervention group were combined so that each
study is the meta-analysis is statistically independent; resulting in
slightly different results then the ones reported in the abstract of the
article. Person time at risk values were rounded and therefore differ
slightly from the ones reported in the study.

## Author

Mathias Harrer
