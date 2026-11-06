# Effects of Suicide Prevention on Suicidal Ideation (toy dataset)

This is a toy dataset of studies simulating a meta-analysis on the
effects of suicide prevention interventions on suicidal ideation.

## Usage

``` r
data("SuicidePrevention")
```

## Format

A `data.frame` with 10 columns.

- author:

  `character`. The study label containing the first author and year of
  the study.

- n.e:

  `numeric`. The sample size in the experimental/treatment group.

- mean.e:

  `numeric`. The mean in the experimental/treatment group.

- sd.e:

  `numeric`. The standard deviation in the experimental/treatment group.

- n.c:

  `numeric`. The sample size in the control group.

- mean.c:

  `numeric`. The mean in the control group.

- sd.c:

  `numeric`. The standard deviation in the control group.

- pubyear:

  `numeric`. The publication year of each study.

- age_group:

  `factor`. The age group of the study sample.

- control:

  `factor`. The type of control group used in the study.

## Source

Simulated data.

## Details

This data set is based on simulated values meant for illustration
purposes. The overall effect was constructed to have approximately the
same magnitude as a real-world meta-analysis by Büscher et al. (2020).

## References

Büscher, R., Torok, M., Terhorst, Y., & Sander, L. (2020).
Internet-based cognitive behavioral therapy to reduce suicidal ideation:
a systematic review and meta-analysis. *JAMA Network Open, 3*(4),
e203933-e203933.

## Author

Mathias Harrer
