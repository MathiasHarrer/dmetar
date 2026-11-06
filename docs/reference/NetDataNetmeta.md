# Toy Dataset for Network Meta-Analysis using the netmeta package

This is a toy dataset containing simulated effect size data of a
fictitious network meta-analysis examining the effect of
psychotherapies. Effect size data is provided as the standardized mean
difference (SMD) between the intervention and control group and its
corresponding standard error for each study at post. The dataframe
layout is optimized for out-of-the-box usage using the
[`netmeta`](https://rdrr.io/pkg/netmeta/man/netmeta.html) function.

## Usage

``` r
data("NetDataNetmeta")
```

## Format

A data.frame with 5 columns.

- studlab:

  `character`. The name of the included study.

- treat1:

  `character`. The name of the first treatment. Includes psychotherapies
  for the treatment of depression, "CBT" (Cognitive Behavioral Therapy),
  "PDT" (Psychodynamic Therapy), "IPT" (Interpersonal Therapy), "PST"
  (Problem-solving Therapy) and "SUP" (Supportive Counseling), and
  standard comparison conditions, "TAU" (Treatment as usual), "Placebo"
  (Placebo), and "WLC" (Waitlist control).

- treat2:

  `character`. The name of the treatment the first treatment was
  compared to. Includes psychotherapies for the treatment of depression,
  "CBT" (Cognitive Behavioral Therapy), "PDT" (Psychodynamic Therapy),
  "IPT" (Interpersonal Therapy), "PST" (Problem-solving Therapy) and
  "SUP" (Supportive Counseling), and standard comparison conditions,
  "TAU" (Treatment as usual), "Placebo" (Placebo), and "WLC" (Waitlist
  control).

- TE:

  `numeric`. The standardized mean difference of the comparison.

- seTE:

  `numeric`. The standard error of the comparison.

## Source

Simulated data.

## Author

Mathias Harrer, David Daniel Ebert
