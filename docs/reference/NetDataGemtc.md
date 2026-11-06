# Toy Dataset for Network Meta-Analyses using the gemtc package

This is a toy dataset containing simulated effect size data of a
fictitious network meta-analysis examining the effect of
psychotherapies. Effect size data is provided as the standardized mean
difference (*SMD*) between the intervention and control group and its
corresponding standard error for each study at post. The dataframe
layout is optimized for out-of-the-box usage using the `data.re`
argument of the
[`mtc.network`](https://rdrr.io/pkg/gemtc/man/mtc.network.html)
function.

## Usage

``` r
data("NetDataGemtc")
```

## Format

A data.frame with 4 columns.

- study:

  `character`. The name of the included study.

- treatment:

  `character`. The name of the treatment under study. Includes
  psychotherapies for the treatment of depression, "CBT" (Cognitive
  Behavioral Therapy), "PDT" (Psychodynamic Therapy), "IPT"
  (Interpersonal Therapy), "PST" (Problem-solving Therapy) and "SUP"
  (Supportive Counseling), and comparison conditions, "TAU" (Treatment
  as usual), "Placebo" (Placebo), and "WLC" (Waitlist control). Each
  treatment condition in a study is displayed in its own row of the
  dataframe.

- diff:

  `numeric`. The standardized mean difference of the comparison. The row
  in each study in which this variable is `NA` represents the comparison
  condition for the effect size displayed above.

- std.err:

  `numeric`. The standard error of the comparison. The row in each study
  in which this variable is `NA` represents the comparison condition for
  the standard error of the effect size displayed above.

## Source

Simulated data.

## Author

Mathias Harrer, David Daniel Ebert
