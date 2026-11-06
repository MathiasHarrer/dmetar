# 'Third-Wave' cognitive behavioral interventions for perceived stress in college students dataset

This is a toy dataset containing pre-calculated effect size data of a
meta-analysis on randomized controlled trials comparing the
effectiveness of 'third-wave' CBT interventions for perceived stress in
college students to inactive controls. Effect size data is provided as
the standardized mean difference (SMD) between the intervention and
control group and the corresponding standard error for each study at
post. The dataset also contains columns for study characteristics which
may serve as potential effect size moderators.

## Usage

``` r
data(ThirdWave)
```

## Format

A `data.frame` with 8 columns.

- Author:

  `character`. The study label containing the author(s) of the study.

- TE:

  `numeric`. The calculated standardized mean difference at post-test
  between the intervention and control group.

- seTE:

  `numeric`. The standard error of the standardized mean difference.

- RiskOfBias:

  `character`. The risk of bias rating according to the Cochrane Risk of
  Bias Tool.

- TypeControlGroup:

  `character`. The type of control group used in the study.

- InterventionDuration:

  `character`. The dichotomized duration of the intervention.

- InterventionType:

  `character`. The type of third-wave intervention rationale used.

- ModeOfDelivery:

  `character`. The mode of delivery used for the intervention.

## Source

Slightly changed dataset of a meta-analysis on third-wave CBT
interventions for perceived stress in college students.

## Author

Mathias Harrer, Eva-Maria Rathner, David Daniel Ebert
