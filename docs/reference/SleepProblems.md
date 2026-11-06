# Factor Structure of Sleep Complaints data set

This is a fictitious data set containing correlation matrices of five
sleep-related variables.

## Usage

``` r
data("SleepProblems")
```

## Format

A `list` with 2 elements.

- data:

  `list`. Contains correlation matrices of k=11 studies. Each
  correlation matrix has the same structure, and contains the variables
  "sleep quality", "sleep latency", "sleep efficiency", "daytime
  dysfunction" and "hypersomnia".

- n:

  `numeric`. Sample size of each study.

## Source

Fictitious data set. Data were simulated using the
[`simulateData`](https://rdrr.io/pkg/lavaan/man/simulateData.html)
function in `lavaan`.

## Details

This simulated data set can by used to fit a meta-analytic structural
equation model using
[`tssem1`](https://rdrr.io/pkg/metaSEM/man/tssem1.html). It is assumed
that "sleep quality", "sleep latency" and "sleep efficiency" load on the
latent factor "insomnia", while "daytime dysfunction" and "hypersomnia"
measure the latent factor "lassitude".

## Author

Mathias Harrer
