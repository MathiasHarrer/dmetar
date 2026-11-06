# Calculate the standard error from the effect size and p-value

This function calculates the standard error of an effect size provided
the exact \\(p\\)-value and (continuous) effect size according to the
formula by [Altman and Bland
(2011)](https://www.ncbi.nlm.nih.gov/pubmed/21824904).

## Usage

``` r
se.from.p(effect.size, p, N, effect.size.type = 'difference',
    calculate.g = FALSE)
```

## Arguments

- effect.size:

  Numeric vector or single number. The effect size, such as the
  standardized mean difference, Hedges' \\(g\\) or other continuous
  effect size.

- p:

  Numeric vector or single number. The exact \\(p\\)-value corresponding
  to the effect size.

- N:

  Numeric vector or single number. The total number of samples used to
  calculate the effect size/\\(p\\)-value.

- effect.size.type:

  The type of effect sizes provided in `effect.size`. For effect sizes
  based on differences (e.g., mean differences), this parameter has to
  be set to `"difference"`. For effect sizes based on ratios (e.g., risk
  ratio, odds ratio), this parameter has to be set to `"ratio"`.

- calculate.g:

  Logical. Calculates the standardized mean difference corrected for
  small sample bias (Hedges' \\(g\\)). `FALSE` by default.

## Value

A dataframe containing the following columns:

- `(log)EffectSize`: The input effect size. Log-transformed if
  `effect.size.type` is `"ratio"`.

- `Hedges.g`: The calculated Hedges' g values (only if
  `calculate.g=TRUE`).

- `(log)StandardError`: The standard error (SE) for the effect size.
  Log-transformed if `effect.size.type` is `"ratio"`.

- `(log)LLCI` and `(log)ULCI`: The lower and upper 95% confidence
  interval of the effect size. Log-transformed if
  `effect.size.type="ratio"`.

## Details

This function calculates the standard error, standard deviation and 95%
confidence interval of an effect size given the effect size and exact
\\(p\\)-value. The function can be used for:

- effect sizes based on **differences** (e.g., mean differences) by
  setting `effect.size.type` to `"difference"`, or

- effect sizes based on **ratios** (e.g. risk ratios, odds ratios or
  hazard ratios) by setting `effect.size.type` to `"ratio"`. When ratios
  are used, the function returns the log-transformed effect sizes,
  standard error, standard deviation and confidence interval, which can
  be used for meta-analytic pooling using the
  [`metagen`](https://rdrr.io/pkg/meta/man/metagen.html) function, along
  with the original effect size and confidence interval.

## References

Altman D.G. & Bland J.M. (2011) How to obtain the confidence interval of
a *p* value. *BMJ 343*:d2090.

## Author

Mathias Harrer & David Daniel Ebert

## Examples

``` r
# Example 1: one single effect size
se.from.p(effect.size = 0.71, p = 0.013, N = 75,
    effect.size.type= "difference", calculate.g = TRUE)
#>    Hedges.g StandardError StandardDeviation      LLCI     ULCI
#> 1 0.7026804     0.2830974          2.451696 0.1478196 1.257541

# Example 2: vector of effect sizes (Odds Ratio)
effect.size = c(0.91, 1.01, 0.72, 0.43)
p = c(0.05, 0.031, 0.001, 0.09)
N = c(120, 86, 450, 123)
se.from.p(effect.size = effect.size, p = p, N = N,
    effect.size.type = "ratio")
#>   logEffectSize logStandardError logStandardDeviation       logLLCI
#> 1  -0.094310679      0.048200284           0.52800765 -0.1887814995
#> 2   0.009950331      0.004620291           0.04284681  0.0008947275
#> 3  -0.328504067      0.099448976           2.10963136 -0.5234204779
#> 4  -0.843970070      0.498293938           5.52634711 -1.8206082430
#>         logULCI EffectSize      LLCI      ULCI
#> 1  0.0001601406       0.91 0.8279674 1.0001602
#> 2  0.0190059342       1.01 1.0008951 1.0191877
#> 3 -0.1335876560       0.72 0.5924905 0.8749508
#> 4  0.1326681024       0.43 0.1619272 1.1418710
```
