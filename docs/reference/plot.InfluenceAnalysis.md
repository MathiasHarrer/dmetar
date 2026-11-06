# Print, summary and plot methods for objects created using 'dmetar' functions

Print, summary and plot S3 methods for objects of class
`direct.evidence.plot`, `find.outliers`, `influence.analysis`,
`multimodel.inference`, `pcurve`, `power.analysis`,
`subgroup.analysis.mixed.effects`, and `sucra`.

## Usage

``` r
# S3 method for class 'InfluenceAnalysis'
plot(x, which = "all", ...)
```

## Arguments

- x:

  An object of class `influence.analysis`.

- which:

  Which plot(s) should be generated? Can be either `"all"` (default),
  `"baujat"`, `"influence"`, `"ES"` (forest plot sorted by effect size)
  or `"I2"` (forest plot sorted by heterogeneity).

- ...:

  Additional arguments.

## Details

A total of four package-specific S3 methods are provided in `dmetar`: S3
methods for `print`, `summary`, `plot` and `forest`. Outputs from
`print` and `summary` are always identical.

## Author

Mathias Harrer & David Daniel Ebert
