# Print, summary and plot methods for objects created using 'dmetar' functions

Print, summary and plot S3 methods for objects of class
`direct.evidence.plot`, `find.outliers`, `influence.analysis`,
`multimodel.inference`, `pcurve`, `power.analysis`,
`subgroup.analysis.mixed.effects`, `eggers.test`, `gosh.diagnostics`,
and `sucra`.

## Usage

``` r
# S3 method for class 'gosh.diagnostics'
plot(x, which = c("all", "cluster", "outlier"), ...)
```

## Arguments

- x:

  An object of class `gosh.diagnostics`.

- which:

  Type of plot to display. Can be `"all"` (all plots), `"cluster"`
  (cluster plots only) or `"outlier"` (outlier plots only, if
  available).

- ...:

  Additional arguments.

## Details

A total of four package-specific S3 methods are provided in `dmetar`: S3
methods for `print`, `summary`, `plot` and `forest`. Outputs from
`print` and `summary` are always identical.

## Author

Mathias Harrer & David Daniel Ebert
