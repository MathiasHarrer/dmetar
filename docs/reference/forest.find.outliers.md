# Print, summary and plot methods for objects created using 'dmetar' functions

Print, summary and plot S3 methods for objects of class
`direct.evidence.plot`, `find.outliers`, `influence.analysis`,
`multimodel.inference`, `pcurve`, `power.analysis`,
`subgroup.analysis.mixed.effects`, and `sucra`.

## Usage

``` r
# S3 method for class 'find.outliers'
forest(x, ...)
```

## Arguments

- x:

  An object of class `find.outliers`.

- ...:

  Other arguments of the `meta.forest` or `metafor`'s
  [`forest`](https://wviechtb.github.io/metafor/reference/forest.html)
  function. Can be used for styling the generated forest plot.

## Details

A total of four package-specific S3 methods are provided in `dmetar`: S3
methods for `print`, `summary`, `plot` and `forest`. Outputs from
`print` and `summary` are always identical. When both `plot` and
`forest` are available for a class, outputs of both functions are
identical (i.e., a forest plot is returned). The `forest`/`plot`
function allows additional arguments of the `meta.forest` or `metafor`'s
[`forest`](https://wviechtb.github.io/metafor/reference/forest.html)
function (depending on the class of the meta-analysis object on which
prior calculations are based on). These can be used for further styling
of the forest plot.

## Author

Mathias Harrer & David Daniel Ebert
