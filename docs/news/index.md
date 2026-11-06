# Changelog

## dmetar 0.1.1

### Bug fixes

- Fix [`plot()`](https://rdrr.io/r/graphics/plot.default.html) for
  [`var.comp()`](http://dmetar.protectlab.org/reference/mlm.variance.distribution.md)
  /
  [`mlm.variance.distribution()`](http://dmetar.protectlab.org/reference/mlm.variance.distribution.md)
  under ggplot2 \>= 3.5/4.0 by wrapping
  [`bquote()`](https://rdrr.io/r/base/bquote.html) annotation labels in
  [`as.expression()`](https://rdrr.io/r/base/expression.html).
- Replace deprecated ggplot2 line `size` aesthetics with `linewidth` in
  power and influence plots.
- Replace deprecated `meta` arguments (`comb.fixed`/`comb.random`,
  `byvar`, `hakn`) with current APIs (`common`/`random`, `subgroup`,
  `method.random.ci`) and prefer `$TE.common` / `$lower.common` /
  `$upper.common`.
- Use `pooled = "common"` with
  [`metainf()`](https://rdrr.io/pkg/meta/man/metainf.html) in influence
  analyses.
- Fix fragile `class(x) != "netmeta"` check in
  [`direct.evidence.plot()`](http://dmetar.protectlab.org/reference/direct.evidence.plot.md).
- Fix `"metapropr"` typo in outlier print/summary class checks.
- Remove undocumented `%>%` usage from InfluenceAnalysis print/summary
  methods (no magrittr import).

### Documentation

- Raise Imports floors for ggplot2, meta, metafor, and netmeta.
- Drop unused `spot.outliers*` aliases from `find.outliers`
  documentation.
- Update guide links to <https://doing-meta.guide/> and repair vignette
  URL typos.
- Regenerate Rd with roxygen2 7.3.3 and rebuild the pkgdown site.
