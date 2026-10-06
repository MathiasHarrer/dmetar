# Exact noncentrality solve deduplication

Baseline: `89dbcac19e6e261225703f8a526b10ec6c49e21d` (upstream master).
Validation: 2026-10-06, Ubuntu 24.04.4 LTS, x86_64, R 4.6.1 (2026-06-24).

`ncp33` and each scalar `powerfit()` evaluation previously solved the same
noncentrality equation once per effect row. The helper solves once per exact
`(family, df1, df2)` tuple at that power, then expands the roots in original row
order. `%a` hexadecimal double representations avoid rounding collisions;
missing `df2` values remain in the keys. First-occurrence order preserves the
first genuine numerical error. Each invocation is independent.

The nested solvers, `uniroot(c(0, 1000))` interval and default tolerance are
unchanged. No changes to significance eligibility, hypotheses, effect
estimation, signs, result structure, plotting or process option side effects.
A diagnostic warning from a repeated solve is emitted once per distinct tuple;
warning multiplicity is deliberately reduced along with redundant work. The
public fixtures and measured valid inputs have unchanged warning output.

Public inputs currently become z statistics, then chi-square with `df1=1` and
`df2=NA`; thus all rows share a tuple. The mixed F/chi-square measurements below
exercise the internal solver, not an additional public input format.

## Equivalence and package checks

The installed candidate passed **54 assertions**, with no failures, warnings or
skips (51 p-curve assertions plus the existing three NNT assertions).
`tests/testthat/fixtures/pcurve-upstream.rds` contains complete frozen results from the
unchanged upstream source for 34 tiny deterministic cases (26 successes and
8 existing errors). Tests require `identical()` results, including all numeric
and statistical outputs, names, row order, classes and captured warning/error
messages. Cases cover repeated and reordered rows, positive/mixed signs,
effect estimation on/off, significance boundaries, extreme statistics,
missing TE/seTE/N, invalid N, effect bounds, data frames, all seven accepted
meta class dispatches, and a real `meta::metagen()` object. Minimal meta objects
test the fields consumed by pcurve, not each meta constructor. No imputation.

Direct tests evaluate the package's actual nested F/chi-square solvers:
repeated/mixed parameters, adjacent representable doubles, missing/NaN values,
failing degrees of freedom/powers, first-error preservation, solve counts,
warning counts and absence of reuse across invocations. Full results for all
benchmark runs also require exact identity against the frozen source.

Both baseline and candidate were built and installed in scratch locations.
Both `R CMD check --no-manual --no-build-vignettes`, with
`_R_CHECK_FORCE_SUGGESTS_=false`, report **1 ERROR, 4 WARNINGs, 2 NOTEs**.
Check logs differ only in their output path and timestamp; tests pass in both.
This is not a clean full check:

- ERROR: the existing `mlm.variance.distribution`/`var.comp` plotting example
  fails in ggplot2 `annotate()` (`x$label` is a call rather than a vector).
- WARNING: existing `makeArgs`/`makeArgs.rma` S3 signature mismatch.
- Three vignette warnings: missing built `inst/doc`, missing vignette outputs,
  and execution requires unavailable `devtools`.
- Two documentation notes: existing Rd braces and S3 usage markup.
- `gemtc`, `robvis`, `devtools` and some documentation cross-reference packages
  are unavailable. Manual/PDF and vignette rebuilding were not checked.

Upstream has no GitHub Actions workflows at validation time. Unrelated defects
and documentation were left outside this patch.

## Timing method

`benchmark-pcurve.R` sources the exact baseline revision and candidate, using
the same R and dependencies. Each method/model/size has a fresh R process, one
first call and three warm repetitions; elapsed time excludes process startup,
dependency loading and source loading. GC precedes each timed run. Public
fixtures use seed 103, varied effects/SEs, repeated statistics and some
non-significant rows. Effect estimation uses identical sample sizes. These are
synthetic valid fixtures, not an empirical corpus or a universal speed promise.

Internal timings evaluate the unchanged solver across 96 scalar powers
(`1/3`, `.051`, `.06` through `.99`). `inner_repeated` uses five exact tuples;
`inner_distinct` uses k distinct tuples, combining F and chi-square families.
These isolate the benefit and overhead of deduplication; they are not full
p-curve timings. Genuine errors are recorded per method/model in the CSV;
all measured cases succeeded. Returned objects and warnings are checked for
exact identity for every before/after repetition. Compilation and package
checks completed before the final measurements.

| Workload | k | Before first (s) | After first (s) | Before warm median (s) | After warm median (s) |
| --- | ---: | ---: | ---: | ---: | ---: |
| Public, effect estimation off | 50 | 1.032 | 0.508 | 1.000 | 0.075 |
| Public, effect estimation off | 200 | 3.709 | 0.642 | 3.522 | 0.202 |
| Public, effect estimation off | 1000 | 18.404 | 1.527 | 18.464 | 1.086 |
| Public, effect estimation on | 50 | 1.110 | 0.618 | 1.049 | 0.118 |
| Public, effect estimation on | 200 | 3.952 | 0.752 | 3.714 | 0.305 |
| Public, effect estimation on | 1000 | 19.453 | 1.790 | 19.095 | 1.314 |
| Internal, five tuples | 50 | 0.637 | 0.085 | 0.631 | 0.072 |
| Internal, five tuples | 200 | 2.384 | 0.097 | 2.447 | 0.083 |
| Internal, five tuples | 1000 | 12.901 | 0.175 | 12.972 | 0.154 |
| Internal, k distinct tuples | 50 | 0.605 | 0.647 | 0.606 | 0.618 |
| Internal, k distinct tuples | 200 | 2.401 | 2.453 | 2.471 | 2.474 |
| Internal, k distinct tuples | 1000 | 12.310 | 12.320 | 12.892 | 13.091 |

For k=1000 with all distinct internal tuples, warm medians were 12.892 → 13.091 s; there is no solve-count reduction in that case.

The raw first/warm observations are in `pcurve-timings.csv`. Timings include
unchanged plotting and deprecated sensitivity calculations in public cases.
Other workloads, hardware and package versions may behave differently.

Dependencies: stringr 1.6.0, poibin 1.6 (source benchmarks); meta 8.5.0,
testthat 3.3.2, netmeta 3.7.0, MuMIn 1.48.19, igraph 2.3.4, metafor 4.4.0,
ggplot2 4.0.3, fpc 2.2.11, mclust 6.0.1 (package validation). Missing dependencies
were added only to a scratch library; compatible shared libraries were reused.
Raw logs, source snapshots, build/check artifacts and per-run RDS files remain
outside the checkout.

## Reproduce

From the repository root, with package dependencies available (no need to
regenerate the committed frozen fixture for ordinary tests):

```sh
repo="$PWD"
scratch=$(mktemp -d)
mkdir "$scratch/library"
export R_PROFILE_USER=/dev/null
export R_LIBS="$scratch/library${R_LIBS:+:$R_LIBS}"
R CMD INSTALL --library="$scratch/library" "$repo"
Rscript -e 'library(dmetar); testthat::test_dir("tests/testthat", stop_on_failure=TRUE)'
Rscript tools/benchmark-pcurve.R "$scratch/timings.csv"
(cd "$scratch" && R CMD build --no-build-vignettes --no-manual "$repo")
(cd "$scratch" && _R_CHECK_FORCE_SUGGESTS_=false R CMD check \
  --no-manual --no-build-vignettes dmetar_0.1.0.tar.gz)
```

Set `R_LIBS` to include existing dependency libraries before those commands.
Reference source comes from `git show 89dbcac19e6e261225703f8a526b10ec6c49e21d:R/pcurve2.R`;
in an archive/shallow clone without that revision, set `DMETAR_BASELINE_SOURCE`
to a read-only copy of that exact file. To deliberately regenerate the frozen
fixture with the same dependencies, run `Rscript tools/freeze-pcurve-reference.R`.
To repeat the baseline package check, archive the same revision into a separate
scratch directory and build/check it with the identical flags and libraries.

A read-only Claude Opus 5.5 review found no blocking scientific/code issues.
Review used the Max subscription; incremental metered API spend was US$0
(CLI reported US$0.3075002 in usage-equivalent cost, not an API charge).
