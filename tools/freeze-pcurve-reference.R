# Rscript tools/freeze-pcurve-reference.R
# Requires stringr and poibin; real metagen fixture also requires meta.
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(poibin))
source("tools/pcurve-validation.R")
x <- pcurve_fixture()
cases <- list(
  repeated = list(x = x),
  effect = list(x = x, effect.estimation = TRUE, N = c(80, 90, 110, 70, 90, 120, 80, 100)),
  signed = list(x = transform(x, TE = TE * rep(c(-1, 1), 4))),
  signed_effect = list(x = transform(x, TE = TE * rep(c(-1, 1), 4)),
                       effect.estimation = TRUE, N = rep(100, 8)),
  reordered_duplicates = list(x = x[c(8, 2, 5, 1, 7, 3, 6, 4), ]),
  missing = list(x = transform(x, TE = replace(TE, 4, NA_real_))),
  missing_se = list(x = transform(x, seTE = replace(seTE, 4, NA_real_))),
  missing_N_row = list(x = x, effect.estimation = TRUE, N = c(NA, rep(100, 7))),
  significance_boundary = list(x = transform(x, TE = replace(TE, 4, qnorm(.975) * seTE[4]))),
  zero_se = list(x = transform(x, seTE = replace(seTE, 4, 0))),
  few_significant = list(x = x[1:2, ]),
  missing_column = list(x = x[c("TE", "seTE")]),
  missing_N = list(x = x, effect.estimation = TRUE),
  short_N = list(x = x, effect.estimation = TRUE, N = 80),
  narrow_effect_bounds = list(x = x, effect.estimation = TRUE, N = rep(100, 8), dmin = .2, dmax = .3),
  reversed_effect_bounds = list(x = x, effect.estimation = TRUE, N = rep(100, 8), dmin = .3, dmax = .2),
  invalid_N = list(x = x, effect.estimation = TRUE, N = rep(1, 8)),
  no_half_curve = list(x = transform(x, TE = 2.05 * seTE)),
  extreme = list(x = transform(x, TE = 12 * seTE))
)
for (cls in c("metagen", "metabin", "metacont", "metacor", "metainc", "meta", "metaprop")) {
  # Minimal fields consumed by pcurve; these test dispatch, not meta constructors.
  object <- c(as.list(x), list(I2 = .2))
  class(object) <- c(cls, "meta")
  cases[[paste0("class_", cls)]] <- list(x = object)
  cases[[paste0("effect_", cls)]] <- list(x = object, effect.estimation = TRUE, N = rep(100, 8))
}
cases$real_metagen <- list(x = meta::metagen(x$TE, x$seTE, studlab = x$studlab))
reference <- pcurve_reference()
results <- lapply(cases, function(args) pcurve_capture(reference, args))
saveRDS(list(revision = "89dbcac19e6e261225703f8a526b10ec6c49e21d",
             R = R.version.string, cases = cases, results = results),
        "tests/testthat/fixtures/pcurve-upstream.rds", version = 2)
