context("find.outliers with current meta")

library(meta)

test_that("find.outliers returns expected class with meta 6+", {
  skip_if_not_installed("meta", "6.0.0")
  data("ThirdWave", package = "dmetar")
  m <- metagen(TE, seTE, studlab = Author, data = ThirdWave,
               common = TRUE, random = TRUE)
  fo <- find.outliers(m)
  expect_s3_class(fo, "find.outliers")
  expect_true(inherits(fo$m.fixed, "meta") || inherits(fo$m.random, "meta") ||
                !is.null(fo$out.study.fixed) || !is.null(fo$out.study.random))
})
