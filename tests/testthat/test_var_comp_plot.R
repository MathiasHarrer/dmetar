context("var.comp plot compatibility")

library(metafor)
library(ggplot2)

test_that("var.comp plot builds under current ggplot2", {
  skip_if_not_installed("ggplot2", "3.4.0")
  data("Chernobyl", package = "dmetar")
  m <- rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
  res <- var.comp(m)
  expect_s3_class(res, "mlm.variance.distribution")
  expect_true(is.data.frame(res$results))
  built <- ggplot_build(plot(res))
  expect_true(length(built$data) >= 1)
})
