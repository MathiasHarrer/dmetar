context("NNT checks")
library(dmetar)

test_that("Kraemer Kupfer method is correct", {
  d = c(NNT(0.3), NNT(0.5), NNT(0.8))
  expect_equal(round(d[1],3), round(5.952524,3))
  expect_equal(round(d[2],3), round(3.618909,3))
  expect_equal(round(d[3],3), round(2.334309,3))
})
