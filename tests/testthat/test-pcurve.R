test_that("complete pcurve outputs and failures match frozen upstream", {
  frozen <- readRDS(test_path("fixtures", "pcurve-upstream.rds"))
  # Harness lives outside the package API and is also used for reproduction.
  capture <- function(args) {
    old <- options()
    on.exit(options(old), add = TRUE)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    warnings <- character()
    value <- withCallingHandlers(
      tryCatch(do.call(pcurve, args), error = function(e) list(error = conditionMessage(e))),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(value = value, warnings = warnings)
  }
  for (name in names(frozen$cases)) {
    expect_identical(capture(frozen$cases[[name]]), frozen$results[[name]], info = name)
  }
})

test_that("only exact distribution tuples share a solve, in original order", {
  calls <- list()
  solver <- function(family, df1, df2, power) {
    calls[[length(calls) + 1L]] <<- list(family, df1, df2, power)
    length(calls)
  }
  df1 <- c(1, 2, 1, 1 + .Machine$double.eps, 1, 1, 1)
  df2 <- c(NA, 30, NA, NA, 31, NaN, NA)
  family <- c("c", "f", "c", "c", "f", "c", "f")
  result <- dmetar:::.pcurve_getncps(solver, df1, df2, 1/3, family)
  expect_identical(result, c(1L, 2L, 1L, 3L, 4L, 5L, 6L))
  expect_length(calls, 6L)
  expect_identical(calls[[3]][[2]], 1 + .Machine$double.eps)
  # No cache survives into another power evaluation or another invocation.
  dmetar:::.pcurve_getncps(solver, df1, df2, .5, family)
  expect_length(calls, 12L)
})

test_that("deduplication preserves actual roots and genuine solver errors", {
  # Evaluate the package's actual nested solvers without running an analysis.
  env <- new.env(parent = environment(pcurve))
  for (expr in as.list(body(pcurve))[-1L]) {
    if (is.call(expr) && as.character(expr[[1L]]) %in% c("=", "<-") &&
        is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in%
        c("getncp.f", "getncp.c", "getncp")) eval(expr, env)
  }
  solver <- env$getncp
  df1 <- c(1, 2, 1, 1 + .Machine$double.eps, 3, 2)
  df2 <- c(NA, 30, NA, NA, 60, 30)
  family <- c("c", "f", "c", "c", "f", "f")
  for (power in c(.051, 1/3, .5, .99)) {
    expect_identical(dmetar:::.pcurve_getncps(solver, df1, df2, power, family),
                     unname(mapply(solver, df1=df1, df2=df2, power=power, family=family)))
  }
  error <- function(expr) suppressWarnings(tryCatch(expr, error = conditionMessage))
  for (bad in c(NA_real_, NaN, -1)) {
    expect_identical(error(dmetar:::.pcurve_getncps(solver, c(1, bad, 1),
                                                 rep(NA_real_, 3), .5, rep("c", 3))),
                     error(mapply(solver, df1=c(1, bad, 1), df2=rep(NA_real_, 3),
                                  power=.5, family=rep("c", 3))))
  }
  for (power in c(0, .01, 1, NA_real_)) {
    expect_identical(error(dmetar:::.pcurve_getncps(solver, 1, NA_real_, power, "c")),
                     error(mapply(solver, df1=1, df2=NA_real_, power=power, family="c")))
  }
})

test_that("diagnostics are emitted once per distinct solve", {
  warnings <- character()
  result <- withCallingHandlers(
    dmetar:::.pcurve_getncps(function(family, df1, df2, power) {
      warning("solver diagnostic")
      df1
    }, c(1, 1, 2, 1), rep(NA_real_, 4), .5, rep("c", 4)),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(result, c(1, 1, 2, 1))
  expect_identical(warnings, rep("solver diagnostic", 2))
})
