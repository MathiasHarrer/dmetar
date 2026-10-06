# From repository root:
# Rscript tools/benchmark-pcurve.R /absolute/scratch/results.csv
# Each method/model/size runs in a fresh R process: first call, then 3 warm calls.
# First-call timings exclude process startup and dependency/source loading.
args <- commandArgs(TRUE)
if (length(args) && args[1] == "--worker") {
  suppressPackageStartupMessages(library(stringr))
  suppressPackageStartupMessages(library(poibin))
  source("tools/pcurve-validation.R")
  source("R/pcurve2.R")
  method <- args[2]; model <- args[3]; k <- as.integer(args[4]); output <- args[5]
  reference <- pcurve_reference()
  solver <- pcurve_solver(reference)
  if (model %in% c("public", "public_effect")) {
    fun <- if (method == "before") reference else pcurve
    input <- list(x = pcurve_fixture(k))
    if (model == "public_effect") {
      input$effect.estimation <- TRUE
      input$N <- rep(c(80, 90, 110, 100), length.out = k)
    }
    run <- function() pcurve_capture(fun, input)
  } else {
    # Public inputs all become chi-square(df=1). Exercise different parameter
    # frequencies on the unchanged inner solver, not an invented public API.
    groups <- if (model == "inner_repeated") 5L else k
    id <- rep(seq_len(groups), length.out = k)
    df1 <- as.numeric(1 + id %% 3L)
    df2 <- as.numeric(20 + id)
    family <- ifelse(id %% 2L == 0L, "f", "c")
    df2[family == "c"] <- NA_real_
    # Keep df1 distinct in the less-repeated chi-square tuples too.
    if (model == "inner_distinct") df1[family == "c"] <- 1 + id[family == "c"] / k
    powers <- c(1/3, .051, (6:99)/100)
    solve <- function() {
      values <- lapply(powers, function(power) {
        if (method == "before") unname(mapply(solver, df1=df1, df2=df2, power=power, family=family))
        else .pcurve_getncps(solver, df1, df2, power, family)
      })
      values
    }
    run <- function() {
      warnings <- character()
      value <- withCallingHandlers(
        tryCatch(solve(), error = function(e) list(error = conditionMessage(e))),
        warning = function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      list(value = value, warnings = warnings)
    }
  }
  rows <- lapply(0:3, function(i) {
    gc()
    elapsed <- system.time(result <- run())[["elapsed"]]
    saveRDS(result, paste0(output, ".", i, ".rds"))
    data.frame(method, model, k, repetition = i,
               phase = if (i == 0) "first" else "warm", seconds = elapsed,
               error = if (is.list(result$value) && !is.null(result$value$error)) result$value$error else "")
  })
  write.csv(do.call(rbind, rows), output, row.names = FALSE)
} else {
  if (length(args) != 1L) stop("Provide an absolute scratch CSV path")
  output <- normalizePath(args[1], mustWork = FALSE)
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  chunks <- list()
  for (model in c("public", "public_effect", "inner_repeated", "inner_distinct")) {
    for (k in c(50L, 200L, 1000L)) {
      files <- character()
      for (method in c("before", "after")) {
        file <- paste0(output, ".", model, ".", k, ".", method, ".csv")
        files <- c(files, file)
        status <- system2(file.path(R.home("bin"), "Rscript"),
                          c("tools/benchmark-pcurve.R", "--worker", method, model, k, shQuote(file)))
        if (status != 0) stop("Worker failed: ", model, "/", method, "/", k)
        chunks[[length(chunks) + 1L]] <- read.csv(file, colClasses = c(error = "character"),
                                               na.strings = character())
      }
      for (i in 0:3) {
        before <- readRDS(paste0(files[1], ".", i, ".rds"))
        after <- readRDS(paste0(files[2], ".", i, ".rds"))
        if (!identical(before, after)) stop("Output mismatch: ", model, "/", k)
      }
      cat("Equivalent:", model, k, "\n")
      write.csv(do.call(rbind, chunks), output, row.names = FALSE)
    }
  }
  writeLines(capture.output(sessionInfo()), paste0(output, ".session.txt"))
}
