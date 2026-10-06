# Run from the repository root. Reference source is read, never modified.
# DMETAR_BASELINE_SOURCE may point to an already frozen R/pcurve2.R.
pcurve_reference <- function() {
  path <- Sys.getenv("DMETAR_BASELINE_SOURCE")
  env <- new.env(parent = globalenv())
  if (nzchar(path)) {
    sys.source(path, env)
  } else {
    text <- system2("git", c("show", "89dbcac19e6e261225703f8a526b10ec6c49e21d:R/pcurve2.R"), stdout = TRUE)
    if (!is.null(attr(text, "status"))) stop("Cannot read frozen upstream source")
    eval(parse(text = text), env)
  }
  env$pcurve
}

# Preserve baseline options/graphics side effects inside this harness only.
pcurve_capture <- function(fun, args) {
  old <- options()
  on.exit(options(old), add = TRUE)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(do.call(fun, args), error = function(e) list(error = conditionMessage(e))),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, warnings = warnings)
}

pcurve_fixture <- function(k = 8L) {
  set.seed(103)
  z <- rep(c(2.05, 2.4, 3.1, 1.2, 2.4, 3.5, 2.05, 2.8), length.out = k)
  se <- runif(k, .15, .3)
  data.frame(TE = z * se, seTE = se, studlab = paste0("study-", seq_len(k)))
}

# Evaluate the unchanged nested solvers independently of the public z-input
# conversion, to exercise the retained F and chi-square branches.
pcurve_solver <- function(reference) {
  env <- new.env(parent = environment(reference))
  for (expr in as.list(body(reference))[-1L]) {
    if (is.call(expr) && as.character(expr[[1L]]) %in% c("=", "<-") &&
        is.symbol(expr[[2L]]) && as.character(expr[[2L]]) %in%
        c("getncp.f", "getncp.c", "getncp")) eval(expr, env)
  }
  env$getncp
}
