#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{eggers.test}, and \code{sucra}.
#'
#' @param object An object of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{eggers.test}, or \code{sucra}.
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#'
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method summary eggers.test


summary.eggers.test = function(object, ...){

  x = object; rm(object)
  x = data.frame(x[c("intercept", "llci", "ulci", "t", "p")])
  rownames(x) = ""

  if (x$p < 0.05) sig = TRUE
  else sig = FALSE

  # Format
  within(x, {
    intercept = round(intercept, 3)
    `95% CI` = paste(round(llci, 2), "-", round(ulci, 2))
    t = round(t, 3)
    p = format.pval(p, digits = 2, eps = 0.001)
    llci = NULL
    ulci = NULL
  }) -> x

  x = x[c("intercept", "95% CI", "t", "p")]

  cat("Eggers' test of the intercept \n")
  cat("============================= \n")
  cat("\n")

  print(x)

  cat("\n")
  if(sig == TRUE) cat("Eggers' test indicates the presence of funnel plot asymmetry. \n")
  else cat("Eggers' test does not indicate the presence of funnel plot asymmetry. \n")

}
