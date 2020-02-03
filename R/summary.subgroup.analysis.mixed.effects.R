#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#' @param object An object of class \code{subgroup.analysis.mixed.effects}.
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method summary subgroup.analysis.mixed.effects

summary.subgroup.analysis.mixed.effects = function(object, ...){

  x = object

  cat("Subgroup Results:", "--------------", sep = "\n")
  print(x$within.subgroup.results)
  cat("", "Test for subgroup differences (mixed/fixed-effects (plural) model):",
      "--------------", sep = "\n")
  print(x$subgroup.analysis.results)
  cat("", sep = "\n")
  cat("- Total number of studies included in subgroup analysis: ", x$k)
  cat("", sep = "\n")
  cat("- Tau estimator used for within-group pooling: ", x$method.tau)

}
