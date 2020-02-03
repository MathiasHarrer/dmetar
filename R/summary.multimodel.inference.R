#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#' @param object An object of class \code{multimodel.inference}.
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
#' @method summary multimodel.inference



summary.multimodel.inference = function(object, ...){

  x = object

  # Print out results
  cat("\n", "Multimodel Inference: Final Results", "--------------------------", sep = "\n")
  cat("\n", "- Number of fitted models:", nrow(x$all.models))
  cat("\n", "- Full formula:", as.character(x$formula))
  cat("\n", "- Coefficient significance test:", x$type.test)
  if (x$interaction == TRUE) {
    cat("\n", "- Interactions modeled: yes")
  } else {
    cat("\n", "- Interactions modeled: no")
  }
  cat("\n", "- Evaluation criterion:", x$eval.criterion, "\n")
  cat("\n", "Best 5 Models", "--------------------------", "\n", sep = "\n")
  print(x$top5.models)
  cat("\n", "Multimodel Inference Coefficients", "--------------------------", "\n", sep = "\n")
  print(x$multimodel.coef)
  cat("\n", "Predictor Importance", "--------------------------", "\n", sep = "\n")
  print(x$predictor.importance)

  # Generate Plot
  plot(x$predictor.importance.plot)

}
