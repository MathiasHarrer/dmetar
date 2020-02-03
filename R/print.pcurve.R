#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#' @param x An object of class \code{pcurve}.
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
#' @method print pcurve


print.pcurve = function(x, ...){

  cat("P-curve analysis", "\n", "-----------------------", "\n")
  cat("- Total number of provided studies: k =", x$kInput, "\n")
  cat("- Total number of p<0.05 studies included into the analysis: k =",
      x$kAnalyzed, paste("(", round(x$kAnalyzed/x$kInput*100, 2), "%)", sep=""), "\n")
  cat("- Total number of studies with p<0.025: k =", x$kp0.25,
      paste("(", round(x$kp0.25/x$kInput*100, 2), "%)", sep=""), "\n")
  cat("  ", "\n")
  cat("Results", "\n", "-----------------------", "\n")

  print(x$pcurveResults)
  cat("Note: p-values of 0 or 1 correspond to p<0.001 and p>0.999, respectively.")
  cat("  ", "\n")
  cat("Power Estimate: ", x$Power[,1]*100, "%", " (", x$Power[,2]*100, "%-", x$Power[,3]*100, "%)", "\n",sep="")
  cat("  ", "\n")
  cat("Evidential value", "\n", "-----------------------", "\n")
  cat("- Evidential value present:", x$EvidencePresent, "\n")
  cat("- Evidential value absent/inadequate:", x$EvidenceAbsent, "\n")

  # Additional Prints for Effect Estimation
  if (class(x)[2] == "effect.estimation"){
    cat("  ", "\n")
    cat("P-curve's estimate of the true effect size: d=", x$dEstimate, sep="")
    cat("  ", "\n")

    if (x$I2 > 0.49 & (x$class.meta.object %in% c("metagen", "metabin", "metacont", "meta", "metainc"))){
      cat("  ", "\n")
      cat("Warning: I-squared of the meta-analysis is >= 50%, so effect size estimates are not trustworthy.")
    }

  }

}
