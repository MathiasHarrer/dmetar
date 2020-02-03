#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#' @param object An object of class \code{power.analysis}.
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#'
#' @import ggplot2
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method summary power.analysis

summary.power.analysis = function(object, ...){

  x = object

  if (class(x)[2] == "log"){
    cat("Power Analysis based on log-transformed OR. \n")
  }

  if (class(x)[3] == "fixed"){
    cat("Fixed-effect model used. \n")
  }

  if (class(x)[3] == "low"){
    cat("Random-effects model used (low heterogeneity assumed). \n")
  }

  if (class(x)[3] == "moderate"){
    cat("Random-effects model used (moderate heterogeneity assumed). \n")
  }

  if (class(x)[3] == "high"){
    cat("Random-effects model used (high heterogeneity assumed). \n")
  }

  plot(x$Plot)

  cat(paste0("Power: ", round(x$Power*100,2), "%"))

}
