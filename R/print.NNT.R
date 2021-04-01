#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{eggers.test}, \code{NNT}, and \code{sucra}.
#'
#' @param x An object of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{eggers.test}, \code{NNT}, or \code{sucra}.
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
#' @method print NNT


print.NNT = function(x, ...){

  if (class(x)[3] == "numeric"){
    has.negs = sum(as.numeric(x) < 0) > 0
  } else {
    has.negs = sum(as.numeric(x$NNT) < 0) > 0
  }

  if (class(x)[2] == "kk"){
    cat("Kraemer & Kupfer method used. \n")
    if (has.negs == TRUE){
      cat("Negative NNT values refer to the number needed to harm (NNH) \n")
    }
  }

  if (class(x)[2] == "fl"){
    cat("Furukawa & Leucht method used. \n")
    if (has.negs == TRUE){
      cat("Negative NNT values refer to the number needed to harm (NNH) \n")
    }
  }

  if (class(x)[2] == "raw"){
    if (has.negs == TRUE){
      cat("Negative NNT values refer to the number needed to harm (NNH) \n")
    }
  }

  if (class(x)[2] == "unspecified"){
    cat("NNTs were calculated from raw data, so neither Kraemer & Kupfer nor Furukawa method was used. \n")
    if (has.negs == TRUE){
      cat("Negative NNT values refer to the number needed to harm (NNH) \n")
    }
  }

  class(x) = class(x)[3]
  print(x)

}
