#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{mlm.variance.distribution}, and \code{sucra}.
#'
#'
#' @param x An object of class \code{mlm.variance.distribution}.
#' @param greyscale Should a black and white plot be generated? \code{FALSE} by default.
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
#' @method plot mlm.variance.distribution

plot.mlm.variance.distribution = function(x, greyscale = FALSE, ...){

  if (greyscale == TRUE){
    suppressWarnings(suppressMessages(plot(x$plot + scale_fill_manual(values = c("gray85", "gray90", "white", "gray85", "gray95")))))
  } else {
    suppressWarnings(plot(x$plot))
  }

}


