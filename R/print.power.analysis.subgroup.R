#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#' @param x An object of class \code{power.analysis.subgroup}.
#' @param ... Additional arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @import ggplot2
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method print power.analysis.subgroup


print.power.analysis.subgroup = function(x, ...){

  if (!is.na(x$Data[x$Data$Power >= 0.8, ][1, 2])) {

    cat("Minimum effect size difference needed for sufficient power: ", x$Data[x$Data$Power >= 0.8, ][1, 2],
        " (input: ", round(x$Gamma, 2),
        ")", "\n", sep = "")

  }

  if (x$Test == "two.tailed"){

    cat(paste0("Power for subgroup difference test (two-tailed): ", round(x$Power*100, 2), "%"))

  }

  if (x$Test == "one.tailed"){

    cat(paste0("Power for subgroup difference test (one-tailed): ", round(x$Power*100, 2), "%"))

  }

  plot(x$Plot)

}
