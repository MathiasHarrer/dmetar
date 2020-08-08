#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#' @param x An object of class \code{sucra}.
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
#' @method plot sucra

plot.sucra = function(x, ...){

  class(x) = "data.frame"
  res = x
  res$Treatment = rownames(res)
  res$Treatment = factor(res$Treatment,
                         levels = res$Treatment[rev(order(res$SUCRA))])

  ggplot2::ggplot(res, aes(x=Treatment, y=SUCRA)) +
    geom_bar(stat="identity") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, color="black"),
          axis.text.y = element_text(color="black")) +
    ylab("SUCRA") +
    xlab("Treatment")

}
