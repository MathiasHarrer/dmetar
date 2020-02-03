#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#' @param x An object of class \code{direct.evidence.plot}.
#' @param ... Other arguments.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#' When both \code{plot} and \code{forest} are available for a class, outputs of both functions are identical (i.e., a forest
#' plot is returned).
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @importFrom grid grid.newpage grid.draw
#' @importFrom gridExtra grid.arrange
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @method plot direct.evidence.plot
#' @export



plot.direct.evidence.plot = function(x, ...){

  grid::grid.newpage()
  grid.draw(x$plot, ...)

}
