#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#'
#'
#' @param x An object of class \code{subgroup.analysis.mixed.effects}.
#' @param ... Other arguments of the \code{meta.forest} or \code{metafor}'s code{\link[metafor]{forest}}
#' function. Can be used for styling the generated forest plot.
#'
#' @details
#' A total of four package-specific S3 methods are provided in \code{dmetar}: S3 methods for \code{print},
#' \code{summary}, \code{plot} and \code{forest}. Outputs from \code{print} and \code{summary} are always identical.
#' When both \code{plot} and \code{forest} are available for a class, outputs of both functions are identical (i.e., a forest
#' plot is returned). The \code{forest}/\code{plot} function allows additional arguments of the \code{meta.forest} or
#' \code{metafor}'s code{\link[metafor]{forest}} function (depending on the class of the meta-analysis object on which prior
#' calculations are based on). These can be used for further styling of the forest plot.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#'
#' @importFrom meta forest.meta
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method forest subgroup.analysis.mixed.effects


forest.subgroup.analysis.mixed.effects = function(x, ...){

  meta::forest.meta(x$m.random, text.random = "Fixed effects (plural) model",
                    text.random.w = "Random effects model",
                    print.byvar = FALSE,
                    leftcols = c("studlab"),
                    rightcols = c("effect", "ci"),
                    leftlabs = "Subgroup",
                    print.I2.ci = TRUE,
                    print.Q.subgroup = FALSE,
                    print.Q = TRUE,
                    print.tau2 = FALSE,
                    resid.hetstat = FALSE,
                    colgap.forest.left = "14mm",
                    hetlab="",
                    col.by = "black",
                    ...)

}


