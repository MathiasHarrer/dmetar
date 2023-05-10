#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{direct.evidence.plot}, \code{gosh.diagnostics} and \code{sucra}.
#'
#'
#' @param x An object of class \code{gosh.diagnostics}.
#' @param ... Additional arguments.
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
#' @importFrom stats as.formula hat influence ks.test optimize pbinom pchisq pf pnorm pt punif qchisq qf qnorm qt reformulate reorder setNames uniroot
#'
#' @export
#' @method print gosh.diagnostics

print.gosh.diagnostics = function(x, ...){

  cat("GOSH Diagnostics \n")
  cat("================================ \n")
  if (!is.null(x[["km.clusters"]])) cat("\n","- Number of K-means clusters detected:", x$km.clusters)
  if (!is.null(x[["db.clusters"]])) cat("\n","- Number of DBSCAN clusters detected:", x$db.clusters)
  if (!is.null(x[["gmm.clusters"]])) cat("\n","- Number of GMM clusters detected:", x$gmm.clusters)

  cat("\n")
  cat("\n", "Identification of potential outliers", "\n", "---------------------------------", "\n")

  if(!is.null(x[["outlier.studies.km"]])){
    if(length(x$outlier.studies.km) > 0){
      cat("\n", "- K-means:", paste("Study", x$outlier.studies.km, collapse = ", "))
    } else {
      cat("\n", "- K-means:", "No outliers detected.")
    }
  }

  if(!is.null(x[["outlier.studies.db"]])){
    if(length(x$outlier.studies.db) > 0){
      cat("\n", "- DBSCAN:", paste("Study", x$outlier.studies.db, collapse = ", "))
    } else {
      cat("\n", "- DBSCAN:", "No outliers detected.")
    }
  }

  if(!is.null(x[["outlier.studies.gmm"]])){
    if(length(x$outlier.studies.gmm) > 0){
      cat("\n", "- Gaussian Mixture Model:", paste("Study", x$outlier.studies.gmm, collapse = ", "))
    } else {
      cat("\n", "- Gaussian Mixture Model:", "No outliers detected.")
    }
  }

}
