#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, \code{eggers.test}, \code{gosh.diagnostics}, and \code{sucra}.
#'
#' @param x An object of class \code{gosh.diagnostics}.
#' @param which Type of plot to display. Can be \code{"all"} (all plots), \code{"cluster"}
#' (cluster plots only) or \code{"outlier"} (outlier plots only, if available).
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
#' @importFrom grid grid.draw grid.newpage
#'
#' @export
#' @method plot gosh.diagnostics

plot.gosh.diagnostics = function(x, which = c("all", "cluster", "outlier"), ...){

  which = which[1]
  clusterplots = names(x)[grepl("\\.plot", names(x))]
  outlierplots = names(x)[grepl("\\.removed", names(x))]

  if (which == "all"){
    for (i in 1:length(clusterplots)){
      grid::grid.newpage()
      grid::grid.draw(x[[clusterplots[i]]])
    }
    if (length(outlierplots) > 0){
      for (i in 1:length(outlierplots)){
        grid::grid.newpage()
        grid::grid.draw(x[[outlierplots[i]]])
      }
    }
  }

  if (which == "cluster"){
    for (i in 1:length(clusterplots)){
      grid::grid.newpage()
      grid::grid.draw(x[[clusterplots[i]]])
    }
  }

  if (which == "outlier"){
    if (length(outlierplots) > 0){
      for (i in 1:length(outlierplots)){
        grid::grid.newpage()
        grid::grid.draw(x[[outlierplots[i]]])}
      } else {
        message("No outliers detected.")
      }
  }

}

