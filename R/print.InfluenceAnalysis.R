#' Print, summary and plot methods for objects created using 'dmetar' functions
#'
#' Print, summary and plot S3 methods for objects of class \code{direct.evidence.plot}, \code{find.outliers},
#' \code{influence.analysis}, \code{multimodel.inference}, \code{pcurve}, \code{power.analysis},
#' \code{subgroup.analysis.mixed.effects}, and \code{sucra}.
#'
#' @param x An object of class \code{InfluenceAnalysis}.
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
#' @method print InfluenceAnalysis
#'
#'



print.InfluenceAnalysis = function(x, ...){

  cat("Leave-One-Out Analysis (Sorted by I2)", "\n",
      "-----------------------------------", "\n")
  loo.data = x$Data[,1:5]
  rownames(loo.data) = loo.data$Author
  loo.data$Author = NULL
  colnames(loo.data) = c("Effect", "LLCI", "ULCI", "I2")
  loo.data = round(loo.data, 3)
  loo.data = loo.data[order(loo.data$I2),]
  print(loo.data)


  cat("\n")
  cat("\n")
  cat("Influence Diagnostics", "\n",
      "-------------------", "\n")
  infl.data = data.frame(x$Data[,c(6:9,11:13)] %>% round(3),
                         "infl" = x$Data$inf)
  rownames(infl.data) = x$Data$Author
  print(infl.data)


  cat("\n")
  cat("\n")
  cat("Baujat Diagnostics (sorted by Heterogeneity Contribution)", "\n",
      "-------------------------------------------------------", "\n")
  baujat.data = data.frame(x$Data[,16:17] %>% round(3))
  rownames(baujat.data) = x$Data$Author
  baujat.data = baujat.data[order(-baujat.data$HetContrib),]
  print(baujat.data)

}
