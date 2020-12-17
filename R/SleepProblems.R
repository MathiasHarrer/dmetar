#' Factor Structure of Sleep Complaints data set
#'
#' This is a fictitious data set containing correlation matrices of five sleep-related variables.
#'
#' @format A \code{list} with 2 elements.
#' \describe{
#' \item{data}{\code{list}. Contains correlation matrices of k=11 studies. Each correlation matrix
#' has the same structure, and contains the variables "sleep quality", "sleep latency", "sleep efficiency", "daytime dysfunction" and "hypersomnia".}
#' \item{n}{\code{numeric}. Sample size of each study.}
#' }
#'
#' @details This simulated data set can by used to fit a meta-analytic structural equation model using \code{\link[metaSEM]{tssem1}}. It is assumed that
#' "sleep quality", "sleep latency" and "sleep efficiency" load on the latent factor "insomnia", while "daytime dysfunction" and "hypersomnia" measure the
#' latent factor "lassitude".
#'
#' @source Fictitious data set. Data were simulated using the \code{\link[lavaan]{simulateData}} function in \code{lavaan}.
#'
#' @usage data("SleepProblems")
#'
#' @author Mathias Harrer
#'
"SleepProblems"
