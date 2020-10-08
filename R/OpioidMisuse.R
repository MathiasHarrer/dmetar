#' Prevalence of Prescription Opioid Misuse in US Adolescents and Young Adults dataset
#'
#' This is a dataset containing results of studies examining the prevalence of prescribed opioid misuse among adolescents
#' and young adults in the United States.
#'
#' @format A \code{data.frame} with 3 columns.
#' \describe{
#' \item{author}{\code{character}. The study label containing the first author and year of the study.}
#' \item{event}{\code{numeric}. The number of opioid misuse cases.}
#' \item{n}{\code{numeric}. Sample size of the study.}
#' }
#'
#' @details Studies included in this dataset were identified through a meta-analysis of Jordan et al. (2017), which examined
#' the pooled prevalence of prescription opioid misuse among US individuals 11 to 30 years of age. The dataset is meant for
#' illustration purposes, and only contains a subset of the studies included in Jordan and colleagues' study.
#' Results are therefore not identical with the actual results of this meta-analysis, and should not be interpreted as an
#' accurate representation of the evidence.
#'
#' @source Jordan, A. E., Blackburn, N. A., Des Jarlais, D. C., & Hagan, H. (2017). Past-year prevalence of prescription opioid misuse
#' among those 11 to 30 years of age in the United States: A systematic review and meta-analysis. \emph{Journal of Substance Abuse Treatment, 77}, 31-37.
#'
#' @usage data("OpioidMisuse")
#'
#' @author Mathias Harrer
#'
"OpioidMisuse"
