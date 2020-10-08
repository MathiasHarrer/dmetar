#' Association of Health and Well-being dataset
#'
#' This is a dataset containing effect size data of a meta-analysis examining the relationship
#' between health status and well-being.
#'
#' @format A \code{data.frame} with 5 columns.
#' \describe{
#' \item{author}{\code{character}. The study label containing the first author and year of the study.}
#' \item{cor}{\code{numeric}. The Pearson correlation between health and well-being.}
#' \item{n}{\code{numeric}. The sample size of the study.}
#' \item{population}{\code{character}. The type of sample in which the effects were measured.}
#' \item{country}{\code{character}. The country in which the study was conducted.}
#' }
#'
#' @details This data set is based on a meta-analysis examining the correlation between health status and subjective well-being
#' (operationalized as life satisfaction or happiness; Ngamaba et al., 2017).
#'
#' @source Ngamaba, K. H., Panagioti, M., & Armitage, C. J. (2017). How strongly related are health status and subjective well-being?
#' Systematic review and meta-analysis. \emph{The European Journal of Public Health, 27}(5), 879-885.
#'
#' @usage data("HealthWellbeing")
#'
#' @author Mathias Harrer
#'
"HealthWellbeing"
