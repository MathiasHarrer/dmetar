#' Ionizing Radiation from Chernobyl and Mutation Rates in Humans dataset
#'
#' This is a dataset containing correlations between exposure to ionized radiation due to the 1986 Chernobyl reactor disaster and
#' mutation rates in humans.
#'
#' @format A \code{data.frame} with 8 columns.
#' \describe{
#' \item{author}{\code{character}. The study label containing the author and year of the study.}
#' \item{cor}{\code{numeric}. Untransformed Pearson correlation between radiation exposure and mutation rates.}
#' \item{n}{\code{numeric}. Sample size.}
#' \item{z}{\code{numeric}. Fisher-\eqn{z} transformed correlations.}
#' \item{se.z}{\code{numeric}. Standard error of the Fisher-\eqn{z} transformed correlations.}
#' \item{var.z}{\code{numeric}. Variance of the Fisher-\eqn{z} transformed correlations.}
#' \item{radiation}{\code{character}. Overall amount of exposure in the study sample.}
#' \item{es.id}{\code{character}. Variable representing study membership.}
#' }
#'
#' @details This data set is based on studies assessing mutations in humans exposed to radioactive fallout of the Chernobyl reactor
#' incident, as identified in Møller & Mousseau (2015). Several studies report more than one estimate of the association between
#' radiation exposure and mutation rate, which generates a nested data structure (effect sizes are nested in studies). A variable
#' \code{es.id} was added to the data set, coding the clustered data structure. The dataset contains the untransformed
#' correlation (\code{cor}) and sample size, as well as the Fisher-\eqn{z} transformed correlation, along with their standard error
#' and variance. The \eqn{z} and variance value may be used in three-level meta-analysis models implemented via the \code{\link[metafor]{rma.mv}} function
#' in the \code{metafor} package.
#'
#' Please note that the values in this dataset are not exactly identical to the ones in Møller & Mousseau (2015), and results may therefore
#' differ to the ones the reported in the paper.
#'
#'
#' @source Møller, A. P., & Mousseau, T. A. (2015). Strong Effects of Ionizing Radiation from Chernobyl on Mutation Rates. \emph{Scientific Reports}, 5, 8363.
#'
#' @usage data("Chernobyl")
#'
#' @author Mathias Harrer
"Chernobyl"
