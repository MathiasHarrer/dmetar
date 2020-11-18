#' Differential Effects of Cognitive Behavioral Therapy Delivery Formats in Depressive Patients dataset
#'
#' This data set contains simulated effect sizes of randomized controlled trials evaluating the (relative) effectiveness of
#' cognitive behavioral therapy for depression, using different delivery formats.
#'
#' @format A \code{list} with 3 elements.
#' \describe{
#' \item{data}{\code{data.frame}. The data frame containing the network meta-analysis data.
#' See the documentation of the \code{TherapyFormats} data set for more information.}
#' \item{study.info}{\code{data.frame}. Contains information on the risk of bias of the included studies.
#' Can be used for network meta-regression.}
#' \item{treat.codes}{\code{data.frame}. Contains the long-form treatment names associated with each label.}
#' }
#'
#' @details This simulated data set is based on pooled results of a network
#' meta-analysis examining the effects of different cognitive behavioral therapy
#' (CBT) delivery formats in adults with depression (Cuijpers et al., 2019). Using
#' network meta-analysis techniques on this data set will yield roughly similar,
#' although not identical results to the ones reported in the original study.
#'
#' For the simulation, pooled effects within designs as reported in Cuijpers et al. along with the estimated
#' between-study heterogeneity variance were assumed as true population parameters.
#' Based on these "true" population parameters, simulations
#' were generated assuming a (random-effects) bayesian hierarchical model, using a
#' non-central chi-squared distribution for the simulation of study-specific sampling errors.
#' The number of effect size draws was chosen to (approximately)
#' mimic the number of comparisons in the original analysis.
#'
#' This is an alternative version of the data set, adapted for out-of-the-box usage in \code{gemtc}.
#'
#'
#' @source Cuijpers, P., Noma, H., Karyotaki, E., Cipriani, A., & Furukawa, T. A. (2019).
#' Effectiveness and Acceptability of Cognitive Behavior Therapy Delivery Formats in Adults with Depression: A Network Meta-analysis.
#' \emph{JAMA Psychiatry, 76}(7), 700-707.
#'
#' @usage data("TherapyFormatsGeMTC")
#'
#' @author Mathias Harrer
#'
"TherapyFormatsGeMTC"
