#' BDI-II and HAM-D Scores in Clinical Depression Trials
#'
#' This is a dataset containing the baseline means and standard deviations of the BDI-II and HAM-D
#' in a selection of clinical trials examining the effects of psychotherapy and antidepressants
#' in patients with Major Depression.
#'
#' @format A \code{data.frame} with six columns.
#' \describe{
#' \item{author}{\code{character}. The study label containing the first author and year of the study.}
#' \item{n}{\code{numeric}. The sample size of each study.}
#' \item{mean}{\code{numeric}. The mean depression score at baseline, when measured by the BDI-II.}
#' \item{sd}{\code{numeric}. The standard deviation of the depression score, when measured by the BDI-II.}
#' \item{mean.hamd}{\code{numeric}. The mean depression score at baseline, when measured by the HAM-D.}
#' \item{sd.hamd}{\code{numeric}. The standard deviation of the depression score, when measured by the HAM-D.}
#' }
#'
#' @details This data set was derived from an individual patient data analysis by Furukawa et al. (2020), examining the relationship
#' between the Beck Depression Inventory (BDI; Beck et al., 1961, 1996) and Hamilton Depression Rating Scale (HAM-D; Hamilton, 1960).
#' The dataset contains baseline means and standard devations of both the BDI-II and HAM-D of patients with Major Depression, as measured in
#' clinical trials examining the effects of antidepressants and/or psychotherapy.
#'
#'
#' @source Furukawa, T. A., Reijnders, M., Kishimoto, S., Sakata, M., DeRubeis, R. J., Dimidjian, S., ... & Lespérance, F. (2020).
#' Translating the BDI and BDI-II into the HAMD and vice versa with equipercentile linking. \emph{Epidemiology and Psychiatric Sciences}, 29.
#'
#' @references
#'
#' Beck, A. T., Ward, C., Mendelson, M., Mock, J., & Erbaugh, J. (1961). Beck Depression Inventory (BDI). \emph{Arch Gen Psychiatry, 4}(6), 561-571.
#'
#' Beck, A. T., Steer, R. A., & Brown, G. (1996). Beck Depression Inventory–II. \emph{Psychological Assessment}.
#'
#' Hamilton M (1960) A rating scale for depression. \emph{Journal of Neurology, Neurosurgery and Psychiatry 23}, 56–62
#'
#'
#' @usage data("BdiScores")
#'
#' @author Mathias Harrer, Toshi A. Furukawa, Pim Cuijpers
#'
"BdiScores"
