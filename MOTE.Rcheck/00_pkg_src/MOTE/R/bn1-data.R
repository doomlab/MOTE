#' Between-Subjects One-Way ANOVA Example Data
#'
#' @description
#' Ratings of close interpersonal attachments for 45-year-old participants,
#' categorized by self-reported health status: excellent, fair, or poor.
#' This dataset is designed for use with functions such as
#' \code{\link{eta.F}}, \code{\link{eta.full.SS}},
#' \code{\link{omega.F}}, \code{\link{omega.full.SS}},
#' and \code{\link{epsilon.full.SS}}.
#'
#' @docType data
#'
#' @usage data(bn1_data)
#'
#' @format A data frame with *n* rows and 2 variables:
#' \describe{
#'   \item{group}{Factor with levels
#'      \code{"poor"}, \code{"fair"}, and \code{"excellent"}.}
#'   \item{friends}{Numeric rating of close interpersonal attachments.}
#' }
#'
#' @keywords datasets
#'
#' @references
#' Nolan, S. A., & Heinzen, T. E. (*4th ed.*).
#' *Statistics for the Behavioral Sciences*.
#' Macmillan Learning.
#' @source
#' Simulated data inspired by Nolan & Heinzen (4th ed.),
#' *Statistics for the Behavioral Sciences*. Generated for instructional
#' examples in the MOTE package.
"bn1_data"
