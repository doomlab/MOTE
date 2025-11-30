#' Independent-Samples t-Test Example Data
#'
#' @description
#' Example data for an independent-samples t-test examining whether a
#' hypnotism intervention affects recall accuracy after witnessing a crime.
#' Designed for use with functions such as \code{\link{d_ind_t}},
#' \code{\link{d_ind_t_t}}, and \code{\link{delta_ind_t}}.
#'
#' @docType data
#'
#' @usage data(indt_data)
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{correctq}{Numeric recall score/accuracy.}
#'   \item{group}{Factor indicating condition with levels
#' \code{"control"} and \code{"hypnotism"}.}
#' }
#'
#' @keywords datasets
"indt_data"
