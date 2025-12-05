#' Repeated Measures Two-Way ANOVA Example Data
#'
#' @description
#' Example data for a mixed repeated-measures two-way ANOVA examining the
#' effect of instruction type and forward/backward strength in word
#' associations. Designed for use with \code{\link{omega.partial.SS.rm}}
#' and other repeated measures ANOVA designs.
#'
#' The dataset contains a between-subjects variable for instruction type,
#' a subject identifier, and four repeated-measures conditions:
#' - FSG (forward strength): e.g., "cheddar" → "cheese"
#' - BSG (backward strength): e.g., "cheese" → "cheddar"
#' Forward and backward strength were manipulated to measure overestimation of
#' association strength.
#'
#' @docType data
#'
#' @usage data(rm2_data)
#'
#' @format A data frame with 6 variables:
#' \describe{
#'   \item{group}{Factor. Between-subjects variable indicating the
#' type of instructions given.}
#'   \item{subject}{Integer or factor. Subject identifier.}
#'   \item{fsglobsglo}{Numeric. Low FSG, low BSG condition.}
#'   \item{fsghibsglo}{Numeric. High FSG, low BSG condition.}
#'   \item{fsglobsghi}{Numeric. Low FSG, high BSG condition.}
#'   \item{fsghibsghi}{Numeric. High FSG, high BSG condition.}
#' }
#'
#' @keywords datasets
#'
"rm2_data"
