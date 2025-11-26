#' Between-Subjects Two-Way ANOVA Example Data
#'
#' @description
#' Example data for a between-subjects two-way ANOVA examining whether
#' athletic spending differs by sport type and coach experience.
#' This dataset contains simulated athletic budgets (in thousands of dollars)
#' for baseball, basketball, football, soccer, and volleyball teams,
#' with either a new or old coach. Designed for use with
#' \code{\link{omega.partial.SS.bn}}, \code{\link{eta.partial.SS}}, and
#' other between-subjects ANOVA designs.
#'
#' @docType data
#'
#' @usage data(bn2_data)
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{coach}{Factor with levels \code{"old"} and \code{"new"} indicating coach experience.}
#'   \item{type}{Factor indicating sport type: \code{"baseball"}, \code{"basketball"}, \code{"football"}, \code{"soccer"}, or \code{"volleyball"}.}
#'   \item{money}{Numeric. Athletic spending in thousands of dollars.}
#' }
#'
#' @keywords datasets
"bn2_data"
