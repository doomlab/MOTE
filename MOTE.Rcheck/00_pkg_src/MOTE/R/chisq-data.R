#' Chi-Square Test Example Data
#'
#' @description
#' Example data for a chi-square test of independence. Individuals were
#' polled and asked to report their number of friends (low, medium, high)
#' and their number of children (1, 2, 3 or more). The analysis examines
#' whether there is an association between friend group size and number of
#' children. It was hypothesized that those with more children may have
#' less time for friendship-maintaining activities.
#'
#' @docType data
#'
#' @usage data(chisq_data)
#'
#' @format A data frame with 2 variables:
#' \describe{
#'   \item{friends}{Factor with levels \code{"low"}, \code{"medium"},
#' and \code{"high"} indicating self-reported number of friends.}
#'   \item{kids}{Factor with levels \code{"1"}, \code{"2"}, and \code{"3+"}
#' indicating number of children.}
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
"chisq_data"
