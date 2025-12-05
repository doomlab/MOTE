#' Mixed Two-Way ANOVA Example Data
#'
#' @description
#' Example data for a mixed two-way ANOVA examining whether instructions to
#' ignore backward strength in free association (BSG) reduce overestimation in
#' response probabilities. Participants provided estimates of how many out of
#' 100 people would say a given response to a target word ("Family Feud"-style),
#' under low vs. high BSG conditions, after receiving either regular or
#' debiasing instructions.
#'
#' @docType data
#'
#' @usage data(mix2_data)
#'
#' @format A data frame with 3 variables:
#' \describe{
#'   \item{group}{Factor indicating instruction condition with levels
#'     \code{"Regular JAM Task"} and \code{"Debiasing JAM task"}.}
#'   \item{bsglo}{Numeric. Estimated response percentage in the Low
#' BSG condition.}
#'   \item{bsghi}{Numeric. Estimated response percentage in the High
#' BSG condition.}
#' }
#'
#' @keywords datasets
"mix2_data"
