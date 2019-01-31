#' Mixed Two-way ANOVA Example Data
#'
#' Dataset for use in \code{\link{ges.partial.SS.mix}}.
#' Given previous research, we know that backward strength
#' in free association tends to increase the ratings
#' participants give when you ask them how many people
#' out of 100 would say a word in response to a target
#' word (like Family Feud). This result is tied to
#' people's overestimation of how well they think they
#' know something, which is bad for studying. So, we
#' gave people instructions on how to ignore the BSG.
#' Did it help? Is there an interaction between BSG
#' and instructions given?
#'
#' @docType data
#'
#' @usage data(mix2_data)
#'
#' @format A data frame including group type and backward strength rating.
#'
#' group: Regular JAM Task or Debiasing JAM task
#' bsglo: estimate of response to target word in a Low BSG condition
#' bsghi: estimate of response to target word in a High BSG condition
#'
#' @keywords datasets
#'
#'
"mix2_data"
