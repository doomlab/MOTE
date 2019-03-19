#' Repeated Measures Two-way ANOVA Example Data
#'
#' Dataset for use in \code{\link{omega.partial.SS.rm}} and other
#' repeated measures ANOVA designs. This dataset includes a group variable
#' used for mixed repeated measures designs, a subject number, and two
#' repeated measures variables. These variables include FSG (forward strength)
#' which is a measure of the relation between two words like cheddar to cheese.
#' The second variable is BSG (backward strength), which is the opposite
#' relation (cheese to cheddar). Participants rated those word pairs in and the
#' strength of FSG and BSG was manipulated to measure overestimation of strength.
#'
#' @docType data
#'
#' @usage data(rm2_data)
#'
#' @format A data frame of ratings of word pair relation
#'
#' group: A between-subjects variable indicating the type of instructions
#' subject: A subject number
#' fsglobsglo: A repeated measures condition of low FSG-BSG
#' fsghihbsglo: A repeated measures condition of high FSG, low BSG
#' fsglobsghi: A repeated measures condition of low FSG, high BSG
#' fsghibsghi: A repeated measures condition of high FSG-BSG
#'
#' @keywords datasets
#'
"rm2_data"
