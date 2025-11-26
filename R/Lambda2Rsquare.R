Lambda2Rsquare <-
function(Lambda=NULL, N=NULL)
{
if(is.null(Lambda) | is.null(N)) stop("You must specify \'Lambda\' (i.e., a noncentral parameter) and \'N\' (i.e., sample size) in order to calculate the noncentraility paramter.")
return(Lambda/(Lambda + N))
}
#' Convert noncentrality parameter (\eqn{\Lambda}) and \eqn{N} to \eqn{R^2}
#'
#' \deqn{R^2 = \frac{\Lambda}{\Lambda + N}}
#'
#' @param Lambda Noncentrality parameter (\eqn{\Lambda}).
#' @param N Sample size (\eqn{N}).
#'
#' @return Numeric scalar: \eqn{R^2}.
#' @keywords internal
#' @noRd
Lambda2Rsquare <- function(Lambda = NULL, N = NULL) {
  if (is.null(Lambda) | is.null(N)) {
    stop("You must specify 'Lambda' (noncentrality parameter) and 'N' (sample size) to calculate R^2.")
  }
  Lambda / (Lambda + N)
}