#' Convert noncentrality parameter (\eqn{\lambda}) and \eqn{n} to \eqn{r^2}
#'
#' \deqn{r^2 = \frac{\lambda}{\lambda + n}}
#'
#' @param lambda Noncentrality parameter (\eqn{\lambda}).
#' @param n Sample size (\eqn{n}).
#'
#' @return Numeric scalar: \eqn{r^2}.
#' @keywords internal
#' @noRd
lambda_to_r2 <- function(lambda = NULL, n = NULL) {
  if (is.null(lambda) || is.null(n)) {
    stop("You must specify 'lambda' (noncentrality parameter)
    and 'n' (sample size) to calculate r2.")
  }
  lambda / (lambda + n)
}
