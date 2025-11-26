#' Convert R\eqn{^2} and degrees of freedom to F
#'
#' \deqn{F = \frac{(R^2 / df_1)}{((1 - R^2) / df_2)}}
#'
#' If \code{df.1} and \code{df.2} are not provided, they can be computed from \code{N} and \code{p}.
#'
#' @param R2 Coefficient of determination (\eqn{R^2}).
#' @param df.1 Numerator degrees of freedom (\eqn{df_1}).
#' @param df.2 Denominator degrees of freedom (\eqn{df_2}).
#' @param p Number of predictors.
#' @param N Total sample size.
#'
#' @return Numeric scalar: \eqn{F} statistic.
#' @keywords internal
#' @noRd
Rsquare2F <- function(R2 = NULL, df.1 = NULL, df.2 = NULL, p = NULL, N = NULL) {
  if (is.null(df.1) & is.null(df.2) & !is.null(N) & !is.null(p)) {
    df.1 <- p
    df.2 <- N - p - 1
  }
  if (is.null(df.1) | is.null(df.2)) {
    stop("You have not specified 'df.1', 'df.2', 'N', and/or 'p' correctly.")
  }
  (R2 / df.1) / ((1 - R2) / df.2)
}