#' Convert R\eqn{^2} and degrees of freedom to F
#'
#' \deqn{F = \frac{(R^2 / df_1)}{((1 - R^2) / df_2)}}
#'
#' If \code{df1} and \code{df2} are not provided,
#' they can be computed from \code{n} and \code{p}.
#'
#' @param r2 Coefficient of determination (\eqn{R^2}).
#' @param df1 Numerator degrees of freedom (\eqn{df_1}).
#' @param df2 Denominator degrees of freedom (\eqn{df_2}).
#' @param p Number of predictors.
#' @param n Total sample size.
#'
#' @return Numeric scalar: \eqn{F} statistic.
#' @keywords internal
#' @noRd
r_square_f <- function(r2 = NULL, df1 = NULL, df2 = NULL, p = NULL, n = NULL) {
  if (is.null(df1) && is.null(df2) && !is.null(n) && !is.null(p)) {
    df1 <- p
    df2 <- n - p - 1
  }
  if (is.null(df1) || is.null(df2)) {
    stop("You have not specified 'df1', 'df2', 'n', and/or 'p' correctly.")
  }
  (r2 / df1) / ((1 - r2) / df2)
}