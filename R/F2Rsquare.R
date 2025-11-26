#' Convert F and degrees of freedom to R\eqn{^2}
#'
#' \deqn{R^2 = \frac{F \cdot df_1}{F \cdot df_1 + df_2}}
#'
#' @param F.value F statistic.
#' @param df.1 Numerator degrees of freedom (\eqn{df_1}).
#' @param df.2 Denominator degrees of freedom (\eqn{df_2}).
#'
#' @return Numeric scalar: \eqn{R^2}.
#' @keywords internal
#' @noRd
F2Rsquare <- function(F.value, df.1, df.2) {
  if (missing(F.value) || missing(df.1) || missing(df.2)) {
    stop("Please supply F.value, df.1, and df.2.")
  }
  (F.value * df.1) / (F.value * df.1 + df.2)
}