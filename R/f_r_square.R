#' Convert f and degrees of freedom to r\eqn{^2}
#'
#' \deqn{r^2 = \frac{f \cdot df_1}{f \cdot df_1 + df_2}}
#'
#' @param f_value F statistic.
#' @param df1 Numerator degrees of freedom (\eqn{df_1}).
#' @param df2 Denominator degrees of freedom (\eqn{df_2}).
#'
#' @return Numeric scalar: \eqn{R^2}.
#' @keywords internal
#' @noRd
f_r_square <- function(f_value, df1, df2) {
  if (missing(f_value) || missing(df1) || missing(df2)) {
    stop("Please supply f_value, df1, and df2.")
  }
  (f_value * df1) / (f_value * df1 + df2)
}