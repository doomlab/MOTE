#' V for Chi-Square
#'
#' This function displays V and non-central confidence interval
#' for the specified chi-square statistic.
#'
#' @param x2 chi-square statistic
#' @param n sample size
#' @param r number of rows in the contingency table
#' @param c number of columns in the contingency table
#' @param a significance level
#' @keywords effect size, chi-square
#' @export
#' @examples
#' v.chi.sq(x2 = 4, n = 25, r = 2, c = 2, a = .05)


v.chi.sq <- function (x2, n, r, c, a = .05) {
  # This function displays V and non-central confidence interval
  # for the specified chi-square statistic.
  #
  # Args:
  #   x2: chi-square metric
  #   n : total sample size
  #   r : number of rows in the contingency table
  #   c : number of columns in the contingency table
  #   a : significance level
  #
  # Returns:
  #   List of V, X2, and sample size statistics

  library(MBESS)

  dfsmall <- min(r - 1, c - 1)
  v <- sqrt(x2 / (n * dfsmall))
  dftotal <- (r - 1) * (c - 1)
  ncpboth <- conf.limits.nc.chisq(x2, df = dftotal, conf.level = (1 - a))
  vlow <- sqrt((ncpboth$Lower.Limit + dftotal) / (n * dfsmall))
  vhigh <- sqrt((ncpboth$Upper.Limit + dftotal) / (n * dfsmall))
  p <- pchisq(x2, dftotal, lower.tail = F)

  output = list("v" = v, #v stats
                "vlow" = vlow,
                "vhigh" = vhigh,
                "n" = n, #sample stats
                "df" = dftotal,
                "x2" = x2, #sig stats,
                "p" = p)

  return(output)
}
