#' r.correl
#'
#' This function Displays transformation from r to r2
#'
#' @param r correlation coefficient
#' @param n sample size
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' r.correl(r = .5, n = 100, a = .05, k = 2)


r.correl <- function (r, n, a = .05, k = 2) {
  # Displays transformation from r to r2
  #
  # Args: 
  #   r : correlation coefficient
  #   n : sample size
  #   a : significance level
  #   k : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic and r2 
  #   effect size with confidence intervals.
  
  rsq <- r ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) * ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  Fvalue <- t ^ 2
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  dfm <- 1
  dfe <- n - 2
  ncplow <- Ncpf(x = Fvalue, q = alow, df1 = dfm, df2 = dfe)
  ncphigh <- Ncpf(x = Fvalue, q = ahigh, df1 = dfm, df2 = dfe)
  rsqlow <- ncplow / (ncplow + dfm + dfe + 1)
  rsqhigh <- ncphigh / (ncphigh + dfm + dfe + 1)
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)
  # Print the result
  cat("r = ", 
      Apa(r, k),
      " t(", (n - 2), ") = ", 
      Apa(t, k),
      ", ", 
      p.value(p, k), 
      ", r2 = ",
      Apa(rsq, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(rsqlow, k), 
      " - ", 
      Apa(rsqhigh, k), "]", sep = "")
}
