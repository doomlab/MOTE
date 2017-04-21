#' v.chi.sq
#'
#' This function Displays the effect size and confidence interval for the specified chi-square metric using Cramer's V.
#'
#' @param x2 chi-square metric
#' @param n sample size
#' @param r correlation
#' @param c number of columns in the contigency table
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' v.chi.sq(x2, n, r, c, a = .05, k = 2)


v.chi.sq <- function (x2, n, r, c, a = .05, k = 2) {
  # Displays the effect size and confidence interval for the specified
  # chi-square metric using Cramer's V.
  #
  # Args: 
  #   x2: chi-square metric
  #   n : total sample size
  #   r : correlation
  #   c : number of columns in the contigency table
  #   a : significance level
  #   k : significant digits to use for formatting
  #
  # Returns:
  #   String describing the effect size and CI using Cramer's V.
  
  dfsmall <- min(r - 1, c - 1)
  v <- sqrt(x2 / n * dfsmall)
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  dftotal <- (r - 1) * (c - 1)
  ncplow <- NcpChisq(x = x2, q = alow, df = dftotal)
  ncphigh <- NcpChisq(x = x2, q = ahigh, df = dftotal)
  vlow <- sqrt((ncplow + dftotal) / (n * dfsmall))
  vhigh <- sqrt((ncphigh + dftotal) / (n * dfsmall)) 
  p <- pchisq(x2, dftotal, lower.tail = F)
  if (p < .001) {
    p = .001
  }
  # Print the result
  cat("X2(", dftotal, ") = ", 
      Apa(x2, k),
      ", ", 
      p.value(p, k), 
      ", V = ", 
      Apa(v, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(vlow, k), 
      " - ", 
      Apa(vhigh, k), "]", sep = "")
}
