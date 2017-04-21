#' d.single.tt
#'
#' This function Displays Cohen's d and confidence interval for single t from t.
#'
#' @param t t-test value
#' @param n sample size
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.single.tt(t=4.20, n=100, a = .05, k = 2)


d.single.tt <- function (t, n, a = .05, k = 2) {
  # Displays Cohen's d and confidence interval for single t
  # from t.
  #
  # Args: 
  #   t : t-test value
  #   n : sample size
  #   a : significance level
  #   k : significant digits to use for formatting
  #
  # Returns:
  #   String describing the test statistic, Cohen's d
  #   effect size with confidence intervals.
  
  d <- t / sqrt(n)
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n - 1))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n - 1))
  dlow <- ncplow / sqrt(n)
  dhigh <- ncphigh / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  # Print the result
  cat("t(", n - 1, ") = ", 
      Apa(t, k),
      ", ", 
      p.value(p, k), 
      ", d = ", 
      Apa(d, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(dlow, k), 
      " - ", 
      Apa(dhigh, k), "]", sep = "")
}
