#' d.single.t
#'
#' This function Displays Cohen's d and confidence interval for single t from means.
#'
#' @param m sample mean
#' @param u population mean
#' @param sd sample standard deviation
#' @param n sample size
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.single.t(m=20, u=17, sd=4, n=100, a = .05, k = 2)


d.single.t <- function (m, u, sd, n, a = .05, k = 2) {
  # Displays Cohen's d and confidence interval for single t
  # from means.
  #
  # Args: 
  #   m : sample mean
  #   u : population mean
  #   sd: sample standard deviation
  #   n : sample size
  #   a : significance level
  #   k : significant digits to use for formatting
  #
  # Returns:
  #   String describing the test statistic, Cohen's d
  #   effect size with confidence intervals.
  
  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n - 1))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n - 1))
  dlow <- ncplow / sqrt(n)
  dhigh <- ncphigh / sqrt(n)
  Mlow <- m - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- m + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  # Print the result
  cat("M = ", 
      Apa(m, k),
      ", SD = ", 
      Apa(sd, k),
      ", SE = ", Apa(se, k),
      ", ", (1 - a) * 100, "%CI[", 
      Apa(Mlow, k),
      " - ",
      Apa(Mhigh, k),
      "]",
      "\nt(", n - 1, ") = ", 
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
