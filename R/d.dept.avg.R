#' d.dept.avg
#'
#' This function displays cohen's d and confidence interval for dependent t from averages
#'
#' @param m1 mean from first group
#' @param m2 mean from second group
#' @param sd1 standard deviation from first group
#' @param sd2 standard deviation from second group
#' @param n sample size
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.dept.avg(m1=20, m2=17, sd1=4, sd2=5, n=100, a = .05, k = 2)


d.dept.avg <- function (m1, m2, sd1, sd2, n, a = .05, k = 2) {
  # Displays Cohen's d and confidence interval for
  # dependent t from averages.
  #
  # Args:
  #   m1 : mean from first group
  #   m2 : mean from second group
  #   sd1: standard deviation from first group
  #   sd2: standard deviation from second group
  #   n  : sample size
  #   a  : significance level
  #   k  : significant digits to use for formatting
  #
  # Returns:
  #   String describing Cohen's d with confidence intervals
  
  d <- (m1 - m2) / ((sd1 + sd2) / 2)
  se1 <- sd1 / sqrt(n)
  se2 <- sd2 / sqrt(n)
  t <- (m1 - m2) / ((se1 + se2) / 2)
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n - 1))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n - 1))
  dlow <- ncplow / sqrt(n)
  dhigh <- ncphigh / sqrt(n)
  M1low <- m1 - se1 * qt(a / 2, n - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n - 1, lower.tail = FALSE)
  M2high <- m2 + se2 *  qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  # Print the result
  cat("M1 = ",
      Apa(m1, k),
      ", SD = ",
      Apa(sd1, k),
      ", SE = ", Apa(se1, k),
      ", ", (1 - a) * 100, "%CI[",
      Apa(M1low, k),
      " - ",
      Apa(M1high, k),
      "]",
      "\nM2 = ",
      Apa(m2, k),
      ", SD = ",
      Apa(sd2, k),
      ", SE = ", Apa(se2, k),
      ", ", (1 - a) * 100, "%CI[",
      Apa(M2low, k),
      " - ",
      Apa(M2high, k),
      "]",
      "\nd = ",
      Apa(d, k),
      ", ", (1 - a) * 100, "%CI[",
      Apa(dlow, k),
      " - ",
      Apa(dhigh, k), "] \nNote: t and p values not reported because they are not correct for hypothesis testing.", sep = "")
}
