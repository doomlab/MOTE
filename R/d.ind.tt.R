#' d.ind.tt
#'
#' This function Displays Cohen's d and confidence interval for independent t from t.
#'
#' @param t t-test value
#' @param n1 sample size from first group
#' @param n2 sample size from second group
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.ind.tt(t = 4.12, n1 = 100, n2 = 100, a = .05, k = 2)


d.ind.tt <- function (t, n1, n2, a = .05, k = 2) {
  # Displays Cohen's d and confidence interval for
  # independent t from t.
  #
  # Args: 
  #   t : t-test value
  #   n1: sample size from first group
  #   n2: sample size from second group
  #   a  : significance level
  #   k  : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic and 
  #   Cohen's d with confidence intervals.
  
  d <- 2 * t / sqrt(n1 + n2 - 2)
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n1 + n2 - 2))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n1 + n2 - 2))
  dlow <- ncplow / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncphigh / sqrt(((n1 * n2) / (n1 + n2)))
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2
  # Print the result
  cat("t(", (n1 - 1 + n2 - 1), ") = ", 
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
