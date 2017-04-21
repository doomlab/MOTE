#' d.dept.diff
#'
#' This function displays Cohen's d and confidence itnerval for dependent t differences
#'
#' @param mdiff mean difference score
#' @param sddiff standard deviation of the differences scores
#' @param n sample size
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.dept.diff(mdiff = 5, sddiff = 1, n=100, a = .05, k = 2)


d.dept.diff <- function (mdiff, sddiff, n, a = .05, k = 2) {
  # Displays Cohen's d and confidence interval for
  # dependent t differences.
  #
  # Args: 
  #   mdiff : mean difference score
  #   sddiff: standard deviation of the differences scores
  #   n     : sample size
  #   a     : significance level
  #   k     : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic and 
  #   Cohen's d with confidence intervals.
  
  d <- mdiff / sddiff
  se <- sddiff / sqrt(n)
  t <- mdiff / se
  ahigh <- a / 2
  alow <- 1 - (a / 2)
  ncplow <- Ncpt(x = t, q = alow, df = (n - 1))
  ncphigh <- Ncpt(x = t, q = ahigh, df = (n - 1))
  dlow <- ncplow / sqrt(n)
  dhigh <- ncphigh / sqrt(n)
  Mlow <- mdiff - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- mdiff + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  # Print the result
  cat("M = ", 
      Apa(mdiff, k),
      ", SD = ", 
      Apa(sddiff, k),
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
