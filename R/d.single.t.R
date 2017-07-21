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

d.single.t = function (m = 0, u = 0, sd = 1, n = 10, a = .05, k = 3) {
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
  #   The output of the function will return a list of d, upper and lower CI for d, upper and lower CI for M, t-test value, p-value for t-test,  the original mean, the original standard deviation, and degrees of freedom. 

  library(MBESS)
  se = sd / sqrt(n)
  d = (m - u) / sd
  t = (m - u) / se
  ncpboth = conf.limits.nct(t, (n-1), conf.level = (1-a), sup.int.warns = TRUE)
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = m - se*qt(a/2, n-1, lower.tail = FALSE)
  Mhigh = m + se*qt(a/2, n-1, lower.tail = FALSE)
  p = pt(abs(t), n-1, lower.tail = F)*2
  
  output = list("d" = apa(d, k), 
                "dlow" = apa(dlow, k), 
                "dhigh" = apa(dhigh, k), 
                "Mlow" = apa(Mlow, k), 
                "Mhigh" = apa(Mhigh, k),
                "t" = apa(t, k),
                "p" = p.value(p, k), 
                "m" = apa(m, k),
                "sd" = apa(sd, k),
                "df" = (n-1))
  return(output)
}
