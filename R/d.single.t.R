#' d.single.t
#'
#' This function displays d and non-central confidence interval for single t from means. 
#'
#' @param m sample mean
#' @param u population mean
#' @param sd sample standard deviation
#' @param n sample size
#' @param a significance level
#' @keywords effect size, single t
#' @export
#' @examples
#' d.single.t(m = 20, u = 17, sd = 4, n = 100, a = .05)

d.single.t = function (m, u, sd, n, a = .05) {
  # This function displays d and non-central confidence interval for single t from means. 
  #
  # Args: 
  #   m : sample mean
  #   u : population mean
  #   sd: sample standard deviation
  #   n : sample size
  #   a : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics
  
  library(MBESS)
  
  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ncpboth = conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = m - se*qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh = m + se*qt(a / 2, n - 1, lower.tail = FALSE)
  p = pt(abs(t), n - 1, lower.tail = F)*2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "m" = m, #mean stats
                "sd" = sd,
                "se" = se,
                "Mlow" = Mlow, 
                "Mhigh" = Mhigh,
                "u" = u,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats,
                "p" = p) 
  
  return(output)
}
