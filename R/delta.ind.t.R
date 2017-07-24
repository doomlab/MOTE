#' delta.ind.t
#'
#' This function displays d-delta for between subjects data
#' and the non-central confidence interval using the 
#' control group standard deviation as the denominator.
#'
#' @param m1 mean from control group
#' @param m2 mean from experimental group
#' @param sd1 standard deviation from control group
#' @param sd2 standard deviation from experimental group
#' @param n1 sample size from control group
#' @param n2 sample size from experimental group
#' @param a significance level
#' @keywords effect size, delta, independent t
#' @export
#' @examples
#' delta.ind.t(m1 = 20, m2 = 17, sd1 = 4, sd2 = 5, n1 = 100, n2 = 100, a = .05)


delta.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05) {
  # This function displays d-delta for between subjects data
  # and the non-central confidence interval using the 
  # control group standard deviation as the denominator.
  #
  # Args: 
  #   m1 : mean from control group
  #   m2 : mean from experimental group
  #   sd1: standard deviation from control group
  #   sd2: standard deviation from experimental group
  #   n1 : sample size from control group
  #   n2 : sample size from experimental group
  #   a  : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics

  library(MBESS)  
  
  spooled <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- (m1 - m2) / sd1
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "M1" = m1, #control group stats
                "sd1" = sd1,
                "se1" = se1,
                "M1low" = M1low, 
                "M1high" = M1high,
                "M2" = m2, #experimental group stats
                "sd2" = sd2,
                "se2" = se2,
                "M2low" = M2low,
                "M2high" = M2high,
                "spooled" = spooled,
                "sepooled" = sepooled,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p) 
  
  return(output)
}
