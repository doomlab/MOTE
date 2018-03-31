#' d.to.r
#'
#' Calculates r from d and then translates r to r2 to calculate
#' the non-central confidence interval for r2 using the F distribution.
#'
#' @param d effect size statistic
#' @param n1 sample size group one
#' @param n2 sample size group two 
#' @param a significance level
#' @keywords effect size, correlation
#' @export
#' @examples
#' d.to.r(d = .5, n1 = 50, n2 = 50, a = .05)


d.to.r <- function (d, n1, n2, a = .05) {
  # This function Displays transformation from r to r2 to calculate
  # the non-central confidence interval for r2.
  #
  # Args: 
  #   d   : effect size statistic
  #   n1  : sample size group one
  #   n2  : sample size group two
  #   a   : significance level
  #
  # Returns:
  #   List of r, r2, and sample size statistics
  
  library(MBESS)
  
  correct = (n1 + n2)^2 / (n1*n2)
  n = n1 + n2
  r <- d / sqrt(d^2 + correct)
  rsq <- (r) ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) * ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  Fvalue <- t ^ 2
  dfm <- 1
  dfe <- n - 2
  ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  rsqlow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  rsqhigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)
  ciforr <- ci.R(R = abs(r), df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)
  
  #deal with negative r / d values 
  if (r < 0) { 
    rlow = 0 - ciforr$Lower.Conf.Limit.R
    rhigh = 0 - ciforr$Upper.Conf.Limit.R
  } else {
    rlow = ciforr$Lower.Conf.Limit.R
    rhigh = ciforr$Upper.Conf.Limit.R
    }
  
  output = list("r" = r, #r stats
                "rlow" = rlow, 
                "rhigh" = rhigh, 
                "R2" = rsq, #R squared stats
                "R2low" = rsqlow,
                "R2high" = rsqhigh,
                "se" = se,
                "n" = n, #sample stats
                "dfm" = 1, #sig stats
                "dfe" = (n - 2),
                "t" = t,
                "F" = Fvalue,
                "p" = p) 
  
  return(output)
}
