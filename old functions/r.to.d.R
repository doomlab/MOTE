#' r.to.d
#'
#' This function displays d for repeated measures data
#' and the non-central confidence interval using the 
#' standard deviation of the differences as the denominator
#' estimating from the t-statistic based on provided r value.
#' 
#' @param r correlation coefficient
#' @param n sample size
#' @param a significance level
#' @keywords effect size, dependent t-test
#' @export
#' @examples
#' r.to.d(r = .5, n = 100, a = .05)


r.to.d <- function (r, n, a = .05) {
  # This function displays d for repeated measures data
  # and the non-central confidence interval using the 
  # standard deviation of the differences as the denominator
  # estimating from the t-statistic based on provided r value.
  #
  # Args: 
  #   r : correlation coefficient
  #   n : sample size 
  #   a  : significance level
  #
  # Returns:
  #   List of d and sample size statistics
  
  library(MBESS)
  
  d <- 2*r / (1 - r^2)
  t <- r / sqrt((1-r^2) / (n - 2))
  t <- d*sqrt(n)
  ncpboth <- conf.limits.nct(t, (n - 2), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  p <- pt(abs(t), n - 2, lower.tail = F) * 2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "n" = n, #sample stats
                "df" = (n - 2),
                "t" = t, #sig stats
                "p" = p
  ) 
  
  return(output)
}
