#' d.single.t.t
#'
#' This function displays d and non-central confidence interval for single t 
#' estimated from the t-statistic.
#'
#' @param t t-test value
#' @param n sample size
#' @param a significance level
#' @keywords effect size, single t
#' @export
#' @examples
#' d.single.t.t(t = 4.20, n = 100, a = .05)


d.single.t.t <- function (t, n, a = .05) {
  # This function displays d and non-central confidence interval for single t 
  # estimated from the t-statistic.  
  #
  # Args: 
  #   t : t-test value
  #   n : sample size
  #   a : significance level
  #
  # Returns:
  #   List of d and sample size statistics

  library(MBESS)
    
  d <- t / sqrt(n)
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats
                "p" = p
  ) 
  
  return(output)

}
