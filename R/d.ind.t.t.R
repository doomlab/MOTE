#' d.ind.t.t
#'
#' This function displays d for between subjects data
#' and the non-central confidence interval estimating from the t-statistic.
#' 
#' @param t t-test value
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @keywords effect size, independent t
#' @export
#' @examples
#' d.ind.t.t(t = 4.12, n1 = 100, n2 = 100, a = .05)


d.ind.t.t <- function (t, n1, n2, a = .05) {
  # Displays d and non-cental confidence interval
  # estimating from the t-statistic.
  #
  # Args: 
  #   t : t-test value
  #   n1: sample size group one
  #   n2: sample size group two
  #   a  : significance level
  #
  # Returns:
  #   List of d and sample size statistics
  
  library(MBESS)
  
  d <- 2 * t / sqrt(n1 + n2 - 2)
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p) 
  
  return(output)
}
