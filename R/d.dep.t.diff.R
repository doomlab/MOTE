#' d.dep.t.diff
#'
#' This function displays d for repeated measures data
#' and the non-central confidence interval using the 
#' standard deviation of the differences as the denominator.
#' 
#' @param mdiff mean difference score
#' @param sddiff standard deviation of the difference scores
#' @param n sample size
#' @param a significance level
#' @keywords effect size, dependent t-test
#' @export
#' @examples
#' d.dep.t.diff(mdiff = 5, sddiff = 3, n = 100, a = .05)


d.dep.t.diff <- function (mdiff, sddiff, n, a = .05) {
  # Displays d and non-central confidence interval for repeated measures
  # using the standard deviation of the differences as the denominator.
  #
  # Args: 
  #   mdiff : mean difference score
  #   sddiff: standard deviation of the differences scores
  #   n     : sample size
  #   a     : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics
  
  library(MBESS)
  
  d <- mdiff / sddiff
  se <- sddiff / sqrt(n)
  t <- mdiff / se
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  Mlow <- mdiff - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- mdiff + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2
  
  output = list("d" = d, #d stats
                "dlow" = dlow, 
                "dhigh" = dhigh, 
                "mdiff" = mdiff, #mean stats
                "Mlow" = Mlow, 
                "Mhigh" = Mhigh,
                "sddiff" = sddiff,
                "se" = se,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats
                "p" = p
                ) 
  
  return(output)
}
