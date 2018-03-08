#' eta.anova
#'
#' This function displays eta, r squared, ICCs from ANOVA analyses
#' and their non-central confidence interval based on the F distribution.
#'
#' @param dfm degrees of freedom model
#' @param dfe degrees of freedom error
#' @param Fvalue F statistic
#' @param a significance level
#' @keywords effect size, eta, ANOVA
#' @export
#' @examples
#' eta.anova(dfm = 2, dfe = 20, Fvalue = 5.7, a = .05)


eta.anova <- function (dfm, dfe, Fvalue, a = .05) {
  # This function displays eta, r squared, ICCs from ANOVA analyses
  # and their non-central confidence interval based on the F distribution.
  #
  # Args: 
  #   dfm: degrees of freedom model
  #   dfe: degrees of freedom error
  #   f  : F statistic
  #   a  : significance level
  #
  # Returns:
  #   List of eta, F, and sample size statistics
  
  eta <- (dfm * Fvalue) / (dfm * Fvalue + dfe)
  ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  elow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  ehigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)
  
  output <- data.frame(es = eta,
                       es_ll = elow,
                       es_ul = ehigh,
                       Fvalue = Fvalue,
                       dfm = dfm,
                       dfe = dfe)
  return(output)
  
}
