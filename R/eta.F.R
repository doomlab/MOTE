#' eta.F
#'
#' This function displays eta, r squared, ICCs from ANOVA analyses
#' and their non-central confidence interval based on the F distribution.
#' These values are calculated directly from F statistics and can be used 
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial eta squared.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#' @keywords effect size, eta, ANOVA
#' @export
#' @examples
#' eta.F(dfm = 2, dfe = 20, Fvalue = 5.7, a = .05)


eta.F <- function (dfm, dfe, Fvalue, a = .05) {
  # This function displays eta, r squared, ICCs from ANOVA analyses
  # and their non-central confidence interval based on the F distribution.
  #
  # Args: 
  #   dfm     : degrees of freedom model/IV/between
  #   dfe     : degrees of freedom error/residual/within
  #   Fvalue  : F statistic
  #   a       : significance level
  #
  # Returns:
  #   List of eta, F, and sample size statistics
  
  eta <- (dfm * Fvalue) / (dfm * Fvalue + dfe)
  ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  elow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  ehigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)
  
  output <- list("eta" = eta, #eta stats
                "etalow" = elow,
                "etahigh" = ehigh,
                "dfm" = dfm, #sig stats
                "dfe" = dfe,
                "F" = Fvalue,
                "p" = p) 
  
  return(output)
  
}
