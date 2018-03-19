#' omega.full.SS
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs with careful
#' focus on which error term you are using for the calculation.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total 
#' @param a significance level
#' @keywords effect size, omega, ANOVA
#' @export
#' @examples
#' omega.full.SS(dfm = 2, dfe = 100, msm = 214, mse = 100, sst = 5339, a = .05)


omega.full.SS <- function (dfm, dfe, msm, mse, sst, a = .05) {
  # This function displays omega squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args: 
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom error/residual/within
  #   msm     : mean square for the model/IV/between
  #   mse     : mean square for the error/residual/within
  #   sst     : sum of squares total 
  #   a       : significance level
  #
  # Returns:
  #   List of omega, F, and sample size statistics
  
  omega <- (dfm * (msm - mse)) / (sst + mse)
  Fvalue <- msm / mse
  ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  olow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  ohigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)
  
  output <- list("omega" = omega, #omega stats
                 "omegalow" = olow,
                 "omegahigh" = ohigh,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p) 
  
  return(output)
  
}
