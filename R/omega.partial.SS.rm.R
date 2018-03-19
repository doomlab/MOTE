#' omega.partial.SS.rm
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way repeated measures designs and mix level designs.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param mss mean square for the subject variance
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param sss sum of squares for the subject variance
#' @param a significance level
#' @keywords effect size, omega, ANOVA
#' @export
#' @examples
#' omega.partial.SS.rm(dfm = 2, dfe = 100, 
#'                     msm = 214, mse = 100, mss = 20, 
#'                     ssm = 5339, sse = 435, sss = 53, a = .05)

omega.partial.SS.rm <- function (dfm, dfe, msm, mse, mss, ssm, sse, sss, a = .05) {
  # This function displays omega squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args: 
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom for the error/residual/within
  #   msm     : mean square for the model/IV/between
  #   mse     : mean square for the error/residual/within
  #   mss     : mean square for the subject variance
  #   ssm     : sum of squares for the model/IV/between
  #   sse     : sum of squares for the error/residual/within
  #   sss     : sum of squares for the subject variance
  #   a       : significance level
  #
  # Returns:
  #   List of omega, F, and sample size statistics
  
  omega <- (dfm * (msm - mse)) / (ssm + sse + sss + mss)
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
