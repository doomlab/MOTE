#' Generalized Omega Squared for Multi-Way and Mixed ANOVA from F
#'
#' This function displays generalized omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way repeated measures designs and mix level designs.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the MAIN model/IV/between
#' @param ssm2 sum sum of squares for the OTHER model/IV/between
#' @param sst sum of squares total across the whole ANOVA
#' @param mss mean square for the subject variance
#' @param j number of levels in the OTHER IV
#' @param Fvalue F statistic from the output for your IV
#' @param a significance level
#' @keywords effect size, omega, ANOVA
#' @export
#' @examples
#' omega.gen.SS.rm(dfm = 2, dfe = 100,
#'                      ssm = 5339, ssm2 = 432, sst = 10947,
#'                      mss = 543, j = 3, Fvalue = 12, a = .05)

omega.gen.SS.rm <- function (dfm, dfe, ssm, ssm2, sst, mss, j, Fvalue, a = .05) {
  # This function displays omega squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom for the error/residual/within
  #   ssm     : sum of squares for the MAIN model/IV/between
  #   ssm2    : sum sum of squares for the OTHER model/IV/between
  #   sst     : sum of squares total across the whole ANOVA
  #   mss     : mean square for the subject variance
  #   j       : number of levels in the OTHER IV
  #   Fvalue  : F statistic from the output for your IV
  #   a       : significance level
  #
  # Returns:
  #   List of omega, F, and sample size statistics

  omega <- (ssm - (dfm * mss)) / (sst + ssm2 + j*mss)

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #olow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ohigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = omega, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  output <- list("omega" = omega, #omega stats
                 "omegalow" = limits$Lower.Conf.Limit.R2,
                 "omegahigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p)

  return(output)

}
