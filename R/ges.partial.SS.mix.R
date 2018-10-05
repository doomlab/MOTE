#' Partial Generalized Eta-Squared for Mixed Design ANOVA from F
#'
#' This function displays partial generalized eta-squared (GES) from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for mixed designs.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sss sum of squares subject variance
#' @param sse sum of squares for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#'
#' @return Partial generalized eta-squared (GES) with associated confidence intervals
#' and relevant statistics.
#' \item{ges}{effect size}
#' \item{geslow}{lower level confidence interval for ges}
#' \item{geshigh}{upper level confidence interval for ges}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#'
#' @keywords effect size, ges, ANOVA
#' @export
#' @examples
#' ges.partial.SS.mix(dfm = 2, dfe = 100, ssm = 435, sss = 235, sse = 659, Fvalue = 5.46, a = .05)


ges.partial.SS.mix <- function (dfm, dfe, ssm, sss, sse, Fvalue, a = .05) {
  # This function displays ges squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom for the error/residual/within
  #   ssm     : sum of squares for the model/IV/between
  #   sss     : sum of squares subject variance
  #   sse     : sum of squares for the error/residual/within
  #   Fvalue  : F statistic
  #   a       : significance level
  #
  # Returns:
  #   List of ges, F, and sample size statistics

  ges <- ssm / (ssm + sss+ sse)

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #glow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ghigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = ges, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)


  output <- list("ges" = ges, #ges stats
                 "geslow" = limits$Lower.Conf.Limit.R2,
                 "geshigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p)

  return(output)

}
