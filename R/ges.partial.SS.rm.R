#' Partial Generalized Eta-Squared for ANOVA from F
#'
#' This function displays partial ges squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for multi-way repeated measures designs.
#'
#' To calculate partial generalized eta squared, first, the sum of
#' squares of the model, sum of squares of the subject
#' variance, sum of squares for the first and second independent variables,
#' and the sum of squares for the interaction are added together.
#' The sum of squares of the model is divided by this value.
#'
#'      ges <- ssm / (ssm + sss + sse1 + sse2 + sse3)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gesrmss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sss sum of squares subject variance
#' @param sse1 sum of squares for the error/residual/within for the first IV
#' @param sse2 sum of squares for the error/residual/within for the second IV
#' @param sse3 sum of squares for the error/residual/within for the interaction
#' @param Fvalue F statistic
#' @param a significance level
#' @return Partial generalized eta-squared (GES) with associated confidence intervals
#' and relevant statistics.
#' \item{ges}{effect size}
#' \item{geslow}{lower level confidence interval for ges}
#' \item{geshigh}{upper level confidence interval for ges}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the generalized eta squared statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, ges, ANOVA
#' @import MBESS
#' @import stats
#' @export
#' @examples
#' ges.partial.SS.rm(dfm = 2, dfe = 100,
#'                   ssm = 435, sss = 659,
#'                   sse1 = 435, sse2 = 446, sse3 = 546,
#'                   Fvalue = 5.46, a = .05)


ges.partial.SS.rm <- function (dfm, dfe, ssm, sss, sse1, sse2, sse3, Fvalue, a = .05) {
  # This function displays ges squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom for the error/residual/within
  #   ssm     : sum of squares for the model/IV/between
  #   sss     : sum of squares subject variance
  #   sse1    : sum of squares for the error/residual/within for the first IV
  #   sse2    : sum of squares for the error/residual/within for the second IV
  #   sse3    : sum of squares for the error/residual/within for the interaction
  #   Fvalue  : F statistic
  #   a       : significance level
  #
  # Returns:
  #   List of ges, F, and sample size statistics

  ges <- ssm / (ssm + sss + sse1 + sse2 + sse3)

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #glow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ghigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = ges, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("ges" = ges, #ges stats
                 "geslow" = limits$Lower.Conf.Limit.R2,
                 "geshigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\eta^2_{G}$ = ", apa(ges,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname ges.partial.SS.rm
#' @export
