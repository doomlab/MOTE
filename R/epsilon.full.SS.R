#' Epsilon for ANOVA from F and Sum of Squares
#'
#' This function displays epsilon squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total calculation.
#'
#' To calculate epsilon, first, the mean square for the error is
#' substracted from the mean square for the model. The difference
#' is multiplied by the degrees of freedom for the model. The
#' product is divided by the sum of squares total.
#'
#'      epsilon = (dfm * (msm - mse)) / (sst)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/epsilon.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total
#' @param a significance level
#' @return Provides the effect size (Cohen's d) with associated confidence intervals,
#' the t-statistic, the confidence intervals associated with the means of each group, as well as the
#' standard deviations and standard errors of the means for each group.
#'
#' \item{epsilon}{effect size}
#' \item{epsilonlow}{lower level confidence interval of epsilon}
#' \item{epsilonhigh}{upper level confidence interval of epsilon}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, epsilon, ANOVA
#' @export
#' @examples
#' epsilon.full.SS(dfm = 2, dfe = 100, msm = 214, mse = 100, sst = 5339, a = .05)


epsilon.full.SS <- function (dfm, dfe, msm, mse, sst, a = .05) {
  # This function displays epsilon squared from ANOVA analyses
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
  #   List of epsilon, F, and sample size statistics

  epsilon <- (dfm * (msm - mse)) / (sst)
  Fvalue <- msm / mse

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #elow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ehigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = epsilon, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  output <- list("epsilon" = epsilon, #epsilon stats
                 "epsilonlow" = limits$Lower.Conf.Limit.R2,
                 "epsilonhigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p)

  return(output)

}
