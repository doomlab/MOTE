#' Omega Squared for One-Way and Multi-Way ANOVA from F
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs with careful
#' focus on which error term you are using for the calculation.
#'
#' Omega squared is calculated by deducting the mean square of the error
#' from the mean square of the model and multiplying by the degrees of freedom for the model.
#' This is divided by the sum of the sum of squares total and the mean square of the error.
#'
#'      omega = (dfm * (msm - mse)) / (sst + mse)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegass.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total
#' @param a significance level
#' @return Provides omega squared with associated confidence intervals
#' and relevant statistics.
#'
#' \item{omega}{omega squared}
#' \item{omegalow}{lower level confidence interval of omega}
#' \item{omegahigh}{upper level confidence interval of omega}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
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
