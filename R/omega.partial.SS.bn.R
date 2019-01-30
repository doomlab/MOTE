#' Partial Omega Squared for Between Subjects ANOVA from F
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way between subjects designs.
#'
#' Partial omega squared is calculated by subtracting the mean square for the error
#' from the mean square of the model, which is multiplied by degrees of freedom of
#' the model. This is divided by the product of the degrees of freedom
#' for the model are deducted from the sample size, multiplied by the
#' mean square of the error, plus the sum of squares for the model.
#'
#'      omega <- (dfm * (msm - mse)) / (ssm + (n-dfm)*mse)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegapbnss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param n total sample size
#' @param a significance level
#'
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
#' \item{estimate}{the omega squared statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, omega, ANOVA
#' @import MBESS
#' @import stats
#' @export
#' @examples
#' omega.partial.SS.bn(dfm = 2, dfe = 100, msm = 214, mse = 100, ssm = 5339, n = 150, a = .05)

omega.partial.SS.bn <- function (dfm, dfe, msm, mse, ssm, n, a = .05) {
  # This function displays omega squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom for the model/IV/between
  #   dfe     : degrees of freedom error/residual/within
  #   msm     : mean square for the model/IV/between
  #   mse     : mean square for the error/residual/within
  #   ssm     : sum of squares for the model/IV/between
  #   n       : total sample size
  #   a       : significance level
  #
  # Returns:
  #   List of omega, F, and sample size statistics

  omega <- (dfm * (msm - mse)) / (ssm + (n-dfm)*mse)
  Fvalue <- msm / mse

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #olow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ohigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = omega, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("omega" = omega, #omega stats
                 "omegalow" = limits$Lower.Conf.Limit.R2,
                 "omegahigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\omega^2_{p}$ = ", apa(omega,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname omega.partial.SS.bn
#' @export

