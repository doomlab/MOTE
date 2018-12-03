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
#' @return Provides the effect size (epsilong) with associated
#' confidence intervals from the F-statistic.
#'
#' \item{epsilon}{effect size}
#' \item{epsilonlow}{lower level confidence interval of epsilon}
#' \item{epsilonhigh}{upper level confidence interval of epsilon}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the epsilon statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, epsilon, ANOVA
#' @import MBESS
#' @import stats
#' @export
#' @examples
#' epsilon.full.SS(dfm = 2, dfe = 100, msm = 214, mse = 100, sst = 5339, a = .05)


epsilon.full.SS <- function (dfm, dfe, msm, mse, sst, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(msm)){
    stop("Be sure to include the mean square value for your model (IV).")
  }

  if (missing(mse)){
    stop("Be sure to include the mean square value for the error.")
  }

  if (missing(sst)){
    stop("Be sure to include the sum of squares total for your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  epsilon <- (dfm * (msm - mse)) / (sst)
  Fvalue <- msm / mse

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #elow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ehigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = epsilon, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("epsilon" = epsilon, #epsilon stats
                 "epsilonlow" = limits$Lower.Conf.Limit.R2,
                 "epsilonhigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p)

  return(output)

}
