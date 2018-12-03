#' Partial Eta Squared for ANOVA from F and Sum of Squares
#'
#' This function displays partial eta squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs.
#'
#' Partial eta squared is calculated by dividing the sum of squares
#' of the model by the sum of the sum of squares of the model and
#' sum of squares of the error.
#'
#'      partial eta squared = ssm / (ssm + sse)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etapss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#'
#' @return Provides partial eta squared with associated confidence intervals
#' and relevant statistics.
#'
#' \item{eta}{partial eta squared effect size}
#' \item{etalow}{lower level confidence interval of partial eta squared}
#' \item{etahigh}{upper level confidence interval of partial eta squared}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, eta, ANOVA
#' @import MBESS
#' @import stats
#' @export
#' @examples
#' eta.partial.SS(dfm = 2, dfe = 100, ssm = 435, sse = 659, Fvalue = 5.46, a = .05)


eta.partial.SS <- function (dfm, dfe, ssm, sse, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sse)){
    stop("Be sure to include the sum of squares for the error.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta <- ssm / (ssm + sse)

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #elow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #ehigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = eta, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  output <- list("eta" = eta, #eta stats
                 "etalow" = limits$Lower.Conf.Limit.R2,
                 "etahigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p)

  return(output)

}
