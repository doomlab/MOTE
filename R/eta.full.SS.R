#' Eta for ANOVA from F and Sum of Squares
#'
#' This function displays eta squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sst sum of squares total
#' @param Fvalue F statistic
#' @param a significance level
#' @return Provides eta with associated confidence intervals and relevant statistics.
#'
#' \item{eta}{effect size}
#' \item{etalow}{lower level confidence interval of eta}
#' \item{etahigh}{upper level confidence interval of eta}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#'
#' @keywords effect size, eta, ANOVA
#' @export
#' @examples
#' eta.full.SS(dfm = 2, dfe = 100, ssm = 435, sst = 659, Fvalue = 5.46, a = .05)


eta.full.SS <- function (dfm, dfe, ssm, sst, Fvalue, a = .05) {
  # This function displays eta squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     :  degrees of freedom for the model/IV/between
  #   dfe     :  degrees of freedom for the error/residual/within
  #   ssm     :  sum of squares for the model/IV/between
  #   sst     :  sum of squares total
  #   Fvalue  :  F statistic
  #   a       :  significance level
  #
  # Returns:
  #   List of eta, F, and sample size statistics

  eta <- ssm / sst

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
