#' Eta and Coefficient of Determination (R2) for ANOVA from F
#'
#' This function displays eta, r squared, ICCs from ANOVA analyses
#' and their non-central confidence interval based on the F distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial eta squared.
#'
#' Eta is calculated by multiplying the degrees of freedom of
#' the model by the F-statistic. This is divided by the product
#' of degrees of freedom of the model, the F-statistic, and
#' the degrees of freedom for the error or residual.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
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
#'
#' eta.F(dfm = 2, dfe = 20, Fvalue = 5.7, a = .05)


eta.F <- function (dfm, dfe, Fvalue, a = .05) {
  # This function displays eta, r squared, ICCs from ANOVA analyses
  # and their non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom model/IV/between
  #   dfe     : degrees of freedom error/residual/within
  #   Fvalue  : F statistic
  #   a       : significance level
  #
  # Returns:
  #   List of eta, F, and sample size statistics

  eta <- (dfm * Fvalue) / (dfm * Fvalue + dfe)

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
