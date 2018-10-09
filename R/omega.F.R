#' Omega Squared for ANOVA from F
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial omega squared.
#'
#' Omega squared or partial omega squared is calculated by subtracting one
#' from the F-statistic and multiplying it by degrees of freedom of the model. This is
#' divided by the same value after adding the number of valid responses. This
#' value will be omega squared for one-way ANOVA designs, and will be
#' partial omega squared for multi-way ANOVA designs (i.e. with more than one IV).
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param Fvalue F statistic
#' @param n full sample size
#' @param a significance level
#' @return The effect size (Cohen's d) with associated confidence intervals
#' and relevant statistics.
#'
#' \item{omega}{omega statistic}
#' \item{omegalow}{lower level confidence interval d value}
#' \item{omegahigh}{upper level confidence interval d value}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}

#'
#' @keywords effect size, omega, ANOVA
#' @export
#' @examples
#' omega.F(dfm = 2, dfe = 97, Fvalue = 5.7, n = 100, a = .05)


omega.F <- function (dfm, dfe, Fvalue, n, a = .05) {
  # This function displays omega squared from ANOVA analyses
  # and its non-central confidence interval based on the F distribution.
  #
  # Args:
  #   dfm     : degrees of freedom model/IV/between
  #   dfe     : degrees of freedom error/residual/within
  #   Fvalue  : F statistic
  #   n       : full sample size
  #   a       : significance level
  #
  # Returns:
  #   List of omega, F, and sample size statistics

  omega <- (dfm * (Fvalue-1)) / ((dfm * (Fvalue-1)) + n)

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
