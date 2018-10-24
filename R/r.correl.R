#' r to Coefficient of Determination (R2) from F
#'
#' This function displays transformation from r to r2 to calculate
#' the non-central confidence interval for r2 using the F distribution.
#'
#' The t-statistic is calculated by first dividing one minus the
#' square root of r squared by degrees of freedom of the error.
#' r is divided by this value.
#'
#'      t = r / sqrt((1 - rsq) / (n - 2))
#'
#' The F-statistic is the t-statistic squared.
#'
#'      Fvalue = t ^ 2
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/rcorrel.html}{Learn more on our example page.}
#'
#' @param r correlation coefficient
#' @param n sample size
#' @param a significance level
#' @return Provides correlation coefficient and coefficient of
#' determination with associated confidence intervals
#' and relevant statistics.
#'
#' \item{r}{correlation coefficient}
#' \item{rlow}{lower level confidence interval r}
#' \item{rhigh}{upper level confidence interval r}
#' \item{R2}{coefficient of determination}
#' \item{R2low}{lower level confidence interval of R2}
#' \item{R2high}{upper level confidence interval of R2}
#' \item{se}{standard error}
#' \item{n}{sample size}
#' \item{dfm}{degrees of freedom of mean}
#' \item{dfe}{degrees of freedom of error}
#' \item{t}{t-statistic}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#'
#' @keywords effect size, correlation
#' @export
#' @examples
#' r.correl(r = .5, n = 100, a = .05)


r.correl <- function (r, n, a = .05) {
  # This function Displays transformation from r to r2 to calculate
  # the non-central confidence interval for r2.
  #
  # Args:
  #   r : correlation coefficient
  #   n : sample size
  #   a : significance level
  #
  # Returns:
  #   List of r, r2, and sample size statistics

  library(MBESS)

  rsq <- (r) ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) * ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  Fvalue <- t ^ 2
  dfm <- 1
  dfe <- n - 2

  #ncpboth <- conf.limits.ncf(Fvalue, df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  #rsqlow <- ncpboth$Lower.Limit / (ncpboth$Lower.Limit + dfm + dfe + 1)
  #rsqhigh <- ncpboth$Upper.Limit / (ncpboth$Upper.Limit + dfm + dfe + 1)

  limits <- ci.R2(R2 = rsq, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  ciforr <- ci.R(R = abs(r), df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  #deal with negative r / d values
  if (r < 0) {
    rlow = 0 - ciforr$Lower.Conf.Limit.R
    rhigh = 0 - ciforr$Upper.Conf.Limit.R
  } else {
    rlow = ciforr$Lower.Conf.Limit.R
    rhigh = ciforr$Upper.Conf.Limit.R
  }

  output = list("r" = r, #r stats
                "rlow" = rlow,
                "rhigh" = rhigh,
                "R2" = rsq, #R squared stats
                "R2low" = limits$Lower.Conf.Limit.R2,
                "R2high" = limits$Upper.Conf.Limit.R2,
                "se" = se,
                "n" = n, #sample stats
                "dfm" = 1, #sig stats
                "dfe" = (n - 2),
                "t" = t,
                "F" = Fvalue,
                "p" = p)

  return(output)
}
