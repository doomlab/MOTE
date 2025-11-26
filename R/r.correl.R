#' r to Coefficient of Determination (R\eqn{^2}) from F
#'
#' This function displays the transformation from \eqn{r} to \eqn{R^2} to calculate
#' the non-central confidence interval for \eqn{R^2} using the \eqn{F} distribution.
#'
#' The \eqn{t}-statistic is calculated by:
#' \deqn{t = \frac{r}{\sqrt{\frac{1 - r^2}{n - 2}}}}
#'
#' The \eqn{F}-statistic is the \eqn{t}-statistic squared:
#' \deqn{F = t^2}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/rcorrel.html}{Learn more on our example page.}
#'
#' @param r correlation coefficient
#' @param n sample size
#' @param a significance level
#' @return \describe{
#'   \item{r}{correlation coefficient}
#'   \item{rlow}{lower level confidence interval for \eqn{r}}
#'   \item{rhigh}{upper level confidence interval for \eqn{r}}
#'   \item{R2}{coefficient of determination}
#'   \item{R2low}{lower level confidence interval of \eqn{R^2}}
#'   \item{R2high}{upper level confidence interval of \eqn{R^2}}
#'   \item{se}{standard error}
#'   \item{n}{sample size}
#'   \item{dfm}{degrees of freedom of mean}
#'   \item{dfe}{degrees of freedom of error}
#'   \item{t}{\eqn{t}-statistic}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{r} statistic and confidence interval in APA style for markdown printing}
#'   \item{estimateR2}{the \eqn{R^2} statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{t}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, correlation
#' @import stats
#' @export
#' @examples
#'
#' # This example is derived from the mtcars dataset provided in R.
#'
#' # What is the correlation between miles per gallon and car weight?
#'
#' cor.test(mtcars$mpg, mtcars$wt)
#'
#' r.correl(r = -0.8676594, n = 32, a = .05)

r.correl <- function (r, n, a = .05) {

  if (missing(r)){
    stop("Be sure to include the correlation r.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  rsq <- (r) ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) * ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  Fvalue <- t ^ 2
  dfm <- 1
  dfe <- n - 2

  limits <- ci.R2(R2 = rsq, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  ciforr <- ci.R(R = abs(r), df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  p <- pf(Fvalue, dfm, dfe, lower.tail = FALSE)

  #deal with negative r / d values
  if (r < 0) {
    rlow = 0 - ciforr$Lower.Conf.Limit.R
    rhigh = 0 - ciforr$Upper.Conf.Limit.R
  } else {
    rlow = ciforr$Lower.Conf.Limit.R
    rhigh = ciforr$Upper.Conf.Limit.R
  }

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,TRUE), sep = "")}

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
                "p" = p,
                "estimate" = paste("$r$ = ", apa(r,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(rlow,2,TRUE), ", ", apa(rhigh,2,TRUE), "]", sep = ""),
                "estimateR2" = paste("$R^2$ = ", apa(rsq,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                     apa(limits$Lower.Conf.Limit.R2,2,TRUE), ", ",
                                     apa(limits$Upper.Conf.Limit.R2,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n-2), ") = ", apa(t,2,TRUE), ", $p$ ",
                                    reportp, sep = ""))

  return(output)
}

