#' V for Chi-Square
#'
#' This function displays \eqn{V} and its non-central confidence interval
#' for the specified \eqn{\chi^2} statistic.
#'
#' \eqn{V} is calculated by finding the square root of \eqn{\chi^2} divided by the product
#' of the sample size and the smaller of the two degrees of freedom.
#'
#' \deqn{V = \sqrt{\frac{\chi^2}{n \times df_{\mathrm{small}}}}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/chiv.html}{Learn more on our example page.}
#'
#' @param x2 chi-square statistic
#' @param n sample size
#' @param r number of rows in the contingency table
#' @param c number of columns in the contingency table
#' @param a significance level
#' @return \describe{
#'   \item{v}{\eqn{V} statistic}
#'   \item{vlow}{lower level confidence interval of \eqn{V}}
#'   \item{vhigh}{upper level confidence interval of \eqn{V}}
#'   \item{n}{sample size}
#'   \item{df}{degrees of freedom}
#'   \item{x2}{\eqn{\chi^2} statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{V} statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{\chi^2} statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, chi-square
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "chisq_data"
#' # dataset, included in the MOTE library.
#'
#' # Individuals were polled about their number of friends (low, medium, high)
#' # and their number of kids (1, 2, 3+) to determine if there was a
#' # relationship between friend groups and number of children, as we
#' # might expect that those with more children may have less time for
#' # friendship maintaining activities.
#'
#' chisq.test(chisq_data$kids, chisq_data$friends)
#'
#' v.chi.sq(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05)
#'
#' # Please note, if you see a warning, that implies the lower effect should
#' # be zero, as noted.

v.chi.sq <- function (x2, n, r, c, a = .05) {

  if (missing(x2)){
    stop("Be sure to include chi-square statistic value.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size.")
  }

  if (missing(r)){
    stop("Be sure to include number of rows.")
  }

  if (missing(c)){
    stop("Be sure to include number of columns")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  dfsmall <- min(r - 1, c - 1)
  v <- sqrt(x2 / (n * dfsmall))
  dftotal <- (r - 1) * (c - 1)
  ncpboth <- noncentral_x(x2, df = dftotal, conf.level = (1 - a))
  vlow <- sqrt((ncpboth$Lower.Limit + dftotal) / (n * dfsmall))
  vhigh <- sqrt((ncpboth$Upper.Limit + dftotal) / (n * dfsmall))
  p <- pchisq(x2, dftotal, lower.tail = FALSE)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("v" = v, #v stats
                "vlow" = vlow,
                "vhigh" = vhigh,
                "n" = n, #sample stats
                "df" = dftotal,
                "x2" = x2, #sig stats,
                "p" = p,
                "estimate" = paste("$V$ = ", apa(v,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(vlow,2,TRUE), ", ", apa(vhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$\\chi^2$(", dftotal, ") = ", apa(x2,2,TRUE), ", $p$ ",
                                    reportp, sep = ""))

  return(output)
}
