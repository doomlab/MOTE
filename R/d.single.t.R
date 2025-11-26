#' Cohen's d for One-Sample t from Summary Stats
#'
#' Compute Cohen's \eqn{d} and a noncentral-t confidence interval for a
#' one-sample (single) t-test using summary statistics.
#'
#' @details
#' The effect size is defined as the standardized mean difference between the
#' sample mean and the population/reference mean:
#' \deqn{d = \frac{m - \mu}{s}.}
#'
#' The corresponding t-statistic is:
#' \deqn{t = \frac{m - \mu}{s/\sqrt{n}}.}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/singletm.html}{Learn more on our example page.}
#'
#' @param m Sample mean.
#' @param u Population (reference) mean \eqn{\mu}.
#' @param sd Sample standard deviation \eqn{s}.
#' @param n Sample size \eqn{n}.
#' @param a Significance level (alpha) for the confidence interval. Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d}.}
#'   \item{m}{Sample mean.}
#'   \item{sd}{Sample standard deviation.}
#'   \item{se}{Standard error of the mean.}
#'   \item{Mlow, Mhigh}{Confidence interval bounds for the mean.}
#'   \item{u}{Population (reference) mean.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting \eqn{d} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, single t, one-sample, population mean, sample mean
#' @import stats
#' @export
#'
#' @examples
#' # Example derived from the "singt_data" dataset included in MOTE.
#'
#' # A school claims their gifted/honors program outperforms the national
#' # average (1080). Their students' SAT scores (sample) have mean 1370 and
#' # SD 112.7.
#'
#'     gift <- t.test(singt_data$SATscore, mu = 1080, alternative = "two.sided")
#'
#' # Direct entry of summary statistics:
#'     d.single.t(m = 1370, u = 1080, sd = 112.7, n = 14, a = .05)
#'
#' # Equivalent shorthand:
#'     d.single.t(1370, 1080, 112.7, 14, .05)
#'
#' # Using values from the t-test object and dataset:
#'     d.single.t(gift$estimate, gift$null.value,
#'                sd(singt_data$SATscore), length(singt_data$SATscore), .05)

d.single.t = function (m, u, sd, n, a = .05) {

  if (missing(m)){
    stop("Be sure to include m for the sample mean.")
  }

  if (missing(u)){
    stop("Be sure to include u for the population mean.")
  }

  if (missing(sd)){
    stop("Be sure to include sd for the sample mean.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ncpboth = noncentral_t(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = m - se*qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh = m + se*qt(a / 2, n - 1, lower.tail = FALSE)
  p = pt(abs(t), n - 1, lower.tail = FALSE)*2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "m" = m, #mean stats
                "sd" = sd,
                "se" = se,
                "Mlow" = Mlow,
                "Mhigh" = Mhigh,
                "u" = u,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats,
                "p" = p,
                "estimate" = paste("$d$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n-1), ") = ", apa(t,2,TRUE), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}
