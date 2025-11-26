#' Cohen's d from t for Paired Samples Using the SD of Difference Scores
#'
#' Compute Cohen's \eqn{d_z} from a paired-samples t-statistic and provide a
#' noncentral-t confidence interval, using the **standard deviation of the
#' difference scores** as the denominator.
#'
#' @details
#' For paired designs, \eqn{d_z} can be obtained directly from the t-statistic:
#' \deqn{d_z = \frac{t}{\sqrt{n}},}
#' where \eqn{n} is the number of paired observations (df = \eqn{n-1}). The
#' \eqn{(1-\alpha)} confidence interval for \eqn{d_z} is derived from the
#' noncentral t distribution for the observed \eqn{t} and df.
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdifft.html}{Learn more on our example page.}
#'
#' @param t t-statistic from a paired-samples t-test.
#' @param n Sample size (number of paired observations).
#' @param a Significance level (alpha) for the confidence interval. Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_z}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_z}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_z}.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting \eqn{d_z} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, dependent t-test, paired sample, repeated measures, t-test
#' @import stats
#' @export
#'
#' @examples
#' # Example derived from the "dept_data" dataset included in MOTE
#'
#' # Suppose seven people completed a measure before and after an intervention.
#' # Higher scores indicate stronger endorsement.
#'
#'     scifi <- t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' # The t-test value was 1.43. You can type in the numbers directly,
#' # or refer to the dataset, as shown below.
#'
#'     d.dep.t.diff.t(t = 1.43, n = 7, a = .05)
#'
#'     d.dep.t.diff.t(1.43, 7, .05)
#'
#'     d.dep.t.diff.t(scifi$statistic, length(dept_data$before), .05)

d.dep.t.diff.t <- function (t, n, a = .05) {

  if (missing(t)){
    stop("Be sure to include your t-value from your dependent t-test.")
  }

  if (missing(n)){
    stop("Be sure to include your sample size value n.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- t / sqrt(n)
  ncpboth <- noncentral_t(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = FALSE) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats
                "p" = p,
                "estimate" = paste("$d_z$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n-1), ") = ", apa(t,2,TRUE), ", $p$ ", reportp, sep = "")
                )

  return(output)
}

