#' Cohen's d from t for Independent Samples (Pooled SD)
#'
#' Compute Cohen's \eqn{d_s} from an independent-samples t-statistic and provide a
#' noncentral-t confidence interval, assuming equal variances (pooled SD).
#'
#' @details
#' For between-subjects designs with pooled SD, \eqn{d_s} can be obtained directly
#' from the t-statistic:
#' \deqn{d_s = \frac{2t}{\sqrt{n_1 + n_2 - 2}},}
#' where \eqn{n_1} and \eqn{n_2} are the group sample sizes (df = \eqn{n_1 + n_2 - 2}).
#' The \eqn{(1-\alpha)} confidence interval for \eqn{d_s} is derived from the
#' noncentral t distribution for the observed \eqn{t} and df.
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/indtt.html}{Learn more on our example page.}
#'
#' @param t t-statistic from an independent-samples t-test.
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level (alpha) for the confidence interval. Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_s}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_s}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_s}.}
#'   \item{n1, n2}{Group sample sizes.}
#'   \item{df}{Degrees of freedom (\eqn{n_1 + n_2 - 2}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting \eqn{d_s} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, independent t-test, pooled standard deviation
#' @import stats
#' @export
#'
#' @examples
#' # The following example is derived from the "indt_data" dataset in MOTE.
#'
#'     hyp <- t.test(correctq ~ group, data = indt_data)
#'
#' # Direct entry of the t-statistic and sample sizes:
#'     d.ind.t.t(t = -2.6599, n1 = 4, n2 = 4, a = .05)
#'
#' # Equivalent shorthand:
#'     d.ind.t.t(-2.6599, 4, 4, .05)
#'
#' # Using the t-statistic from the model object:
#'     d.ind.t.t(hyp$statistic, length(indt_data$group[indt_data$group == 1]),
#'               length(indt_data$group[indt_data$group == 2]), .05)

d.ind.t.t <- function (t, n1, n2, a = .05) {

  if (missing(t)){
    stop("Be sure to include the t-value found from your t-test.")
  }

  if (missing(n1)){
    stop("Be sure to include the sample size n1 for group 1.")
  }

  if (missing(n2)){
    stop("Be sure to include the sample size n2 for group 2.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- 2 * t / sqrt(n1 + n2 - 2)
  ncpboth <- noncentral_t(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = FALSE) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p,
                "estimate" = paste("$d_s$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n1-1+n2-1), ") = ",
                                    apa(t,2,TRUE), ", $p$ ", reportp, sep = "")
                )

  return(output)
}


