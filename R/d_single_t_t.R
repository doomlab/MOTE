#' Cohen's d from t for One-Sample t-Test
#'
#' Compute Cohen's \eqn{d} and a noncentral-t confidence interval for a
#' one-sample (single) t-test using the observed t-statistic.
#'
#' @details
#' The effect size is calculated as:
#' \deqn{d = \frac{t}{\sqrt{n}},}
#' where \eqn{t} is the one-sample t-statistic and \eqn{n} is the sample size.
#'
#' The corresponding \eqn{(1 - \alpha)} confidence interval for \eqn{d} is
#' derived from the noncentral t distribution.
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/singlett.html}
#' {Learn more on our example page.}
#'
#' @param t t-test value.
#' @param n Sample size.
#' @param a Significance level (alpha) for the confidence interval.
#' Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence
#' interval for \eqn{d}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence
#' interval for \eqn{d}.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting
#' \eqn{d} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting
#' the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, single t, one-sample
#' @import stats
#' @export
#'
#' @examples
#' # A school has a gifted/honors program that they claim is
#' # significantly better than others in the country. The gifted/honors
#' # students in this school scored an average of 1370 on the SAT,
#' # with a standard deviation of 112.7, while the national average
#' # for gifted programs is a SAT score of 1080.
#'
#'     gift <- t.test(singt_data$SATscore, mu = 1080, alternative = "two.sided")
#'
#' # Direct entry of t-statistic and sample size:
#'     d_single_t_t(9.968, 15, .05)
#'
#' # Equivalent shorthand:
#'     d_single_t_t(9.968, 15, .05)
#'
#' # Using values from a t-test object and dataset:
#'     d_single_t_t(gift$statistic, length(singt_data$SATscore), .05)


d_single_t_t <- function(t, n, a = .05) {

  if (missing(t)) {
    stop("Be sure to include t from the t-test statistic.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- t / sqrt(n)
  ncp_limits <- noncentral_t(
    ncp = t,
    df = n - 1,
    conf_level = 1 - a,
    sup_int_warns = TRUE
  )
  dlow <- ncp_limits$lower_limit / sqrt(n)
  dhigh <- ncp_limits$upper_limit / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = FALSE) * 2

  if (p < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$d$ = ", apa(d, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(dlow, 2, TRUE), ", ", apa(dhigh, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$t$(", n - 1, ") = ", apa(t, 2, TRUE), ", $p$ ", reportp,
    sep = ""
  )

  output <- list(
    # legacy names
    d     = d,
    dlow  = dlow,
    dhigh = dhigh,

    n  = n,
    df = n - 1,

    t = t,
    p = p,

    estimate  = estimate,
    statistic = statistic,

    # snake_case aliases
    d_lower_limit = dlow,
    d_upper_limit = dhigh,

    sample_size     = n,
    degrees_freedom = n - 1,

    t_value = t,
    p_value = p
  )

  return(output)

}

# Backward compatibility wrapper
#' @rdname d_single_t_t
#' @export
d.single.t.t <- function(t, n, a = .05) { # nolint
  d_single_t_t(t = t, n = n, a = a)
}
