#' Cohen's d for One-Sample t from Summary Stats
#'
#' **Note on function and output names:**
#' This effect size is now implemented with the snake_case function name
#' `d_single_t()` to follow modern R style guidelines. The original dotted
#' version `d.single.t()` is still available as a wrapper for backward
#' compatibility, and both functions return the same list. The returned
#' object includes both the original element names (e.g., `d`, `dlow`,
#' `dhigh`, `m`, `sd`, `se`, `Mlow`, `Mhigh`, `u`, `n`, `df`, `t`, `p`,
#' `estimate`, `statistic`) and newer snake_case aliases (e.g.,
#' `d_lower_limit`, `d_upper_limit`, `mean_value`, `sd_value`,
#' `se_value`, `mean_lower_limit`, `mean_upper_limit`, `population_mean`,
#' `sample_size`, `degrees_freedom`, `t_value`, `p_value`).
#' New code should prefer `d_single_t()` and the snake_case output
#' names, but existing code using the older names will continue to work.
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
#'   \item{m}{Sample mean.}
#'   \item{sd}{Sample standard deviation.}
#'   \item{se}{Standard error of the mean.}
#'   \item{Mlow, Mhigh}{Confidence interval bounds for the mean.}
#'   \item{u}{Population (reference) mean.}
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
#'     d_single_t(m = 1370, u = 1080, sd = 112.7, n = 14, a = .05)
#'
#' # Equivalent shorthand:
#'     d_single_t(1370, 1080, 112.7, 14, .05)
#'
#' # Using values from the t-test object and dataset:
#'     d_single_t(gift$estimate, gift$null.value,
#'                sd(singt_data$SATscore), length(singt_data$SATscore), .05)
d_single_t <- function(m, u, sd, n, a = .05) {

  if (missing(m)) {
    stop("Be sure to include m for the sample mean.")
  }

  if (missing(u)) {
    stop("Be sure to include u for the population mean.")
  }

  if (missing(sd)) {
    stop("Be sure to include sd for the sample mean.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ncp_limits <- noncentral_t(
    ncp          = t,
    df           = n - 1,
    conf_level   = 1 - a,
    sup_int_warns = TRUE
  )

  dlow <- ncp_limits$lower_limit / sqrt(n)
  dhigh <- ncp_limits$upper_limit / sqrt(n)
  m_low <- m - se * qt(a / 2, n - 1, lower.tail = FALSE)
  m_high <- m + se * qt(a / 2, n - 1, lower.tail = FALSE)
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
    # Original names (backward compatible)
    d   = d,
    dlow  = dlow,
    dhigh = dhigh,

    m   = m,
    sd  = sd,
    se  = se,
    Mlow  = m_low,
    Mhigh = m_high,

    u   = u,
    n   = n,
    df  = n - 1,

    t   = t,
    p   = p,

    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases
    d_lower_limit     = dlow,
    d_upper_limit     = dhigh,

    mean_value        = m,
    sd_value          = sd,
    se_value          = se,
    mean_lower_limit  = m_low,
    mean_upper_limit  = m_high,

    population_mean   = u,
    sample_size       = n,
    degrees_freedom   = n - 1,

    t_value           = t,
    p_value           = p
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname d_single_t
#' @export
d.single.t <- function(m, u, sd, n, a = .05) { # nolint
  d_single_t(m = m, u = u, sd = sd, n = n, a = a)
}
