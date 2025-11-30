#' Cohen's d for Paired t Using the SD of Difference Scores
#'
#' **Note on function and output names:**
#' This effect size is now implemented with the snake_case function name
#' `d_dep_t_diff()` to follow modern R style guidelines. The original
#' dotted version `d.dep.t.diff()` is still available as a wrapper for
#' backward compatibility, and both functions return the same list. The
#' returned object includes both the original element names (e.g.,
#' `mdiff`, `Mlow`, `Mhigh`, `sddiff`) and newer snake_case aliases
#' (e.g., `m_diff`, `m_diff_lower_limit`, `m_diff_upper_limit`, `sd_diff`).
#' New code should prefer `d_dep_t_diff()` and the snake_case output
#' names, but existing code using the older names will continue to work.
#'
#' Compute Cohen's \eqn{d_z} and a noncentral-t confidence interval for
#' repeated-measures (paired-samples) designs using the **standard deviation
#' of the difference scores** as the denominator.
#'
#' @details
#' The effect size is defined as:
#' \deqn{d_z = \frac{\bar{X}_D}{s_D}}
#' where \eqn{\bar{X}_D} is the mean of the difference scores and \eqn{s_D} is
#' the standard deviation of the difference scores.
#'
#' The corresponding t statistic for the paired-samples t-test is:
#' \deqn{t = \frac{\bar{X}_D}{s_D / \sqrt{n}}}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdiffm.html}{Learn more on our example page.}
#'
#' @param mdiff Mean of the difference scores.
#' @param sddiff Standard deviation of the difference scores.
#' @param n Sample size (number of paired observations).
#' @param a Significance level (alpha) for the confidence interval.
#' Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_z}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)}
#' confidence interval for \eqn{d_z}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)}
#' confidence interval for \eqn{d_z}.}
#'   \item{mdiff}{Mean difference score.}
#'   \item{Mlow, Mhigh}{Confidence interval bounds for the mean difference.}
#'   \item{sddiff}{Standard deviation of the difference scores.}
#'   \item{se}{Standard error of the difference scores.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting
#' \eqn{d_z} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting
#' the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, dependent t-test, cohen's d, repeated measures
#' @import stats
#' @export
#'
#' @examples
#' # Example derived from the "dept_data" dataset included in MOTE
#'
#' # Suppose seven people completed a measure of belief in the supernatural
#' # before and after watching a sci-fi movie.
#' # Higher scores indicate stronger belief.
#'
#' t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' # Direct entry of summary statistics:
#' d_dep_t_diff(mdiff = 1.14, sddiff = 2.12, n = 7, a = .05)
#'
#' # Equivalent shorthand:
#' d_dep_t_diff(1.14, 2.12, 7, .05)
#'
#' # Using raw data from the dataset:
#' d_dep_t_diff(mdiff = mean(dept_data$before - dept_data$after),
#'              sddiff = sd(dept_data$before - dept_data$after),
#'              n = length(dept_data$before),
#'              a = .05)

d_dep_t_diff <- function(mdiff, sddiff, n, a = .05) {

  if (missing(mdiff)) {
    stop("Be sure to include the mean difference score mdiff.")
  }

  if (missing(sddiff)) {
    stop("Be sure to include the standard deviation 
    of the difference scores sddiff.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size n.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d_value <- mdiff / sddiff
  se_diff <- sddiff / sqrt(n)
  t_value <- mdiff / se_diff

  ncp_limits <- noncentral_t(
    ncp        = t_value,
    df         = n - 1,
    conf_level = 1 - a,
    sup_int_warns = TRUE
  )

  d_low <- ncp_limits$lower_limit / sqrt(n)
  d_high <- ncp_limits$upper_limit / sqrt(n)

  m_diff_lower <- mdiff - se_diff * qt(a / 2, n - 1, lower.tail = FALSE)
  m_diff_upper <- mdiff + se_diff * qt(a / 2, n - 1, lower.tail = FALSE)

  p_value <- pt(abs(t_value), n - 1, lower.tail = FALSE) * 2

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$d_z$ = ", apa(d_value, 2, TRUE), ", ", (1 - a) * 100,
    "\\% CI [", apa(d_low, 2, TRUE), ", ", apa(d_high, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$t$(", (n - 1), ") = ", apa(t_value, 2, TRUE), ", $p$ ", report_p,
    sep = ""
  )

  output_list <- list(
    # Original names (for backward compatibility)
    d      = d_value,
    dlow   = d_low,
    dhigh  = d_high,
    mdiff  = mdiff,
    m_diff = mdiff,
    Mlow   = m_diff_lower,
    Mhigh  = m_diff_upper,
    sddiff = sddiff,
    se     = se_diff,
    n      = n,
    df     = n - 1,

    # Snake_case sample stats
    sample_size       = n,
    degrees_freedom   = n - 1,

    t      = t_value,
    p      = p_value,
    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases (preferred for new code)
    d_lower_limit        = d_low,
    d_upper_limit        = d_high,
    m_diff               = mdiff,
    m_diff_lower_limit   = m_diff_lower,
    m_diff_upper_limit   = m_diff_upper,
    sd_diff              = sddiff,
    se_diff              = se_diff,
    t_value              = t_value,
    p_value              = p_value
  )

  return(output_list)
}

# Backward-compatibility wrapper
#' @rdname d_dep_t_diff
#' @export
d.dep.t.diff <- function(mdiff, sddiff, n, a = .05) { # nolint
  d_dep_t_diff(mdiff = mdiff, sddiff = sddiff, n = n, a = a)
}
