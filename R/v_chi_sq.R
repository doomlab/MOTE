#' V for Chi-Square
#'
#' This function displays \eqn{V} and its non-central confidence interval
#' for the specified \eqn{\chi^2} statistic.
#'
#' \eqn{V} is calculated by finding the square root of \eqn{\chi^2}
#' divided by the product of the sample size and the smaller of the
#' two degrees of freedom.
#'
#' \deqn{V = \sqrt{\frac{\chi^2}{n \times df_{\mathrm{small}}}}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/chiv.html}{Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `v_chi_sq()` to follow modern R style
#' guidelines. The original dotted version `v.chi.sq()` is still available as
#' a wrapper for backward compatibility, and both functions return the same
#' list. The returned object includes both the original element names (e.g.,
#' `v`, `vlow`, `vhigh`, `n`, `df`, `x2`, `p`, `estimate`, `statistic`) and
#' newer snake_case aliases (e.g., `v_value`, `v_lower_limit`, `v_upper_limit`,
#' `sample_size`, `df_total`, `chi_square`, `p_value`). New code should prefer
#' `v_chi_sq()` and the snake_case output names, but existing code using the
#' older names will continue to work.
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
#'   \item{estimate}{the \eqn{V} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{statistic}{the \eqn{\chi^2} statistic in APA style for
#' markdown printing}
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
#' v_chi_sq(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' v.chi.sq(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05)
#'
v_chi_sq <- function(x2, n, r, c, a = .05) {

  if (missing(x2)) {
    stop("Be sure to include chi-square statistic value.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size.")
  }

  if (missing(r)) {
    stop("Be sure to include number of rows.")
  }

  if (missing(c)) {
    stop("Be sure to include number of columns")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  df_small <- min(r - 1, c - 1)

  v_value <- sqrt(x2 / (n * df_small))

  df_total <- (r - 1) * (c - 1)

  ncp_limits <- noncentral_x(
    chi_square       = x2,
    df      = df_total,
    conf_level = (1 - a)
  )

  v_lower <- sqrt((ncp_limits$lower_limit + df_total) / (n * df_small))
  v_upper <- sqrt((ncp_limits$upper_limit + df_total) / (n * df_small))

  p_value <- pchisq(x2, df_total, lower.tail = FALSE)

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$V$ = ", apa(v_value, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(v_lower, 2, TRUE), ", ", apa(v_upper, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$\\chi^2$(", df_total, ") = ", apa(x2, 2, TRUE), ", $p$ ",
    report_p,
    sep = ""
  )

  output <- list(
    # Legacy names
    v         = v_value,
    vlow      = v_lower,
    vhigh     = v_upper,
    n         = n,
    df        = df_total,
    x2        = x2,
    p         = p_value,
    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases
    v_value        = v_value,
    v_lower_limit  = v_lower,
    v_upper_limit  = v_upper,
    sample_size    = n,
    df_total       = df_total,
    df_small       = df_small,
    chi_square     = x2,
    p_value        = p_value
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname v_chi_sq
#' @export
v.chi.sq <- function(x2, n, r, c, a = .05) { # nolint
  v_chi_sq(x2 = x2, n = n, r = r, c = c, a = a)
}
