#' Cohen's d from t for Independent Samples (Pooled SD)
#'
#' **Note on function and output names:**
#' This effect size is now implemented with the snake_case function name
#' `d_ind_t_t()` to follow modern R style guidelines. The original dotted
#' version `d.ind.t.t()` is still available as a wrapper for backward
#' compatibility, and both functions return the same list. The returned
#' object includes both the original element names (e.g., `d`, `dlow`,
#' `dhigh`, `n1`, `n2`, `df`, `t`, `p`, `estimate`, `statistic`) and
#' newer snake_case aliases (e.g., `d_lower_limit`, `d_upper_limit`,
#' `sample_size_1`, `sample_size_2`, `degrees_freedom`, `t_value`,
#' `p_value`). New code should
#' prefer `d_ind_t_t()` and the snake_case output names, but existing
#' code using the older names will continue to work.
#'
#' Compute Cohen's \eqn{d_s} from an independent-samples
#' t-statistic and provide a noncentral-t confidence interval,
#' assuming equal variances (pooled SD).
#'
#' @details
#' For between-subjects designs with pooled SD, \eqn{d_s} can
#' be obtained directly from the t-statistic:
#' \deqn{d_s = \frac{2t}{\sqrt{n_1 + n_2 - 2}},}
#' where \eqn{n_1} and \eqn{n_2} are the group sample sizes
#' (df = \eqn{n_1 + n_2 - 2}).
#' The \eqn{(1-\alpha)} confidence interval for \eqn{d_s} is derived from the
#' noncentral t distribution for the observed \eqn{t} and df.
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/indtt.html}
#' {Learn more on our example page.}
#'
#' @param t_value t-statistic from an independent-samples t-test.
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level (alpha) for the confidence interval.
#' Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_s}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence
#' interval for \eqn{d_s}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence
#' interval for \eqn{d_s}.}
#'   \item{n1, n2}{Group sample sizes.}
#'   \item{df}{Degrees of freedom (\eqn{n_1 + n_2 - 2}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting
#' \eqn{d_s} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting
#' the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, independent t-test, pooled standard deviation
#' @import stats
#' @export
#'
#' @examples
#' # The following example is derived from the "indt_data" dataset in MOTE.
#'     hyp <- t.test(correctq ~ group, data = indt_data)
#'
#' # Direct entry of the t-statistic and sample sizes:
#'     d_ind_t_t(t_value = -2.6599, n1 = 4, n2 = 4, a = .05)
#'
#' # Equivalent shorthand:
#'     d.ind.t.t(-2.6599, 4, 4, .05)
#'
#' # Using the t-statistic from the model object:
#'     d_ind_t_t(hyp$statistic, length(indt_data$group[indt_data$group == 1]),
#'               length(indt_data$group[indt_data$group == 2]), .05)

d_ind_t_t <- function(t_value, n1, n2, a = .05) {

  if (missing(t_value)) {
    stop("Be sure to include the t-value found from your t-test.")
  }

  if (missing(n1)) {
    stop("Be sure to include the sample size n1 for group 1.")
  }

  if (missing(n2)) {
    stop("Be sure to include the sample size n2 for group 2.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- 2 * t_value / sqrt(n1 + n2 - 2)

  ncp_limits <- noncentral_t(
    ncp        = t_value,
    df         = n1 + n2 - 2,
    conf_level = 1 - a,
    sup_int_warns = TRUE
  )

  dlow  <- ncp_limits$lower_limit / sqrt((n1 * n2) / (n1 + n2))
  dhigh <- ncp_limits$upper_limit / sqrt((n1 * n2) / (n1 + n2))

  p_value <- pt(abs(t_value), n1 + n2 - 2, lower.tail = FALSE) * 2

  if (p_value < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  output <- list(
    # Original names (backward compatible)
    d   = d,
    dlow  = dlow,
    dhigh = dhigh,
    n1    = n1,
    n2    = n2,
    df    = n1 + n2 - 2,
    t     = t_value,
    p     = p_value,
    estimate = paste(
      "$d_s$ = ", apa(d, 2, TRUE), ", ", (1 - a) * 100,
      "\\% CI [", apa(dlow, 2, TRUE), ", ", apa(dhigh, 2, TRUE), "]",
      sep = ""
    ),
    statistic = paste(
      "$t$(", n1 + n2 - 2, ") = ", apa(t_value, 2, TRUE),
      ", $p$ ", reportp,
      sep = ""
    ),

    # Snake_case aliases (new preferred names)
    d_lower_limit     = dlow,
    d_upper_limit     = dhigh,
    sample_size_1     = n1,
    sample_size_2     = n2,
    degrees_freedom   = n1 + n2 - 2,
    t_value           = t_value,
    p_value           = p_value
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname d_ind_t_t
#' @export
d.ind.t.t <- function(t, n1, n2, a = .05) { # nolint
  d_ind_t_t(t_value = t, n1 = n1, n2 = n2, a = a)
}
