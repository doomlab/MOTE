#' r and Coefficient of Determination (R2) from d
#'
#' **Note on function and output names:** This effect size translation is now
#' implemented with the snake_case function name `d_to_r()` to follow modern
#' R style guidelines. The original dotted version `d.to.r()` is still
#' available as a wrapper for backward compatibility, and both functions
#' return the same list. The returned object includes both the original
#' element names (e.g., `r`, `rlow`, `rhigh`, `R2`, `R2low`, `R2high`, `se`,
#' `n`, `dfm`, `dfe`, `t`, `F`, `p`, `estimate`, `estimateR2`, `statistic`)
#' and newer snake_case aliases (e.g., `r_lower_limit`, `r_upper_limit`,
#' `r2_value`, `r2_lower_limit`, `r2_upper_limit`, `se_value`,
#' `sample_size`, `degrees_freedom_model`, `degrees_freedom_error`,
#' `t_value`, `f_value`, `p_value`, `estimate_r`, `estimate_r2`). New code
#' should prefer `d_to_r()` and the snake_case output names, but existing
#' code using the older names will continue to work.
#'
#' Calculates r from d and then translates r to r2 to calculate
#' the non-central confidence interval for r2 using the F distribution.
#'
#' The correlation coefficient (\eqn{r}) is calculated by dividing Cohen's d
#' by the square root of the total sample size squared, divided
#' by the product of the sample sizes of group one and group two.
#'
#' \deqn{r = \frac{d}{\sqrt{d^2 + \frac{(n_1 + n_2)^2}{n_1 n_2}}}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/dtor.html}{Learn more on our example page.}
#'
#' @param d Effect size statistic.
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level.
#' @return Provides the effect size (correlation coefficient) with associated
#' confidence intervals, the t-statistic, F-statistic, and other estimates
#' appropriate for d to r translation. Note this CI is not based on the
#' traditional r-to-z transformation but rather non-central F using the
#' ci.R function from MBESS.
#'
#' \describe{
#'   \item{r}{Correlation coefficient.}
#'   \item{rlow}{Lower level confidence interval for r.}
#'   \item{rhigh}{Upper level confidence interval for r.}
#'   \item{R2}{Coefficient of determination.}
#'   \item{R2low}{Lower level confidence interval of R2.}
#'   \item{R2high}{Upper level confidence interval of R2.}
#'   \item{se}{Standard error.}
#'   \item{n}{Sample size.}
#'   \item{dfm}{Degrees of freedom of mean.}
#'   \item{dfe}{Degrees of freedom error.}
#'   \item{t}{t-statistic.}
#'   \item{F}{F-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{The r statistic and confidence interval in
#' APA style for markdown printing.}
#'   \item{estimateR2}{The R\eqn{^2} statistic and confidence
#' interval in APA style for markdown printing.}
#'   \item{statistic}{The t-statistic in APA style for markdown printing.}
#' }
#'
#' @keywords effect size, correlation
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "indt_data"
#' # dataset, included in the MOTE library.
#'
#' # A forensic psychologist conducted a study to examine whether
#' # being hypnotized during recall affects how well a witness
#' # can remember facts about an event. Eight participants
#' # watched a short film of a mock robbery, after which
#' # each participant was questioned about what he or she had
#' # seen. The four participants in the experimental group
#' # were questioned while they were hypnotized. The four
#' # participants in the control group received the same
#' # questioning without hypnosis.
#'
#' # Contrary to the hypothesized result, the group that underwent
#' # hypnosis were significantly less accurate while reporting
#' # facts than the control group with a large effect size, t(6) = -2.66,
#' # p = .038, d_s = -1.88.
#'
#'      d_to_r(d = -1.88, n1 = 4, n2 = 4, a = .05)


d_to_r <- function(d, n1, n2, a = .05) {

  if (missing(d)) {
    stop("Be sure to include d effect size.")
  }

  if (missing(n1)) {
    stop("Be sure to include the sample size n1 for the first group.")
  }

  if (missing(n2)) {
    stop("Be sure to include the sample size n2 for the second group.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  correct <- (n1 + n2)^2 / (n1 * n2)
  n <- n1 + n2
  r <- d / sqrt(d^2 + correct)
  rsq <- (r) ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) *
               ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  f_value <- t ^ 2
  dfm <- 1
  dfe <- n - 2

  limits <- ci_r2(r2 = rsq, df1 = dfm, df2 = dfe, conf_level = (1 - a))
  ciforr <- ci_r(r = abs(r), df1 = dfm, df2 = dfe, conf_level = (1 - a))
  p <- pf(f_value, dfm, dfe, lower.tail = FALSE)

  #deal with negative r / d values
  if (r < 0) {
    rlow <- 0 - ciforr$lower_conf_limit_r
    rhigh <- 0 - ciforr$upper_conf_limit_r
  } else {
    rlow <- ciforr$lower_conf_limit_r
    rhigh <- ciforr$upper_conf_limit_r
  }

  if (p < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p, 3, FALSE), sep = "")
  }

  estimate_r <- paste(
    "$r$ = ", apa(r, 2, FALSE), ", ", (1 - a) * 100, "\\% CI [",
    apa(rlow, 2, FALSE), ", ", apa(rhigh, 2, FALSE), "]",
    sep = ""
  )

  estimate_r2 <- paste(
    "$R^2$ = ", apa(rsq, 2, FALSE), ", ", (1 - a) * 100, "\\% CI [",
    apa(limits$lower_conf_limit_r2, 2, FALSE), ", ",
    apa(limits$upper_conf_limit_r2, 2, FALSE), "]",
    sep = ""
  )

  statistic <- paste(
    "$t$(", (n - 2), ") = ", apa(t, 2, TRUE), ", $p$ ", reportp,
    sep = ""
  )

  output <- list(
    # r statistics (legacy names)
    r     = r,
    rlow  = rlow,
    rhigh = rhigh,

    # r statistics (snake_case aliases)
    r_lower_limit = rlow,
    r_upper_limit = rhigh,

    # R-squared statistics (legacy)
    R2    = rsq,
    R2low = limits$lower_conf_limit_r2,
    R2high = limits$upper_conf_limit_r2,

    # R-squared statistics (snake_case aliases)
    r2_value       = rsq,
    r2_lower_limit = limits$lower_conf_limit_r2,
    r2_upper_limit = limits$upper_conf_limit_r2,

    # Standard error and sample size
    se = se,
    se_value   = se,
    n  = n,
    sample_size = n,

    # Degrees of freedom
    dfm = dfm,
    dfe = dfe,
    degrees_freedom_model = dfm,
    degrees_freedom_error = dfe,

    # Test statistics
    t = t,
    F = f_value,
    p = p,

    # Snake_case statistics
    t_value = t,
    f_value = f_value,
    p_value = p,

    # APA-style strings
    estimate    = estimate_r,
    estimateR2  = estimate_r2,
    estimate_r  = estimate_r,
    estimate_r2 = estimate_r2,
    statistic   = statistic
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname d_to_r
#' @export
 d.to.r <- function(d, n1, n2, a = .05) { # nolint
   d_to_r(d = d, n1 = n1, n2 = n2, a = a)
 }
