#' Odds Ratio from 2x2 Table
#'
#' This function displays odds ratios and their normal confidence intervals.
#' This statistic is calculated as (level 1.1/level 1.2) /
#' (level 2.1/level 2.2), which can be considered the odds of
#' level 1.1 given level1 overall versus level2.1 given level2 overall.
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `odds_ratio()` to follow modern R style
#' guidelines. The original name `odds()` is still available as a wrapper for
#' backward compatibility, and both functions return the same list. The returned
#' object includes both the original element names (e.g., `odds`, `olow`,
#' `ohigh`, `se`) and newer snake_case aliases (e.g., `odds_value`,
#' `odds_lower_limit`, `odds_upper_limit`, `standard_error`). New code should
#' prefer `odds_ratio()` and the snake_case output names, but existing code
#' using the older names will continue to work.
#'
#'
#' @param n11 sample size for level 1.1
#' @param n12 sample size for level 1.2
#' @param n21 sample size for level 2.1
#' @param n22 sample size for level 2.2
#' @param a significance level
#' @return \describe{
#'   \item{odds}{odds ratio statistic (legacy name; see also `odds_value`)}
#'   \item{olow}{lower-level confidence interval of odds ratio
#' (legacy name; see also `odds_lower_limit`)}
#'   \item{ohigh}{upper-level confidence interval of odds ratio
#' (legacy name; see also `odds_upper_limit`)}
#'   \item{se}{standard error (legacy name; see also `standard_error`)}
#'   \item{odds_value}{odds ratio statistic (snake_case alias of `odds`)}
#'   \item{odds_lower_limit}{lower-level confidence interval of odds
#' ratio (alias of `olow`)}
#'   \item{odds_upper_limit}{upper-level confidence interval of odds
#' ratio (alias of `ohigh`)}
#'   \item{standard_error}{standard error (alias of `se`)}
#' }
#'
#' @keywords effect size odds ratios
#' @import stats
#' @export
#' @examples
#'
#' # A health psychologist was interested in the rates of anxiety in
#' # first generation and regular college students. They polled campus
#' # and found the following data:
#'
#'   # |              | First Generation | Regular |
#'   # |--------------|------------------|---------|
#'   # | Low Anxiety  | 10               | 50      |
#'   # | High Anxiety | 20               | 15      |
#'
#' # What are the odds for the first generation students to have anxiety?
#'
#' odds_ratio(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = .05)
#'
#' # Backwards-compatible wrapper (deprecated name)
#' odds(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = .05)


odds_ratio <- function(n11, n12, n21, n22, a = .05) {

  if (missing(n11)) {
    stop("Be sure to include the sample size for row 1 and column 1.")
  }

  if (missing(n12)) {
    stop("Be sure to include the sample size for row 1 and column 2.")
  }

  if (missing(n21)) {
    stop("Be sure to include the sample size for row 2 and column 1.")
  }

  if (missing(n22)) {
    stop("Be sure to include the sample size for row 2 and column 2.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  odds_value <- (n11 / n12) / (n21 / n22)

  standard_error <- sqrt(
    (1 / n11) +
      (1 / n12) +
      (1 / n21) +
      (1 / n22)
  )

  z_crit <- qnorm(a / 2, lower.tail = FALSE)

  odds_lower_limit <- exp(log(odds_value) - z_crit * standard_error)
  odds_upper_limit <- exp(log(odds_value) + z_crit * standard_error)

  output <- list(
    # Legacy names
    odds = odds_value,
    olow = odds_lower_limit,
    ohigh = odds_upper_limit,
    se = standard_error,

    # Snake_case aliases
    odds_value = odds_value,
    odds_lower_limit = odds_lower_limit,
    odds_upper_limit = odds_upper_limit,
    standard_error = standard_error
  )

  return(output)

}

# Backward compatibility wrapper
#' @rdname odds_ratio
#' @export
odds <- function(n11, n12, n21, n22, a = .05) { # nolint
  odds_ratio(n11 = n11, n12 = n12, n21 = n21, n22 = n22, a = a)
}
