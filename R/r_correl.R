#' r to Coefficient of Determination (R\eqn{^2}) from F
#'
#' This function displays the transformation from \eqn{r} to
#' \eqn{R^2} to calculate the non-central confidence interval
#' for \eqn{R^2} using the \eqn{F} distribution.
#'
#' The \eqn{t}-statistic is calculated by:
#' \deqn{t = \frac{r}{\sqrt{\frac{1 - r^2}{n - 2}}}}
#'
#' The \eqn{F}-statistic is the \eqn{t}-statistic squared:
#' \deqn{F = t^2}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/rcorrel.html}{Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `r_correl()` to follow modern R style
#' guidelines. The original dotted version `r.correl()` is still available as
#' a wrapper for backward compatibility, and both functions return the same
#' list. The returned object includes both the original element names (e.g.,
#' `r`, `rlow`, `rhigh`, `R2`, `R2low`, `R2high`, `se`, `n`, `dfm`, `dfe`,
#' `t`, `F`, `p`, `estimate`, `estimateR2`, `statistic`) and newer snake_case
#' aliases (e.g., `r_value`, `r_lower_limit`, `r_upper_limit`, `r2_value`,
#' `r2_lower_limit`, `r2_upper_limit`, `standard_error`, `sample_size`,
#' `df_model`, `df_error`, `t_value`, `f_value`, `p_value`). New code should
#' prefer `r_correl()` and the snake_case output names, but existing code
#' using the older names will continue to work.
#'
#' @param r correlation coefficient
#' @param n sample size
#' @param a significance level
#' @return \describe{
#'   \item{r}{correlation coefficient}
#'   \item{rlow}{lower level confidence interval for \eqn{r}}
#'   \item{rhigh}{upper level confidence interval for \eqn{r}}
#'   \item{R2}{coefficient of determination}
#'   \item{R2low}{lower level confidence interval of \eqn{R^2}}
#'   \item{R2high}{upper level confidence interval of \eqn{R^2}}
#'   \item{se}{standard error}
#'   \item{n}{sample size}
#'   \item{dfm}{degrees of freedom of mean}
#'   \item{dfe}{degrees of freedom of error}
#'   \item{t}{\eqn{t}-statistic}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{r} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{estimateR2}{the \eqn{R^2} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{statistic}{the \eqn{t}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, correlation
#' @import stats
#' @export
#' @examples
#'
#' # This example is derived from the mtcars dataset provided in R.
#'
#' # What is the correlation between miles per gallon and car weight?
#'
#' cor.test(mtcars$mpg, mtcars$wt)
#'
#' r_correl(r = -0.8676594, n = 32, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' r.correl(r = -0.8676594, n = 32, a = .05)

r_correl <- function(r, n, a = .05) {

  if (missing(r)) {
    stop("Be sure to include the correlation r.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  r_squared <- r^2

  standard_error <- sqrt(
    4 * r_squared * (1 - r_squared)^2 * (n - 3)^2 /
      ((n^2 - 1) * (3 + n))
  )

  t_value <- r / sqrt((1 - r_squared) / (n - 2))
  f_value <- t_value^2
  df_model <- 1
  df_error <- n - 2

  r2_limits <- ci_r2(
    r2 = r_squared,
    df1 = df_model,
    df2 = df_error,
    conf_level = (1 - a)
  )

  r_limits <- ci_r(
    r = abs(r),
    df1 = df_model,
    df2 = df_error,
    conf_level = (1 - a)
  )

  p_value <- pf(f_value, df_model, df_error, lower.tail = FALSE)

  # deal with negative r values
  if (r < 0) {
    r_lower <- 0 - r_limits$lower_conf_limit_r
    r_upper <- 0 - r_limits$upper_conf_limit_r
  } else {
    r_lower <- r_limits$lower_conf_limit_r
    r_upper <- r_limits$upper_conf_limit_r
  }

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, TRUE), sep = "")
  }

  estimate_r <- paste(
    "$r$ = ", apa(r, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(r_lower, 2, TRUE), ", ", apa(r_upper, 2, TRUE), "]",
    sep = ""
  )

  estimate_r2 <- paste(
    "$R^2$ = ", apa(r_squared, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(r2_limits$lower_conf_limit_r2, 2, TRUE), ", ",
    apa(r2_limits$upper_conf_limit_r2, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$t$(", df_error, ") = ", apa(t_value, 2, TRUE), ", $p$ ",
    report_p,
    sep = ""
  )

  output <- list(
    # Legacy names
    r          = r,
    rlow       = r_lower,
    rhigh      = r_upper,
    R2         = r_squared,
    R2low      = r2_limits$lower_conf_limit_r2,
    R2high     = r2_limits$upper_conf_limit_r2,
    se         = standard_error,
    n          = n,
    dfm        = df_model,
    dfe        = df_error,
    t          = t_value,
    F          = f_value,
    p          = p_value,
    estimate   = estimate_r,
    estimateR2 = estimate_r2,
    statistic  = statistic,

    # Snake_case aliases
    r_value          = r,
    r_lower_limit    = r_lower,
    r_upper_limit    = r_upper,
    r2_value         = r_squared,
    r2_lower_limit   = r2_limits$lower_conf_limit_r2,
    r2_upper_limit   = r2_limits$upper_conf_limit_r2,
    standard_error   = standard_error,
    sample_size      = n,
    df_model         = df_model,
    df_error         = df_error,
    t_value          = t_value,
    f_value          = f_value,
    p_value          = p_value
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname r_correl
#' @export
r.correl <- function(r, n, a = .05) { # nolint
  r_correl(r = r, n = n, a = a)
}
