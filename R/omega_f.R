#' \eqn{\omega^2} for ANOVA from \eqn{F}
#'
#' This function displays \eqn{\omega^2} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial omega squared.
#'
#' Omega squared or partial omega squared is calculated by subtracting one
#' from the \eqn{F}-statistic and multiplying it by degrees of
#' freedom of the model. This is divided by the same value after
#' adding the number of valid responses. This value will be omega
#' squared for one-way ANOVA designs, and will be partial omega squared
#' for multi-way ANOVA designs (i.e. with more than one IV).
#'
#' \deqn{\omega^2 = \frac{df_m (F - 1)}{df_m (F - 1) + n}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegaf.html}{Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `omega_f()` to follow modern R style
#' guidelines. The original dotted version `omega.F()` is still available as a
#' wrapper for backward compatibility, and both functions return the same list.
#' The returned object includes both the original element names (e.g., `omega`,
#' `omegalow`, `omegahigh`, `dfm`, `dfe`, `F`, `p`, `estimate`, `statistic`)
#' and newer snake_case aliases (e.g., `omega_value`, `omega_lower_limit`,
#' `omega_upper_limit`, `df_model`, `df_error`, `f_value`, `p_value`). New code
#' should prefer `omega_f()` and the snake_case output names, but existing code
#' using the older names will continue to work.
#'
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param f_value F statistic
#' @param Fvalue Backward-compatible argument for the F statistic
#'   (deprecated; use `f_value` instead). If supplied, it overrides `f_value`.
#'   Included for users of the legacy `omega.F()`.
#' @param n full sample size
#' @param a significance level
#' @return \describe{
#'   \item{omega}{\eqn{\omega^2} effect size (legacy name; see also
#' `omega_value`)}
#'   \item{omegalow}{lower-level confidence interval of \eqn{\omega^2}
#'         (legacy name; see also `omega_lower_limit`)}
#'   \item{omegahigh}{upper-level confidence interval of \eqn{\omega^2}
#'         (legacy name; see also `omega_upper_limit`)}
#'   \item{dfm}{degrees of freedom for the model/IV/between
#'         (legacy name; see also `df_model`)}
#'   \item{dfe}{degrees of freedom for the error/residual/within
#'         (legacy name; see also `df_error`)}
#'   \item{F}{\eqn{F}-statistic (legacy name; see also `f_value`)}
#'   \item{p}{p-value (legacy name; see also `p_value`)}
#'   \item{estimate}{the \eqn{\omega^2} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#'   \item{omega_value}{\eqn{\omega^2} effect size (snake_case alias
#' of `omega`)}
#'   \item{omega_lower_limit}{lower-level confidence interval of
#' \eqn{\omega^2} (alias of `omegalow`)}
#'   \item{omega_upper_limit}{upper-level confidence interval of
#' \eqn{\omega^2} (alias of `omegahigh`)}
#'   \item{df_model}{degrees of freedom for the model/IV/between
#' (alias of `dfm`)}
#'   \item{df_error}{degrees of freedom for the error/residual/within
#' (alias of `dfe`)}
#'   \item{f_value}{\eqn{F}-statistic (alias of `F`)}
#'   \item{p_value}{p-value (alias of `p`)}
#' }
#'
#' @keywords effect size, omega, ANOVA
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from
#' # the "bn1_data" dataset, included in the MOTE library.
#'
#' # A health psychologist recorded the number of close inter-personal
#' # attachments of 45-year-olds who were in excellent, fair, or poor
#' # health. People in the Excellent Health group had 4, 3, 2, and 3
#' # close attachments; people in the Fair Health group had 3, 5,
#' # and 8 close attachments; and people in the Poor Health group
#' # had 3, 1, 0, and 2 close attachments.
#'
#' anova_model <- lm(formula = friends ~ group, data = bn1_data)
#' summary.aov(anova_model)
#'
#' omega_f(dfm = 2, dfe = 8,
#'         f_value = 5.134, n = 11, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' omega.F(dfm = 2, dfe = 8,
#'         Fvalue = 5.134, n = 11, a = .05)
omega_f <- function(dfm, dfe, f_value, n, a = .05, Fvalue) { #nolint

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (!missing(Fvalue)) {
    f_value <- Fvalue
  }

  if (missing(f_value)) {
    stop("Be sure to include the F statistic (f_value) from your ANOVA.")
  }

  if (missing(n)) {
    stop("Be sure to include total sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega_value <- (dfm * (f_value - 1)) / ((dfm * (f_value - 1)) + n)

  limits <- ci_r2(
    r2         = omega_value,
    df1       = dfm,
    df2       = dfe,
    conf_level = (1 - a)
  )

  p_value <- pf(f_value, dfm, dfe, lower.tail = FALSE)

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$\\omega^2$ = ", apa(omega_value, 2, TRUE), ", ",
    (1 - a) * 100, "\\% CI [",
    apa(limits$lower_conf_limit_r2, 2, TRUE), ", ",
    apa(limits$upper_conf_limit_r2, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$F$(", dfm, ", ", dfe, ") = ",
    apa(f_value, 2, TRUE), ", $p$ ",
    report_p,
    sep = ""
  )

  output <- list(
    # Legacy names
    omega     = omega_value,
    omegalow  = limits$lower_conf_limit_r2,
    omegahigh = limits$upper_conf_limit_r2,
    dfm       = dfm,
    dfe       = dfe,
    F         = f_value,
    p         = p_value,
    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases
    omega_value        = omega_value,
    omega_lower_limit  = limits$lower_conf_limit_r2,
    omega_upper_limit  = limits$upper_conf_limit_r2,
    df_model           = dfm,
    df_error           = dfe,
    f_value            = f_value,
    p_value            = p_value
  )

  return(output)

}

# Backward compatibility wrapper
#' @rdname omega_f
#' @export
omega.F <- function(dfm, dfe, Fvalue, n, a = .05) { # nolint
  omega_f(
    dfm     = dfm,
    dfe     = dfe,
    f_value = Fvalue,
    n       = n,
    a       = a
  )
}
