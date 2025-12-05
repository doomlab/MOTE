#' \eqn{\eta^2} for ANOVA from \eqn{F} and Sum of Squares
#'
#' This function displays \eqn{\eta^2} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total.
#'
#' Eta squared is calculated by dividing the sum of squares for the model
#' by the sum of squares total.
#'
#' \deqn{\eta^2 = \frac{SS_M}{SS_T}}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `eta_full_ss()` to follow modern R style
#' guidelines. The original dotted version `eta.full.SS()` is still available
#' as a wrapper for backward compatibility, and both functions return the same
#' list. The returned object includes both the original element names
#' (e.g., `eta`, `etalow`, `etahigh`, `dfm`, `dfe`, `F`, `p`, `estimate`,
#' `statistic`) and newer snake_case aliases (e.g., `eta_value`,
#' `eta_lower_limit`, `eta_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `eta_full_ss()` and the snake_case
#' output names, but existing code using the older names will continue to work.
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etass.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sst sum of squares total
#' @param f_value F statistic
#' @param Fvalue Backward-compatible argument for the F statistic
#'   (deprecated; use `f_value` instead). If supplied, it overrides
#'   `f_value`. Included for users of the legacy `eta.full.SS()`.
#' @param a significance level
#' @return Provides the effect size (\eqn{\eta^2}) with associated
#' confidence intervals and relevant statistics.
#'
#' \describe{
#'   \item{eta}{\eqn{\eta^2} effect size}
#'   \item{etalow}{lower level confidence interval of \eqn{\eta^2}}
#'   \item{etahigh}{upper level confidence interval of \eqn{\eta^2}}
#'   \item{dfm}{degrees of freedom for the model/IV/between}
#'   \item{dfe}{degrees of freedom for the error/residual/within}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{\eta^2} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size eta ANOVA
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "bn1_data"
#' # dataset, included in the MOTE library.
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
#' eta_full_ss(dfm = 2, dfe = 8, ssm = 25.24,
#'             sst = (25.24 + 19.67), f_value = 5.134, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' eta.full.SS(dfm = 2, dfe = 8, ssm = 25.24,
#'             sst = (25.24 + 19.67), Fvalue = 5.134, a = .05)


eta_full_ss <- function(dfm, dfe, ssm, sst, f_value, a = .05, Fvalue) { #nolint

  if (!missing(Fvalue)) {
    f_value <- Fvalue
  }

  if (missing(f_value)) {
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)) {
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sst)) {
    stop("Be sure to include the sum of squares total from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta_value <- ssm / sst

  limits <- ci_r2(
    r2 = eta_value,
    df1 = dfm,
    df2 = dfe,
    conf_level = (1 - a)
  )

  p_value <- pf(f_value, dfm, dfe, lower.tail = FALSE)

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$\\eta^2$ = ", apa(eta_value, 2, TRUE), ", ",
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
    eta      = eta_value,
    etalow   = limits$lower_conf_limit_r2,
    etahigh  = limits$upper_conf_limit_r2,
    dfm      = dfm,
    dfe      = dfe,
    F        = f_value,
    p        = p_value,
    estimate = estimate,
    statistic = statistic,

    # Snake_case aliases
    eta_value        = eta_value,
    eta_lower_limit  = limits$lower_conf_limit_r2,
    eta_upper_limit  = limits$upper_conf_limit_r2,
    df_model         = dfm,
    df_error         = dfe,
    f_value          = f_value,
    p_value          = p_value
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname eta_full_ss
#' @export
eta.full.SS <- function(dfm, dfe, ssm, sst, Fvalue, a = .05) { # nolint
  eta_full_ss(dfm = dfm, dfe = dfe, ssm = ssm,
              sst = sst, f_value = Fvalue, a = a)
}
