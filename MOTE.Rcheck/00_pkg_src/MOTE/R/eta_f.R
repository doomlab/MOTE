#' \eqn{\eta^2} and Coefficient of Determination (R\eqn{^2})
#' for ANOVA from \eqn{F}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `eta_f()` to follow modern R style
#' guidelines. The original dotted version `eta.F()` is still available as a
#' wrapper for backward compatibility, and both functions return the same
#' list. The returned object includes both the original element names
#' (e.g., `eta`, `etalow`, `etahigh`, `dfm`, `dfe`, `F`, `p`, `estimate`,
#' `statistic`) and newer snake_case aliases (e.g., `eta_value`,
#' `eta_lower_limit`, `eta_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `eta_f()` and the snake_case output
#' names, but existing code using the older names will continue to work.
#'
#' This function displays \eqn{\eta^2} from ANOVA analyses
#' and their non-central confidence interval based on the \eqn{F} distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial eta squared.
#'
#' Eta is calculated by multiplying the degrees of freedom of
#' the model by the F-statistic. This is divided by the product
#' of degrees of freedom of the model, the F-statistic, and
#' the degrees of freedom for the error or residual.
#'
#' \deqn{\eta^2 = \frac{df_m \cdot F}{df_m \cdot F + df_e}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etaf.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param f_value F statistic
#' @param Fvalue F statistic only for older function
#' @param a significance level
#' @return Provides the effect size (\eqn{\eta^2}) with associated
#' confidence intervals and relevant statistics.
#'
#' \describe{
#' \item{eta}{\eqn{\eta^2} effect size}
#' \item{etalow}{lower level confidence interval of \eqn{\eta^2}}
#' \item{etahigh}{upper level confidence interval of \eqn{\eta^2}}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{\eqn{F}-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the \eqn{\eta^2} statistic and confidence interval
#' in APA style for markdown printing}
#' \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
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
#' eta_f(dfm = 2, dfe = 8,
#'       Fvalue = 5.134, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' eta.F(dfm = 2, dfe = 8,
#'       Fvalue = 5.134, a = .05)
eta_f <- function(dfm, dfe, f_value, a = .05, Fvalue) { # nolint

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
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta_value <- (dfm * f_value) / (dfm * f_value + dfe)

  limits <- ci_r2(
    r2 = eta_value,
    df1 = dfm,
    df2 = dfe,
    conf_level = (1 - a)
  )

  f_value <- f_value

  p_value <- pf(f_value, dfm, dfe, lower.tail = FALSE)

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$\\eta^2$ = ", apa(eta_value, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
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
#' @rdname eta_f
#' @export
eta.F <- function(dfm, dfe, Fvalue, a = .05) { # nolint
  eta_f(dfm = dfm, dfe = dfe, f_value = Fvalue, a = a)
}
