#' \eqn{\eta^2_{G}} (Partial Generalized Eta-Squared) for
#' Repeated-Measures ANOVA from \eqn{F}
#'
#' This function displays partial generalized eta-squared
#' (\eqn{\eta^2_{G}}) from ANOVA analyses and its non-central
#' confidence interval based on the \eqn{F} distribution.
#' This formula works for multi-way repeated measures designs.
#'
#' To calculate partial generalized eta squared, first, the sum of
#' squares of the model, sum of squares of the subject
#' variance, sum of squares for the first and second independent variables,
#' and the sum of squares for the interaction are added together.
#' The sum of squares of the model is divided by this value.
#'
#' \deqn{\eta^2_{G} = \frac{SS_M}{SS_M + SS_S + SS_{E1} + SS_{E2} + SS_{E3}}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gesrmss.html}
#' {Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `ges_partial_ss_rm()` to follow modern R
#' style guidelines. The original dotted version `ges.partial.SS.rm()` is still
#' available as a wrapper for backward compatibility, and both functions return
#' the same list. The returned object includes both the original element names
#' (e.g., `ges`, `geslow`, `geshigh`, `dfm`, `dfe`, `F`, `p`, `estimate`,
#' `statistic`) and newer snake_case aliases (e.g., `ges_value`,
#' `ges_lower_limit`, `ges_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `ges_partial_ss_rm()` and the snake_case
#' output names, but existing code using the older names will continue to work.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sss sum of squares subject variance
#' @param sse1 sum of squares for the error/residual/within for the first IV
#' @param sse2 sum of squares for the error/residual/within for the second IV
#' @param sse3 sum of squares for the error/residual/within for the interaction
#' @param f_value F statistic
#' @param a significance level
#' @return \describe{
#'   \item{ges}{\eqn{\eta^2_{G}} effect size}
#'   \item{geslow}{lower level confidence interval for \eqn{\eta^2_{G}}}
#'   \item{geshigh}{upper level confidence interval for \eqn{\eta^2_{G}}}
#'   \item{dfm}{degrees of freedom for the model/IV/between}
#'   \item{dfe}{degrees of freedom for the error/residual/within}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{\eta^2_{G}} statistic and confidence
#' interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, ges, ANOVA
#' @export
#' @import stats
#' @examples
#'
#' # The following example is derived from the "rm2_data" dataset, included
#' # in the MOTE library.
#'
#' # In this experiment people were given word pairs to rate based on
#' # their "relatedness". How many people out of a 100 would put LOST-FOUND
#' # together? Participants were given pairs of words and asked to rate them
#' # on how often they thought 100 people would give the second word if shown
#' # the first word.  The strength of the word pairs was manipulated through
#' # the actual rating (forward strength: FSG) and the strength of the reverse
#' # rating (backward strength: BSG). Is there an interaction between FSG and
#' # BSG when participants are estimating the relation between word pairs?
#'
#' # You would calculate one partial GES value for each F-statistic.
#' # Here's an example for the interaction with typing in numbers.
#' ges_partial_ss_rm(dfm = 1, dfe = 157,
#'                   ssm = 2442.948, sss = 76988.13,
#'                   sse1 = 5402.567, sse2 = 8318.75, sse3 = 6074.417,
#'                   f_value = 70.9927, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' ges.partial.SS.rm(dfm = 1, dfe = 157,
#'                   ssm = 2442.948, sss = 76988.13,
#'                   sse1 = 5402.567, sse2 = 8318.75, sse3 = 6074.417,
#'                   Fvalue = 70.9927, a = .05)
ges_partial_ss_rm <- function(dfm, dfe, ssm, sss,
                              sse1, sse2, sse3, f_value, a = .05) {

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)) {
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sss)) {
    stop("Be sure to include the sum of squares for the subject variance.")
  }

  if (missing(sse1)) {
    stop("Be sure to include the sum of squares for your error
    for the first IV.")
  }

  if (missing(sse2)) {
    stop("Be sure to include the sum of squares for your error
    for the second IV.")
  }

  if (missing(sse3)) {
    stop("Be sure to include the sum of squares for your error
    for the interaction.")
  }

  if (missing(f_value)) {
    stop("Be sure to include the F statistic (f_value) from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  ges_value <- ssm / (ssm + sss + sse1 + sse2 + sse3)

  limits <- ci_r2(
    r2         = ges_value,
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
    "$\\eta^2_{G}$ = ", apa(ges_value, 2, TRUE), ", ",
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
    ges       = ges_value,
    geslow    = limits$lower_conf_limit_r2,
    geshigh   = limits$upper_conf_limit_r2,
    dfm       = dfm,
    dfe       = dfe,
    F         = f_value,
    p         = p_value,
    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases
    ges_value        = ges_value,
    ges_lower_limit  = limits$lower_conf_limit_r2,
    ges_upper_limit  = limits$upper_conf_limit_r2,
    df_model         = dfm,
    df_error         = dfe,
    f_value          = f_value,
    p_value          = p_value
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname ges_partial_ss_rm
#' @export
ges.partial.SS.rm <- function(dfm, dfe, ssm, sss, sse1, sse2, sse3, Fvalue, a = .05) { # nolint
  ges_partial_ss_rm(
    dfm     = dfm,
    dfe     = dfe,
    ssm     = ssm,
    sss     = sss,
    sse1    = sse1,
    sse2    = sse2,
    sse3    = sse3,
    f_value = Fvalue,
    a       = a
  )
}
