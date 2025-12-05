#' \eqn{\eta^2_p} for ANOVA from \eqn{F} and Sum of Squares
#'
#' This function displays \eqn{\eta^2_p} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula works for one way and multi way designs.
#'
#' \eqn{\eta^2_p} is calculated by dividing the sum of squares
#' of the model by the sum of the sum of squares of the model and
#' sum of squares of the error.
#'
#' \deqn{\eta^2_p = \frac{SS_M}{SS_M + SS_E}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etapss.html}{Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `eta_partial_ss()` to follow modern R
#' style guidelines. The original dotted version `eta.partial.SS()` is still
#' available as a wrapper for backward compatibility, and both functions
#' return the same list. The returned object includes both the original
#' element names (e.g., `eta`, `etalow`, `etahigh`, `dfm`, `dfe`, `F`, `p`,
#' `estimate`, `statistic`) and newer snake_case aliases (e.g., `eta_value`,
#' `eta_lower_limit`, `eta_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `eta_partial_ss()` and the snake_case
#' output names, but existing code using the older names will continue to
#' work.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param f_value F statistic
#' @param Fvalue Backward-compatible argument for the F statistic
#'   (deprecated; use `f_value` instead). If supplied, it overrides `f_value`.
#'   Included for users of the legacy `eta.partial.SS()`.
#' @param a significance level
#'
#' @return Provides the effect size (\eqn{\eta^2_p}) with associated
#' confidence intervals and relevant statistics.
#'
#' \describe{
#' \item{eta}{\eqn{\eta^2_p} effect size}
#' \item{etalow}{lower level confidence interval of \eqn{\eta^2_p}}
#' \item{etahigh}{upper level confidence interval of \eqn{\eta^2_p}}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{\eqn{F}-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the \eqn{\eta^2_p} statistic and confidence interval
#' in APA style for markdown printing}
#' \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size eta ANOVA
#' @export
#' @import stats
#' @examples
#'
#' # The following example is derived from the "bn2_data"
#' # dataset, included in the MOTE library.
#'
#' # Is there a difference in athletic spending budget for different sports?
#' # Does that spending interact with the change in coaching staff?
#' # This data includes (fake) athletic budgets for baseball, basketball,
#' # football, soccer, and volleyball teams with new and old coaches
#' # to determine if there are differences in
#' # spending across coaches and sports.
#'
#' # Example using reported ANOVA table values directly
#' eta_partial_ss(dfm = 4, dfe = 990,
#'                ssm = 338057.9, sse = 32833499,
#'                f_value = 2.548, a = .05)
#'
#' # Example computing Type III SS with code (requires the "car" package)
#' if (requireNamespace("car", quietly = TRUE)) {
#'
#'   # Fit the model using stats::lm
#'   mod <- stats::lm(money ~ coach * type, data = bn2_data)
#'
#'   # Type III table for the effects
#'   aov_type3 <- car::Anova(mod, type = 3)
#'
#'   # Extract DF, SS, and F for the interaction (coach:type)
#'   dfm_int <- aov_type3["coach:type", "Df"]
#'   ssm_int <- aov_type3["coach:type", "Sum Sq"]
#'   F_int   <- aov_type3["coach:type", "F value"]
#'
#'   # Residual DF and SS from the standard ANOVA table
#'   aov_type1 <- stats::anova(mod)
#'   dfe <- aov_type1["Residuals", "Df"]
#'   sse <- aov_type1["Residuals", "Sum Sq"]
#'
#'   # Calculate partial eta-squared for the interaction using Type III SS
#'   eta_partial_ss(dfm = dfm_int, dfe = dfe,
#'                  ssm = ssm_int, sse = sse,
#'                  f_value = F_int, a = .05)
#' #'
#' # Backwards-compatible dotted name (deprecated)
#' eta.partial.SS(dfm = 4, dfe = 990,
#'                ssm = 338057.9, sse = 32833499,
#'                Fvalue = 2.548, a = .05)
#' }
eta_partial_ss <- function(dfm, dfe, ssm, sse,
                           f_value, a = .05, Fvalue) { #nolint

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)) {
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sse)) {
    stop("Be sure to include the sum of squares for the error.")
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

  eta_value <- ssm / (ssm + sse)

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
    "$\\eta^2_{p}$ = ", apa(eta_value, 2, FALSE), ", ",
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
#' @rdname eta_partial_ss
#' @export
eta.partial.SS <- function(dfm, dfe, ssm, sse, Fvalue, a = .05) { # nolint
  eta_partial_ss(dfm = dfm, dfe = dfe, ssm = ssm,
                 sse = sse, f_value = Fvalue, a = a)
}
