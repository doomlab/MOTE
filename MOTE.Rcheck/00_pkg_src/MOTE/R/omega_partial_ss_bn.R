#' omega^2_p (Partial Omega Squared) for Between-Subjects ANOVA from F
#'
#' This function displays \eqn{\omega^2_p} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula is appropriate for multi-way between-subjects designs.
#'
#' Partial omega squared is calculated by subtracting the mean square
#' for the error from the mean square of the model, which is multiplied
#' by degrees of freedom of the model. This is divided by the product
#' of the degrees of freedom for the model are deducted from the sample
#' size, multiplied by the mean square of the error, plus the sum of
#' squares for the model.
#'
#' \deqn{\omega^2_p = \frac{df_m (MS_M - MS_E)}{SS_M + (n - df_m) \times MS_E}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegapbnss.html}{Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `omega_partial_ss_bn()` to follow modern R
#' style guidelines. The original dotted version `omega.partial.SS.bn()` is
#' still available as a wrapper for backward compatibility, and both functions
#' return the same list. The returned object includes both the original element
#' names (e.g., `omega`, `omegalow`, `omegahigh`, `dfm`, `dfe`, `F`, `p`,
#' `estimate`, `statistic`) and newer snake_case aliases (e.g., `omega_value`,
#' `omega_lower_limit`, `omega_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `omega_partial_ss_bn()` and the snake_case
#' output names, but existing code using the older names will continue to work.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param n total sample size
#' @param a significance level
#'
#' @return \describe{
#'   \item{omega}{\eqn{\omega^2_p} effect size (legacy name; see
#' also `omega_value`)}
#'   \item{omegalow}{lower level confidence interval of \eqn{\omega^2_p}
#'         (legacy name; see also `omega_lower_limit`)}
#'   \item{omegahigh}{upper level confidence interval of \eqn{\omega^2_p}
#'         (legacy name; see also `omega_upper_limit`)}
#'   \item{dfm}{degrees of freedom for the model/IV/between
#'         (legacy name; see also `df_model`)}
#'   \item{dfe}{degrees of freedom for the error/residual/within
#'         (legacy name; see also `df_error`)}
#'   \item{F}{\eqn{F}-statistic (legacy name; see also `f_value`)}
#'   \item{p}{p-value (legacy name; see also `p_value`)}
#'   \item{estimate}{the \eqn{\omega^2_p} statistic and confidence
#' interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#'   \item{omega_value}{\eqn{\omega^2_p} effect size (snake_case
#' alias of `omega`)}
#'   \item{omega_lower_limit}{lower level confidence interval of
#' \eqn{\omega^2_p} (alias of `omegalow`)}
#'   \item{omega_upper_limit}{upper level confidence interval of
#' \eqn{\omega^2_p} (alias of `omegahigh`)}
#'   \item{df_model}{degrees of freedom for the model/IV/between
#' (alias of `dfm`)}
#'   \item{df_error}{degrees of freedom for the error/residual/within
#' (alias of `dfe`)}
#'   \item{f_value}{\eqn{F}-statistic (alias of `F`)}
#'   \item{p_value}{p-value (alias of `p`)}
#' }
#'
#' @keywords effect size omega ANOVA
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "bn2_data"
#' # dataset, included in the MOTE library.
#'
#' # Is there a difference in athletic spending budget for different sports?
#' # Does that spending interact with the change in coaching staff?
#' # This data includes (fake) athletic budgets for baseball,
#' # basketball, football, soccer, and volleyball teams
#' # with new and old coaches to determine if there are differences in
#' # spending across coaches and sports.
#'
#' # You would calculate one omega value for each F-statistic.
#' # Here's an example for the interaction using reported ANOVA values.
#' omega_partial_ss_bn(dfm = 4, dfe = 990,
#'                     msm = 338057.9 / 4,
#'                     mse = 32833499 / 990,
#'                     ssm = 338057.9,
#'                     n = 1000, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' omega.partial.SS.bn(dfm = 4, dfe = 990,
#'                     msm = 338057.9 / 4,
#'                     mse = 32833499 / 990,
#'                     ssm = 338057.9,
#'                     n = 1000, a = .05)
#'
#' # The same analysis can be fit with stats::lm and car::Anova(type = 3).
#' # This example shows how to obtain the ANOVA table and plug its values
#' # into omega.partial.SS.bn without relying on ezANOVA.
#' if (requireNamespace("car", quietly = TRUE)) {
#'
#'   mod <- stats::lm(money ~ coach * type, data = bn2_data)
#'
#'   # Type I table (for residual SS and df)
#'   aov_type1 <- stats::anova(mod)
#'
#'   # Type III SS table for the effects
#'   aov_type3 <- car::Anova(mod, type = 3)
#'
#'   # Extract dfs and sums of squares for the interaction coach:type
#'   dfm_int <- aov_type3["coach:type", "Df"]
#'   ssm_int <- aov_type3["coach:type", "Sum Sq"]
#'   msm_int <- ssm_int / dfm_int
#'
#'   dfe <- aov_type1["Residuals", "Df"]
#'   sse <- aov_type1["Residuals", "Sum Sq"]
#'   mse <- sse / dfe
#'
#'   omega_partial_ss_bn(dfm = dfm_int,
#'                       dfe = dfe,
#'                       msm = msm_int,
#'                       mse = mse,
#'                       ssm = ssm_int,
#'                       n = nrow(bn2_data),
#'                       a = .05)
#' }

omega_partial_ss_bn <- function(dfm, dfe, msm, mse, ssm, n, a = .05) {

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(msm)) {
    stop("Be sure to include the mean squared model for your model (IV).")
  }

  if (missing(mse)) {
    stop("Be sure to include the mean squared error for your model.")
  }

  if (missing(ssm)) {
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(n)) {
    stop("Be sure to include total sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega_value <- (dfm * (msm - mse)) / (ssm + (n - dfm) * mse)

  f_value <- msm / mse

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
    "$\\omega^2_{p}$ = ", apa(omega_value, 2, TRUE), ", ",
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
#' @rdname omega_partial_ss_bn
#' @export
omega.partial.SS.bn <- function(dfm, dfe, msm, mse, ssm, n, a = .05) { # nolint
  omega_partial_ss_bn(
    dfm = dfm,
    dfe = dfe,
    msm = msm,
    mse = mse,
    ssm = ssm,
    n   = n,
    a   = a
  )
}
