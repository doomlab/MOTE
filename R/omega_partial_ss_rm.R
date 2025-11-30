#' omega^2_p (Partial Omega Squared) for Repeated Measures ANOVA from F
#'
#' This function displays \eqn{\omega^2_p} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula is appropriate for multi-way repeated measures designs
#' and mixed-level designs.
#'
#' Partial omega squared is calculated by subtracting the mean
#' square for the error from the mean square of the model, which is
#' multiplied by degrees of freedom of the model. This is divided
#' by the sum of the sum of squares for the model, sum of squares
#' for the error, sum of squares for the subject, and the
#' mean square of the subject.
#'
#' \deqn{\omega^2_p = \frac{df_m (MS_M - MS_E)}{SS_M + SS_E + SS_S + MS_S}}
#'
#' The F-statistic is calculated by dividing the mean square
#' of the model by the mean square of the error.
#'
#'      F = msm / mse
#'
#'\href{https://www.aggieerin.com/shiny-server/tests/omegaprmss.html}
#' {Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `omega_partial_ss_rm()` to follow modern R
#' style guidelines. The original dotted version `omega.partial.SS.rm()` is
#' still available as a wrapper for backward compatibility, and both functions
#' return the same list. The returned object includes both the original element
#' names (e.g., `omega`, `omegalow`, `omegahigh`, `dfm`, `dfe`, `F`, `p`,
#' `estimate`, `statistic`) and newer snake_case aliases (e.g., `omega_value`,
#' `omega_lower_limit`, `omega_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `omega_partial_ss_rm()` and the
#' snake_case output names, but existing code using the older names will
#' continue to work.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param mss mean square for the subject variance
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param sss sum of squares for the subject variance
#' @param a significance level
#' @return \describe{
#'   \item{omega}{\eqn{\omega^2_p} effect size (legacy name; see also
#' `omega_value`)}
#'   \item{omegalow}{lower-level confidence interval of \eqn{\omega^2_p}
#'         (legacy name; see also `omega_lower_limit`)}
#'   \item{omegahigh}{upper-level confidence interval of \eqn{\omega^2_p}
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
#'   \item{omega_value}{\eqn{\omega^2_p} effect size (snake_case alias
#' of `omega`)}
#'   \item{omega_lower_limit}{lower-level confidence interval of
#' \eqn{\omega^2_p} (alias of `omegalow`)}
#'   \item{omega_upper_limit}{upper-level confidence interval of
#' \eqn{\omega^2_p} (alias of `omegahigh`)}
#'   \item{df_model}{degrees of freedom for the model/IV/between
#' (alias of `dfm`)}
#'   \item{df_error}{degrees of freedom for the error/residual/within
#' (alias of `dfe`)}
#'   \item{f_value}{\eqn{F}-statistic (alias of `F`)}
#'   \item{p_value}{p-value (alias of `p`)}
#' }
#'
#' @keywords effect size, omega, ANOVA
#' @export
#' @import stats
#' @examples
#'
#' # The following example is derived from the "rm2_data" dataset,
#' # included in the MOTE library.
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
#' # You would calculate one partial GOS value for each F-statistic.
#' # You can leave out the MS options if you include all the SS options.
#' # Here's an example for the interaction with typing in numbers.
#' omega_partial_ss_rm(dfm = 1, dfe = 157,
#'                     msm = 2442.948 / 1,
#'                     mse = 5402.567 / 157,
#'                     mss = 76988.130 / 157,
#'                     ssm = 2442.948, sss = 76988.13,
#'                     sse = 5402.567, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' omega.partial.SS.rm(dfm = 1, dfe = 157,
#'                     msm = 2442.948 / 1,
#'                     mse = 5402.567 / 157,
#'                     mss = 76988.130 / 157,
#'                     ssm = 2442.948, sss = 76988.13,
#'                     sse = 5402.567, a = .05)
omega_partial_ss_rm <- function(dfm, dfe, msm, mse,
                                mss, ssm, sse, sss, a = .05) {

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
    stop("Be sure to include the sum of squares for your error.")
  }

  if (missing(sss)) {
    stop("Be sure to include the sum of squares for the subject variance.")
  }

  if (missing(msm)) {
    msm <- ssm / dfm
  }

  if (missing(mse)) {
    mse <- sse / dfe
  }

  if (missing(mss)) {
    mss <- sss / dfe
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega_value <- (dfm * (msm - mse)) / (ssm + sse + sss + mss)
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
#' @rdname omega_partial_ss_rm
#' @export
omega.partial.SS.rm <- function(dfm, dfe, msm, mse, mss, ssm, sse, sss, a = .05) { # nolint
  omega_partial_ss_rm(
    dfm = dfm,
    dfe = dfe,
    msm = msm,
    mse = mse,
    mss = mss,
    ssm = ssm,
    sse = sse,
    sss = sss,
    a   = a
  )
}
