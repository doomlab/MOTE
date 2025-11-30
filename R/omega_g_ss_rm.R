#' omega^2_G (Generalized Omega Squared) for Multi-Way and Mixed ANOVA from F
#'
#' This function displays \eqn{\omega^2_G} (generalized omega squared)
#' from ANOVA analyses and its non-central confidence interval based on
#' the \eqn{F} distribution. This formula is appropriate for multi-way
#' repeated-measures designs and mixed-level designs.
#'
#' Omega squared is calculated by subtracting the product of the
#' degrees of freedom of the model and the mean square of the
#' subject variance from the sum of squares for the model.
#'
#' This is divided by the value obtained after combining
#' the sum of squares total, sum of squares for the other
#' independent variable, and the mean square of the
#' subject variance multiplied by the number of levels
#' in the other model/IV/between.
#'
#' \deqn{\omega^2_G = \frac{SS_M - (df_m \times MS_S)}{SS_T +
#' SS_{M2} + j \times MS_S}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gosrmss.html}
#' {Learn more on our example page.}
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `omega_g_ss_rm()` to follow modern R style
#' guidelines. The original dotted version `omega.gen.SS.rm()` is still
#' available as a wrapper for backward compatibility, and both functions return
#' the same list. The returned object includes both the original element names
#' (e.g., `omega`, `omegalow`, `omegahigh`, `dfm`, `dfe`, `F`, `p`, `estimate`,
#' `statistic`) and newer snake_case aliases (e.g., `omega_value`,
#' `omega_lower_limit`, `omega_upper_limit`, `df_model`, `df_error`, `f_value`,
#' `p_value`). New code should prefer `omega_g_ss_rm()` and
#' the snake_case output names, but existing code using the older
#' names will continue to work.
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the MAIN model/IV/between
#' @param ssm2 sum of squares for the OTHER model/IV/between
#' @param sst sum of squares total across the whole ANOVA
#' @param mss mean square for the subject variance
#' @param j number of levels in the OTHER IV
#' @param f_value F statistic from the output for your IV
#' @param a significance level
#' @return \describe{
#'   \item{omega}{\eqn{\omega^2_G} effect size (legacy name; see
#' also `omega_value`)}
#'   \item{omegalow}{lower-level confidence interval of \eqn{\omega^2_G}
#'         (legacy name; see also `omega_lower_limit`)}
#'   \item{omegahigh}{upper-level confidence interval of \eqn{\omega^2_G}
#'         (legacy name; see also `omega_upper_limit`)}
#'   \item{dfm}{degrees of freedom for the model/IV/between
#'         (legacy name; see also `df_model`)}
#'   \item{dfe}{degrees of freedom for the error/residual/within
#'         (legacy name; see also `df_error`)}
#'   \item{F}{\eqn{F}-statistic (legacy name; see also `f_value`)}
#'   \item{p}{p-value (legacy name; see also `p_value`)}
#'   \item{estimate}{the \eqn{\omega^2_G} statistic and
#'         confidence interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#'   \item{omega_value}{\eqn{\omega^2_G} effect size (snake_case
#' alias of `omega`)}
#'   \item{omega_lower_limit}{lower-level confidence interval of
#' \eqn{\omega^2_G}
#'         (alias of `omegalow`)}
#'   \item{omega_upper_limit}{upper-level confidence interval of
#' \eqn{\omega^2_G}
#'         (alias of `omegahigh`)}
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
#' # The following example is derived from the "mix2_data"
#' # dataset, included in the MOTE library.
#'
#' # Given previous research, we know that backward strength in free
#' # association tends to increase the ratings participants give when
#' # you ask them how many people out of 100 would say a word in
#' # response to a target word (like Family Feud). This result is
#' # tied to people’s overestimation of how well they think they know
#' # something, which is bad for studying. So, we gave people instructions
#' # on how to ignore the BSG.  Did it help? Is there an interaction
#' # between BSG and instructions given?
#'
#' # You would calculate one partial GOS value for each F-statistic.
#' # Here's an example for the main effect 1 with typing in numbers.
#' omega_g_ss_rm(dfm = 1, dfe = 156,
#'           ssm = 6842.46829,
#'           ssm2 = 14336.07886,
#'           sst = sum(c(30936.498, 6842.46829,
#'                       14336.07886, 8657.094, 71.07608)),
#'           mss = 30936.498 / 156,
#'           j = 2, f_value = 34.503746, a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' omega.gen.SS.rm(dfm = 1, dfe = 156,
#'                 ssm = 6842.46829,
#'                 ssm2 = 14336.07886,
#'                 sst = sum(c(30936.498, 6842.46829,
#'                             14336.07886, 8657.094, 71.07608)),
#'                 mss = 30936.498 / 156,
#'                 j = 2, Fvalue = 34.503746, a = .05)
omega_g_ss_rm <- function(dfm, dfe, ssm, ssm2, sst, mss, j, f_value, a = .05) {

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)) {
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(ssm2)) {
    stop("Be sure to include the sum of squares for the OTHER model (IV).")
  }

  if (missing(sst)) {
    stop("Be sure to include the sum of squares total for your model.")
  }

  if (missing(mss)) {
    stop("Be sure to include the mean square for your subjects
    from your model.")
  }

  if (missing(j)) {
    stop("Be sure to include the number of levels in the OTHER IV.")
  }

  if (missing(f_value)) {
    stop("Be sure to include the F statistic (f_value) from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega_value <- (ssm - (dfm * mss)) / (sst + ssm2 + j * mss)

  limits <- ci_r2(
    r2        = omega_value,
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
    "$\\omega^2_{G}$ = ", apa(omega_value, 2, TRUE), ", ",
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
#' @rdname omega_g_ss_rm
#' @export
omega.gen.SS.rm <- function(dfm, dfe, ssm, ssm2, sst, mss, j, Fvalue, a = .05) { # nolint
  omega_g_ss_rm(
    dfm     = dfm,
    dfe     = dfe,
    ssm     = ssm,
    ssm2    = ssm2,
    sst     = sst,
    mss     = mss,
    j       = j,
    f_value = Fvalue,
    a       = a
  )
}
