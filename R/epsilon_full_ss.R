#' \eqn{\epsilon^2} for ANOVA from \eqn{F} and Sum of Squares
#'
#' **Note on function and output names:** This effect size is now implemented
#' with the snake_case function name `epsilon_full_ss()` to follow modern R
#' style guidelines. The original dotted version `epsilon.full.SS()` is still
#' available as a wrapper for backward compatibility, and both functions
#' return the same list. The returned object includes both the original
#' element names (e.g., `epsilon`, `epsilonlow`, `epsilonhigh`, `dfm`, `dfe`,
#' `F`, `p`, `estimate`, `statistic`) and newer snake_case aliases (e.g.,
#' `epsilon_value`, `epsilon_lower_limit`, `epsilon_upper_limit`,
#' `df_model`, `df_error`, `f_value`, `p_value`). New code should prefer
#' `epsilon_full_ss()` and the snake_case output names, but existing code
#' using the older names will continue to work.
#'
#' This function displays \eqn{\epsilon^2} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total calculation.
#'
#' To calculate \eqn{\epsilon^2}, first, the mean square for the error is
#' is multiplied by the degrees of freedom for the model. The
#' product is divided by the sum of squares total.
#'
#' \deqn{\epsilon^2 = \frac{df_m (ms_m - ms_e)}{SS_T}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/epsilon.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total
#' @param a significance level
#' @return Provides the effect size (\eqn{\epsilon^2}) with associated
#' confidence intervals from the \eqn{F}-statistic.
#' \describe{
#' \item{epsilon}{effect size}
#' \item{epsilonlow}{lower level confidence interval of epsilon}
#' \item{epsilonhigh}{upper level confidence interval of epsilon}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{\eqn{F}-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the \eqn{\epsilon^2} statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, epsilon, ANOVA
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
#' epsilon_full_ss(dfm = 2, dfe = 8, msm = 12.621,
#'                 mse = 2.458, sst = (25.24 + 19.67), a = .05)
#'
#' # Backwards-compatible dotted name (deprecated)
#' epsilon.full.SS(dfm = 2, dfe = 8, msm = 12.621,
#'                 mse = 2.458, sst = (25.24 + 19.67), a = .05)
epsilon_full_ss <- function(dfm, dfe, msm, mse, sst, a = .05) {

  if (missing(dfm)) {
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)) {
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(msm)) {
    stop("Be sure to include the mean square value for your model (IV).")
  }

  if (missing(mse)) {
    stop("Be sure to include the mean square value for the error.")
  }

  if (missing(sst)) {
    stop("Be sure to include the sum of squares total for your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  epsilon <- (dfm * (msm - mse)) / sst
  f_value <- msm / mse

  limits <- ci_r2(
    r2 = epsilon,
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
    "$\\epsilon^2$ = ", apa(epsilon, 2, TRUE), ", ", (1 - a) * 100,
    "\\% CI [",
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
    epsilon    = epsilon,
    epsilonlow = limits$lower_conf_limit_r2,
    epsilonhigh = limits$upper_conf_limit_r2,
    dfm        = dfm,
    dfe        = dfe,
    F          = f_value,
    p          = p_value,
    estimate   = estimate,
    statistic  = statistic,

    # Snake_case aliases
    epsilon_value        = epsilon,
    epsilon_lower_limit  = limits$lower_conf_limit_r2,
    epsilon_upper_limit  = limits$upper_conf_limit_r2,
    df_model             = dfm,
    df_error             = dfe,
    f_value              = f_value,
    p_value              = p_value
  )

  return(output)

}

# Backward compatibility wrapper
#' @rdname epsilon_full_ss
#' @export
epsilon.full.SS <- function(dfm, dfe, msm, mse, sst, a = .05) { # nolint
  epsilon_full_ss(dfm = dfm, dfe = dfe, msm = msm, mse = mse, sst = sst, a = a)
}
