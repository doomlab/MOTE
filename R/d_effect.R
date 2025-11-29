#' General interface for Cohen's d
#'
#' - `"z_z"` — one-sample z-test effect size where the *z* value is supplied
#'   directly along with the sample size `n`. Supply `z_value` and `n`. You
#'   may optionally supply `sig` (population SD) for descriptive reporting.
#'   In this case, `d_effect()` will call [d_z_z()] with the same arguments.
#'
#' @description
#' `d_effect()` is a convenience wrapper that will route to the appropriate
#' Cohen's *d* helper function based on the arguments supplied. This allows
#' users to call a single function for different study designs while
#' maintaining backward compatibility with the more specific helpers.
#'
#'
#' @section Supported designs:
#'
#' - `"dep_t_avg"` — paired/dependent t-test with average SD denominator.
#'   Supply `m1`, `m2`, `sd1`, `sd2`, and `n`. In this case, `d()` will call
#'   [d_dep_t_avg()] with the same arguments.
#'
#' - `"dep_t_diff"` — paired/dependent t-test using the
#' **SD of the difference scores**.
#'   Supply `mdiff`, `sddiff`, and `n`. In this case, `d()` will call
#'   [d_dep_t_diff()] with the same arguments.
#'
#' - `"dep_t_diff_t"` — paired/dependent t-test where the
#' *t* value is supplied directly.
#'   Supply `t_value` and `n`. In this case, `d()` will call
#'   [d_dep_t_diff_t()] with the same arguments.
#'
#' - `"dep_t_rm"` — paired/dependent t-test using the repeated-measures
#'   effect size \(d_{rm}\), which adjusts for the correlation between
#'   measurements. Supply `m1`, `m2`, `sd1`, `sd2`, `r`, and `n`.
#'   In this case, `d()` will call [d_dep_t_rm()] with the same arguments.
#'
#' - `"ind_t"` — independent-groups t-test using the pooled SD (\(d_s\)).
#'   Supply `m1`, `m2`, `sd1`, `sd2`, `n1`, and `n2`. In this case, `d()` will
#'   call [d_ind_t()] with the same arguments.
#'
#' - `"ind_t_t"` — independent-groups t-test where the *t* value is supplied
#'   directly. Supply `t_value`, `n1`, and `n2`. In this case, `d()` will call
#'   [d_ind_t_t()] with the same arguments.
#'
#' - `"single_t"` — one‑sample t‑test effect size using the sample mean,
#'   population mean, sample SD, and sample size. Supply `m1` (sample mean),
#'   `u` (population mean), `sd1`, and `n`. In this case, `d()` will call
#'   [d_single_t()] with the same arguments.
#'
#' - `"single_t_t"` — one-sample t-test effect size where the *t* value is
#'   supplied directly along with the sample size `n`. In this case, `d()`
#'   will call [d_single_t_t()] with the same arguments.
#'
#' - `"prop"` — independent proportions (binary outcome) using a
#'   standardized mean difference (SMD) that treats each proportion as the
#'   mean of a Bernoulli variable with pooled Bernoulli SD. Supply `p1`,
#'   `p2`, `n1`, and `n2`. In this case, `d()` will call [d_prop()] with
#'   the same arguments.
#'
#' - `"prop_h"` — independent proportions (binary outcome) using Cohen's
#'   \(h\) based on the arcsine-transformed difference between proportions.
#'   Supply `p1`, `p2`, `n1`, and `n2`. In this case, `d()` will call
#'   [h_prop()] with the same arguments.
#'
#' - `"z_mean"` — one-sample z-test effect size using a known population
#'   standard deviation. Supply `m1` (sample mean), `u` (population mean),
#'   `sd1` (sample SD, used for descriptive CIs), `sig` (population SD),
#'   and `n`. In this case, `d_effect()` will call [d.z.mean()] with the
#'   same arguments.
#'
#' @param m1 Means of the two conditions or measurements.
#' @param m2 Means of the two conditions or measurements.
#' @param sd1 Standard deviations for the two conditions or measurements.
#' @param sd2 Standard deviations for the two conditions or measurements.
#' @param u Population or comparison mean for one‑sample t‑designs,
#'   used when `design = "single_t"`.
#' @param sig Population standard deviation for z-based designs, used when
#'   `design = "z_mean"`.
#' @param p1 Proportion for group one (between 0 and 1), used in the
#'   `"prop"` design.
#' @param p2 Proportion for group two (between 0 and 1), used in the
#'   `"prop"` design.
#' @param n1 Sample sizes for the two independent groups (used for
#'   independent-groups designs such as `"ind_t"`).
#' @param n2 Sample sizes for the two independent groups (used for
#'   independent-groups designs such as `"ind_t"`).
#' @param r Correlation between the paired measurements (used for
#'   repeated-measures designs such as `"dep_t_rm"`).
#' @param mdiff Mean difference between paired observations.
#' @param sddiff Standard deviation of the difference scores.
#' @param t_value t statistic value for the test. Used in designs where the
#'   effect size is derived directly from a reported t-value (e.g.,
#'   `"dep_t_diff_t"`, `"ind_t_t"`, or `"single_t_t"`).
#' @param z_value z statistic value for the test. Used in designs where the
#'   effect size is derived directly from a reported z-value (e.g.,
#'   `"z_z"`).
#' @param n Sample size (number of paired observations).
#' @param a Significance level used when computing confidence intervals.
#'   Defaults to `0.05`.
#' @param design Character string specifying the study design.
#' @param ... Reserved for future arguments and passed on to the underlying
#'   helper functions when appropriate.
#'
#' @return
#' A list with the same structure as returned by the underlying helper
#' function. For the current paired-means case, this is the output of
#' [d_dep_t_avg()], which includes:
#' \itemize{
#'   \item `d` – Cohen's d using the average SD denominator.
#'   \item `dlow`, `dhigh` – lower and upper confidence limits for `d`.
#'   \item Snake_case aliases such as `d_lower_limit` and `d_upper_limit`.
#'   \item Descriptive statistics (means, SDs, SEs, and their confidence
#'         limits) for each group.
#' }
#'
#' @examples
#' # Paired/dependent t-test using average SD denominator
#' # These arguments will route d() to d_dep_t_avg()
#' d_effect(
#'   m1 = 5.57, m2 = 4.43,
#'   sd1 = 1.99, sd2 = 2.88,
#'   n = 7, a = .05,
#'   design = "dep_t_avg"
#' )
#'
#' # You can also call the helper directly
#' d_dep_t_avg(
#'   m1 = 5.57, m2 = 4.43,
#'   sd1 = 1.99, sd2 = 2.88,
#'   n = 7, a = .05
#' )
#'
#' @export
d_effect <- function(m1 = NULL,
              m2 = NULL,
              sd1 = NULL,
              sd2 = NULL,
              u = NULL,
              sig = NULL,
              r = NULL,
              mdiff = NULL,
              sddiff = NULL,
              t_value = NULL,
              z_value = NULL,
              p1 = NULL,
              p2 = NULL,
              n1 = NULL,
              n2 = NULL,
              n = NULL,
              a = 0.05,
              design,
              ...) {

  design <- match.arg(
    design,
    choices = c(
      "dep_t_avg",
      "dep_t_diff",
      "dep_t_diff_t",
      "dep_t_rm",
      "ind_t",
      "ind_t_t",
      "prop",
      "prop_h",
      "single_t",
      "single_t_t",
      "z_mean",
      "z_z"
    )
  )

  if (design == "dep_t_avg") {
    if (is.null(m1) || is.null(m2) ||
          is.null(sd1) || is.null(sd2) ||
          is.null(n)) {
      stop(
        "For design = 'dep_t_avg', you must supply m1, m2, sd1, sd2, and n."
      )
    }

    return(
      d_dep_t_avg(
        m1 = m1,
        m2 = m2,
        sd1 = sd1,
        sd2 = sd2,
        n = n,
        a = a
      )
    )
  }

  if (design == "dep_t_diff") {
    if (is.null(m1) && is.null(m2) &&
        is.null(sd1) && is.null(sd2)) {
      # This design uses mdiff and sddiff instead of m1/m2/sd1/sd2
      # Ensure required arguments exist
      if (is.null(mdiff) || is.null(sddiff) || is.null(n)) {
        stop(
          "For design = 'dep_t_diff', you must supply mdiff, sddiff, and n."
        )
      }
    }

    # Call the helper
    return(
      d_dep_t_diff(
        mdiff  = mdiff,
        sddiff = sddiff,
        n      = n,
        a      = a
      )
    )
  }

  if (design == "dep_t_diff_t") {
    if (is.null(t_value) || is.null(n)) {
      stop(
        "For design = 'dep_t_diff_t', you must supply t_value and n."
      )
    }

    return(
      d_dep_t_diff_t(
        t_value = t_value,
        n       = n,
        a       = a
      )
    )
  }

  if (design == "dep_t_rm") {
    if (is.null(m1) || is.null(m2) ||
        is.null(sd1) || is.null(sd2) ||
        is.null(r)  || is.null(n)) {
      stop(
        "For design = 'dep_t_rm', you must supply m1, m2, sd1, sd2, r, and n."
      )
    }

    return(
      d_dep_t_rm(
        m1  = m1,
        m2  = m2,
        sd1 = sd1,
        sd2 = sd2,
        r   = r,
        n   = n,
        a   = a
      )
    )
  }

  if (design == "ind_t") {
    if (is.null(m1) || is.null(m2) ||
        is.null(sd1) || is.null(sd2) ||
        is.null(n1) || is.null(n2)) {
      stop(
        "For design = 'ind_t', you must supply m1, m2, sd1, sd2, n1, and n2."
      )
    }

    return(
      d_ind_t(
        m1  = m1,
        m2  = m2,
        sd1 = sd1,
        sd2 = sd2,
        n1  = n1,
        n2  = n2,
        a   = a
      )
    )
  }

  if (design == "ind_t_t") {
    if (is.null(t_value) || is.null(n1) || is.null(n2)) {
      stop(
        "For design = 'ind_t_t', you must supply t_value, n1, and n2."
      )
    }

    return(
      d_ind_t_t(
        t_value = t_value,
        n1      = n1,
        n2      = n2,
        a       = a
      )
    )
  }

  if (design == "prop") {
    if (is.null(p1) || is.null(p2) ||
        is.null(n1) || is.null(n2)) {
      stop(
        "For design = 'prop', you must supply p1, p2, n1, and n2."
      )
    }

    return(
      d_prop(
        p1 = p1,
        p2 = p2,
        n1 = n1,
        n2 = n2,
        a  = a
      )
    )
  }

  if (design == "prop_h") {
    if (is.null(p1) || is.null(p2) ||
        is.null(n1) || is.null(n2)) {
      stop(
        "For design = 'prop_h', you must supply p1, p2, n1, and n2."
      )
    }

    return(
      h_prop(
        p1 = p1,
        p2 = p2,
        n1 = n1,
        n2 = n2,
        a  = a
      )
    )
  }

  if (design == "single_t") {
    if (is.null(m1) || is.null(u) ||
        is.null(sd1) || is.null(n)) {
      stop(
        "For design = 'single_t', you must supply m1 (sample mean), u (population mean), sd1, and n."
      )
    }

    return(
      d_single_t(
        m  = m1,
        u  = u,
        sd = sd1,
        n  = n,
        a  = a
      )
    )
  }

  if (design == "single_t_t") {
    if (is.null(t_value) || is.null(n)) {
      stop(
        "For design = 'single_t_t', you must supply t_value and n."
      )
    }

    return(
      d_single_t_t(
        t = t_value,
        n = n,
        a = a
      )
    )
  }

  if (design == "z_mean") {
    if (is.null(m1) || is.null(u) ||
        is.null(sd1) || is.null(sig) ||
        is.null(n)) {
      stop(
        "For design = 'z_mean', you must supply m1 (sample mean), u (population mean), sd1, sig (population SD), and n."
      )
    }

    return(
      d.z.mean(
        m1  = m1,
        mu  = u,
        sd1 = sd1,
        sig = sig,
        n   = n,
        a   = a
      )
    )
  }

  if (design == "z_z") {
    if (is.null(z_value) || is.null(n)) {
      stop(
        "For design = 'z_z', you must supply z_value and n."
      )
    }

    return(
      d_z_z(
        z   = z_value,
        n   = n,
        a   = a,
        sig = sig
      )
    )
  }
}