#' r-family effect size wrapper
#'
#' This function provides a unified interface for computing r- and
#' variance-based effect sizes (e.g., correlations and coefficients of
#' determination) from different input summaries. It is analogous to the
#' \code{d_effect()} wrapper for standardized mean difference effect sizes.
#'
#' Currently, `r_effect()` supports effect sizes derived from Cohen's d,
#' from correlations, and from ANOVA summaries via several designs (see
#' **Supported designs**). These designs call lower-level functions
#' as [d_to_r()], [r_correl()], [epsilon_full_ss()], [eta_f()],
#' [omega_f()], [eta_full_ss()], [eta_partial_ss()], [ges_partial_ss_mix()], and
#' [ges_partial_ss_rm()] with the appropriate arguments.
#'
#' @section Supported designs:
#'
#' - `"d_to_r"` — correlation and R\eqn{^2} from Cohen's d for
#'   independent groups. Supply `d`, `n1`, and `n2`. In this case,
#'   `r_effect()` will call [d_to_r()] with the same arguments.
#'
#' - `"r_correl"` — correlation and R\eqn{^2} from a sample Pearson
#'   correlation. Supply `r` and `n`. In this case, `r_effect()` will call
#'   [r_correl()] with the same arguments.
#'
#' - `"v_chi_sq"` — Cramer's V from a chi-square test of association
#'   for an r x c contingency table. Supply `x2`, `n`, `r`, and `c`. In
#'   this case, `r_effect()` will call [v_chi_sq()] with the same
#'   arguments.
#'
#' - `"epsilon_full_ss"` — epsilon-squared (\eqn{\epsilon^2}) from an ANOVA
#'   table using model and error mean squares and the total sum of squares.
#'   Supply `dfm`, `dfe`, `msm`, `mse`, and `sst`. In this case,
#'   `r_effect()` will call [epsilon_full_ss()] with the same arguments.
#'
#' - `"eta_f"` — eta-squared (\eqn{\eta^2}) from an ANOVA F statistic and
#'   its associated degrees of freedom. Supply `dfm`, `dfe`, and `f_value`.
#'   In this case, `r_effect()` will call [eta_f()] with the same arguments.
#'
#' - `"omega_f"` — omega-squared (\eqn{\omega^2}) from an ANOVA F statistic,
#'   its associated degrees of freedom, and the total sample size. Supply
#'   `dfm`, `dfe`, `n`, and `f_value`. In this case, `r_effect()` will call
#'   [omega_f()] with the same arguments.
#'
#' - `"omega_full_ss"` — omega-squared (\eqn{\omega^2}) from ANOVA sums of
#'   squares, using the model mean square, error mean square, and total sum of
#'   squares along with the model and error degrees of freedom. Supply `dfm`,
#'   `dfe`, `msm`, `mse`, and `sst`. In this case, `r_effect()` will call
#'   [omega_full_ss()] with the same arguments.
#'
#' - `"eta_full_ss"` — eta-squared (\eqn{\eta^2}) from ANOVA sums of squares,
#'   using the model sum of squares and total sum of squares along with the
#'   model and error degrees of freedom. Supply `dfm`, `dfe`, `ssm`, `sst`,
#'   and `f_value`. In this case, `r_effect()` will call [eta_full_ss()] with
#'   the same arguments.
#'
#' - `"eta_partial_ss"` — partial eta-squared (\eqn{\eta^2_p}) from ANOVA sums
#'   of squares, using the model sum of squares and error sum of squares along
#'   with the model and error degrees of freedom. Supply `dfm`, `dfe`, `ssm`,
#'   `sse`, and `f_value`. In this case, `r_effect()` will call
#'   [eta_partial_ss()] with the same arguments.
#'
#' - `"ges_partial_ss_mix"` — partial generalized eta-squared
#'   (\eqn{\eta^2_{G}}) for mixed designs, using the model sum of squares,
#'   between-subjects sum of squares, and error sum of squares along with the
#'   model and error degrees of freedom. Supply `dfm`, `dfe`, `ssm`, `sss`,
#'   `sse`, and `f_value`. In this case, `r_effect()` will call
#'   [ges_partial_ss_mix()] with the same arguments.
#'
#' - `"ges_partial_ss_rm"` — partial generalized eta-squared
#'   (\eqn{\eta^2_{G}}) for repeated-measures designs, using the model sum of
#'   squares, between-subjects sum of squares, and multiple error sums of
#'   squares (e.g., for each level or effect) along with the model and error
#'   degrees of freedom. Supply `dfm`, `dfe`, `ssm`, `sss`, `sse1`,
#'   `sse2`, `sse3`, and `f_value`. In this case, `r_effect()` will call
#'   [ges_partial_ss_rm()] with the same arguments.
#'
#'
#' @param d Cohen's d value for the contrast of interest (used when
#'   `design = "d_to_r"`).
#' @param n1 Sample size for group one (used when `design = "d_to_r"`).
#' @param n2 Sample size for group two (used when `design = "d_to_r"`).
#' @param r Sample Pearson correlation coefficient (used when
#'   `design = "r_correl"`), or the number of rows in the contingency
#'   table (used when `design = "v_chi_sq"`).
#' @param n Sample size for the correlation (used when
#'   `design = "r_correl"`), the total sample size for the chi-square
#'   test (used when `design = "v_chi_sq"`), or the total sample size for
#'   the ANOVA (used when `design = "omega_f"`).
#' @param x2 Chi-square test statistic for the contingency table (used
#'   when `design = "v_chi_sq"`).
#' @param c Number of columns in the contingency table (used when
#'   `design = "v_chi_sq"`).
#' @param dfm Degrees of freedom for the model term (used when
#'   `design = "epsilon_full_ss"`, `design = "eta_f"`, `design = "omega_f"`, `design = "omega_full_ss"`, `design = "eta_full_ss"`,
#'   `design = "eta_partial_ss"`, `design = "ges_partial_ss_mix"`, or
#'   `design = "ges_partial_ss_rm"`).
#' @param dfe Degrees of freedom for the error term (used when
#'   `design = "epsilon_full_ss"`, `design = "eta_f"`, `design = "omega_f"`, `design = "omega_full_ss"`, `design = "eta_full_ss"`,
#'   `design = "eta_partial_ss"`, `design = "ges_partial_ss_mix"`, or
#'   `design = "ges_partial_ss_rm"`).
#' @param msm Mean square for the model (used when
#'   `design = "epsilon_full_ss"` or `design = "omega_full_ss"`).
#' @param mse Mean square for the error (used when
#'   `design = "epsilon_full_ss"` or `design = "omega_full_ss"`).
#' @param sst Total sum of squares for the outcome (used when
#'   `design = "epsilon_full_ss"` or `design = "omega_full_ss"`).
#' @param ssm Sum of squares for the model term (used when
#'   `design = "eta_full_ss"`, `design = "eta_partial_ss"`,
#'   `design = "ges_partial_ss_mix"`, or `design = "ges_partial_ss_rm"`).
#' @param sss Sum of squares for the subject or between-subjects term
#'   (used when `design = "ges_partial_ss_mix"` or
#'   `design = "ges_partial_ss_rm"`).
#' @param sse Sum of squares for the error term (used when
#'   `design = "eta_partial_ss"` or `design = "ges_partial_ss_mix"`).
#' @param sse1 Sum of squares for the first error term (used when
#'   `design = "ges_partial_ss_rm"`).
#' @param sse2 Sum of squares for the second error term (used when
#'   `design = "ges_partial_ss_rm"`).
#' @param sse3 Sum of squares for the third error term (used when
#'   `design = "ges_partial_ss_rm"`).
#' @param f_value F statistic for the model term (used when
#'   `design = "eta_f"`, `design = "eta_full_ss"`, `design = "eta_partial_ss"`,
#'   `design = "ges_partial_ss_mix"`, `design = "ges_partial_ss_rm"`, or
#'   `design = "omega_f"`).
#' @param a Significance level used for confidence intervals. Defaults to 0.05.
#' @param design Character string indicating which r-family effect size
#'   design to use. See **Supported designs**.
#' @param ... Additional arguments for future methods (currently unused).
#'
#' @return
#' A list whose structure depends on the selected design. For
#' `design = "d_to_r"`, the returned object is the same as from
#' [d_to_r()].
#'
#' @export
#' @examples
#' # From Cohen's d for independent groups to r and R^2
#' r_effect(d = -1.88, n1 = 4, n2 = 4, a = .05, design = "d_to_r")
#' # From a sample correlation to r and R^2
#' r_effect(r = -0.8676594, n = 32, a = .05, design = "r_correl")
#' # From a chi-square test of association to Cramer's V
#' r_effect(x2 = 2.0496, n = 60, r = 3, c = 3, a = .05, design = "v_chi_sq")
#' # From F and degrees of freedom to eta^2
#' r_effect(dfm = 2, dfe = 8, f_value = 5.134, a = .05, design = "eta_f")
#' # From F, degrees of freedom, and N to omega^2
#' r_effect(dfm = 2, dfe = 8, n = 11, f_value = 5.134, a = .05, design = "omega_f")
#' # From sums of squares to omega^2
#' r_effect(
#'   dfm   = 2,
#'   dfe   = 8,
#'   msm   = 12.621,
#'   mse   = 2.548,
#'   sst   = (25.54 + 19.67),
#'   a     = .05,
#'   design = "omega_full_ss"
#' )
#' # From sums of squares to partial eta^2
#' r_effect(
#'   dfm    = 4,
#'   dfe    = 990,
#'   ssm    = 338057.9,
#'   sse    = 32833499,
#'   f_value = 2.548,
#'   a      = .05,
#'   design = "eta_partial_ss"
#' )
#' # From mixed-design sums of squares to partial generalized eta^2
#' r_effect(
#'   dfm     = 1,
#'   dfe     = 156,
#'   ssm     = 71.07608,
#'   sss     = 30936.498,
#'   sse     = 8657.094,
#'   f_value = 1.280784,
#'   a       = .05,
#'   design  = "ges_partial_ss_mix"
#' )
#'
#' # From repeated-measures sums of squares to partial generalized eta^2
#' r_effect(
#'   dfm     = 1,
#'   dfe     = 157,
#'   ssm     = 2442.948,
#'   sss     = 76988.13,
#'   sse1    = 5402.567,
#'   sse2    = 8318.75,
#'   sse3    = 6074.417,
#'   f_value = 70.9927,
#'   a       = .05,
#'   design  = "ges_partial_ss_rm"
#' )
#'
r_effect <- function(d = NULL,
                     n1 = NULL,
                     n2 = NULL,
                     r = NULL,
                     n = NULL,
                     x2 = NULL,
                     c = NULL,
                     dfm = NULL,
                     dfe = NULL,
                     msm = NULL,
                     mse = NULL,
                     sst = NULL,
                     ssm = NULL,
                     sss = NULL,
                     sse = NULL,
                     sse1 = NULL,
                     sse2 = NULL,
                     sse3 = NULL,
                     f_value = NULL,
                     a = 0.05,
                     design,
                     ...) {

  design <- match.arg(
    design,
    choices = c(
      "d_to_r",
      "r_correl",
      "v_chi_sq",
      "epsilon_full_ss",
      "eta_f",
      "omega_f",
      "omega_full_ss",
      "eta_full_ss",
      "eta_partial_ss",
      "ges_partial_ss_mix",
      "ges_partial_ss_rm"
    )
  )

  if (design == "d_to_r") {
    if (is.null(d) || is.null(n1) || is.null(n2)) {
      stop(
        "For design = 'd_to_r', you must supply d, n1, and n2."
      )
    }

    return(
      d_to_r(
        d  = d,
        n1 = n1,
        n2 = n2,
        a  = a
      )
    )
  }

  if (design == "r_correl") {
    if (is.null(r) || is.null(n)) {
      stop(
        "For design = 'r_correl', you must supply r and n."
      )
    }

    return(
      r_correl(
        r = r,
        n = n,
        a = a
      )
    )
  }

  if (design == "v_chi_sq") {
    if (is.null(x2) || is.null(n) ||
        is.null(r)  || is.null(c)) {
      stop(
        "For design = 'v_chi_sq', you must supply x2, n, r, and c."
      )
    }

    return(
      v_chi_sq(
        x2 = x2,
        n  = n,
        r  = r,
        c  = c,
        a  = a
      )
    )
  }

  if (design == "epsilon_full_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(msm) || is.null(mse) ||
        is.null(sst)) {
      stop(
        "For design = 'epsilon_full_ss', you must supply dfm, 
        dfe, msm, mse, and sst."
      )
    }

    return(
      epsilon_full_ss(
        dfm = dfm,
        dfe = dfe,
        msm = msm,
        mse = mse,
        sst = sst,
        a   = a
      )
    )
  }

  if (design == "eta_f") {
    if (is.null(dfm) || is.null(dfe) || is.null(f_value)) {
      stop(
        "For design = 'eta_f', you must supply dfm, dfe, and f_value."
      )
    }

    return(
      eta_f(
        dfm    = dfm,
        dfe    = dfe,
        f_value = f_value,
        a      = a
      )
    )
  }

  if (design == "omega_f") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(n)   || is.null(f_value)) {
      stop(
        "For design = 'omega_f', you must supply dfm, dfe, n, and f_value."
      )
    }

    return(
      omega_f(
        dfm     = dfm,
        dfe     = dfe,
        f_value = f_value,
        n       = n,
        a       = a
      )
    )
  }

  if (design == "omega_full_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(msm) || is.null(mse) ||
        is.null(sst)) {
      stop(
        "For design = 'omega_full_ss', you must supply dfm, dfe, msm, mse, and sst."
      )
    }

    return(
      omega_full_ss(
        dfm = dfm,
        dfe = dfe,
        msm = msm,
        mse = mse,
        sst = sst,
        a   = a
      )
    )
  }

  if (design == "eta_full_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(ssm) || is.null(sst) ||
        is.null(f_value)) {
      stop(
        "For design = 'eta_full_ss', you must supply dfm, dfe, 
        ssm, sst, and f_value."
      )
    }

    return(
      eta_full_ss(
        dfm    = dfm,
        dfe    = dfe,
        ssm    = ssm,
        sst    = sst,
        f_value = f_value,
        a      = a
      )
    )
  }

  if (design == "eta_partial_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(ssm) || is.null(sse) ||
        is.null(f_value)) {
      stop(
        "For design = 'eta_partial_ss', you must supply dfm, dfe, 
        ssm, sse, and f_value."
      )
    }

    return(
      eta_partial_ss(
        dfm    = dfm,
        dfe    = dfe,
        ssm    = ssm,
        sse    = sse,
        f_value = f_value,
        a      = a
      )
    )
  }

  if (design == "ges_partial_ss_mix") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(ssm) || is.null(sss) ||
        is.null(sse) || is.null(f_value)) {
      stop(
        "For design = 'ges_partial_ss_mix', you must supply dfm, 
        dfe, ssm, sss, sse, and f_value."
      )
    }

    return(
      ges_partial_ss_mix(
        dfm    = dfm,
        dfe    = dfe,
        ssm    = ssm,
        sss    = sss,
        sse    = sse,
        f_value = f_value,
        a      = a
      )
    )
  }

  if (design == "ges_partial_ss_rm") {
    if (is.null(dfm)  || is.null(dfe)  ||
        is.null(ssm)  || is.null(sss)  ||
        is.null(sse1) || is.null(sse2) || is.null(sse3) ||
        is.null(f_value)) {
      stop(
        "For design = 'ges_partial_ss_rm', you must supply dfm, dfe, ssm, sss, sse1, sse2, sse3, and f_value."
      )
    }

    return(
      ges_partial_ss_rm(
        dfm     = dfm,
        dfe     = dfe,
        ssm     = ssm,
        sss     = sss,
        sse1    = sse1,
        sse2    = sse2,
        sse3    = sse3,
        f_value = f_value,
        a       = a
      )
    )
  }
}