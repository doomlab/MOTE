#' r-family effect size wrapper
#'
#' This function provides a unified interface for computing r- and
#' variance-based effect sizes (e.g., correlations and coefficients of
#' determination) from different input summaries. It is analogous to the
#' \code{d_effect()} wrapper for standardized mean difference effect sizes.
#'
#' Currently, `r_effect()` supports effect sizes derived from Cohen's d,
#' from correlations, and from ANOVA summaries via several designs (see
#' **Supported designs**). These designs call lower-level functions such
#' as [d_to_r()], [r_correl()], [epsilon_full_ss()], [eta_f()], and
#' [eta_full_ss()] with the appropriate arguments.
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
#' - `"epsilon_full_ss"` — epsilon-squared (\eqn{\epsilon^2}) from an ANOVA
#'   table using model and error mean squares and the total sum of squares.
#'   Supply `dfm`, `dfe`, `msm`, `mse`, and `sst`. In this case,
#'   `r_effect()` will call [epsilon_full_ss()] with the same arguments.
#'
#' - `"eta_f"` — eta-squared (\eqn{\eta^2}) from an ANOVA F statistic and
#'   its associated degrees of freedom. Supply `dfm`, `dfe`, and `f_value`.
#'   In this case, `r_effect()` will call [eta_f()] with the same arguments.
#'
#' - `"eta_full_ss"` — eta-squared (\eqn{\eta^2}) from ANOVA sums of squares,
#'   using the model sum of squares and total sum of squares along with the
#'   model and error degrees of freedom. Supply `dfm`, `dfe`, `ssm`, `sst`,
#'   and `f_value`. In this case, `r_effect()` will call [eta_full_ss()] with
#'   the same arguments.
#'
#' @param d Cohen's d value for the contrast of interest (used when
#'   `design = "d_to_r"`).
#' @param n1 Sample size for group one (used when `design = "d_to_r"`).
#' @param n2 Sample size for group two (used when `design = "d_to_r"`).
#' @param r Sample Pearson correlation coefficient (used when
#'   `design = "r_correl"`).
#' @param n Sample size for the correlation (used when
#'   `design = "r_correl"`).
#' @param dfm Degrees of freedom for the model term (used when
#'   `design = "epsilon_full_ss"`).
#' @param dfe Degrees of freedom for the error term (used when
#'   `design = "epsilon_full_ss"`).
#' @param msm Mean square for the model (used when
#'   `design = "epsilon_full_ss"`).
#' @param mse Mean square for the error (used when
#'   `design = "epsilon_full_ss"`).
#' @param sst Total sum of squares for the outcome (used when
#'   `design = "epsilon_full_ss"`).
#' @param ssm Sum of squares for the model term (used when
#'   `design = "eta_full_ss"`).
#' @param f_value F statistic for the model term (used when
#'   `design = "eta_f"` or `design = "eta_full_ss"`).
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
#' # From F and degrees of freedom to eta^2
#' r_effect(dfm = 2, dfe = 8, f_value = 5.134, a = .05, design = "eta_f")
#'
r_effect <- function(d = NULL,
                     n1 = NULL,
                     n2 = NULL,
                     r = NULL,
                     n = NULL,
                     dfm = NULL,
                     dfe = NULL,
                     msm = NULL,
                     mse = NULL,
                     sst = NULL,
                     ssm = NULL,
                     f_value = NULL,
                     a = 0.05,
                     design,
                     ...) {

  design <- match.arg(
    design,
    choices = c("d_to_r", "r_correl", "epsilon_full_ss", "eta_f", "eta_full_ss")
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

  if (design == "epsilon_full_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(msm) || is.null(mse) ||
        is.null(sst)) {
      stop(
        "For design = 'epsilon_full_ss', you must supply dfm, dfe, msm, mse, and sst."
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
        Fvalue = f_value,
        a      = a
      )
    )
  }

  if (design == "eta_full_ss") {
    if (is.null(dfm) || is.null(dfe) ||
        is.null(ssm) || is.null(sst) ||
        is.null(f_value)) {
      stop(
        "For design = 'eta_full_ss', you must supply dfm, dfe, ssm, sst, and f_value."
      )
    }

    return(
      eta_full_ss(
        dfm    = dfm,
        dfe    = dfe,
        ssm    = ssm,
        sst    = sst,
        Fvalue = f_value,
        a      = a
      )
    )
  }
}