#' r-family effect size wrapper
#'
#' This function provides a unified interface for computing r- and
#' variance-based effect sizes (e.g., correlations and coefficients of
#' determination) from different input summaries. It is analogous to the
#' \code{d_effect()} wrapper for standardized mean difference effect sizes.
#'
#' Currently, `r_effect()` supports effect sizes derived from Cohen's d
#' for independent groups via the design `"d_to_r"`, which calls
#' [d_to_r()]. Additional designs (e.g., based on ANOVA sums of squares)
#' may be added in future versions.
#'
#' @section Supported designs:
#'
#' - `"d_to_r"` — correlation and R\eqn{^2} from Cohen's d for
#'   independent groups. Supply `d`, `n1`, and `n2`. In this case,
#'   `r_effect()` will call [d_to_r()] with the same arguments.
#'
#' - `"epsilon_full_ss"` — epsilon-squared (\eqn{\epsilon^2}) from an ANOVA
#'   table using model and error mean squares and the total sum of squares.
#'   Supply `dfm`, `dfe`, `msm`, `mse`, and `sst`. In this case,
#'   `r_effect()` will call [epsilon_full_ss()] with the same arguments.
#'
#' - `"eta_f"` — eta-squared (\eqn{\eta^2}) from an ANOVA F statistic and
#'   its associated degrees of freedom. Supply `dfm`, `dfe`, and `Fvalue`.
#'   In this case, `r_effect()` will call [eta_f()] with the same arguments.
#'
#' @param d Cohen's d value for the contrast of interest (used when
#'   `design = "d_to_r"`).
#' @param n1 Sample size for group one (used when `design = "d_to_r"`).
#' @param n2 Sample size for group two (used when `design = "d_to_r"`).
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
#' @param f_value F statistic for the model term (used when
#'   `design = "eta_f"`).
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
#' # From F and degrees of freedom to eta^2
#' r_effect(dfm = 2, dfe = 8, Fvalue = 5.134, a = .05, design = "eta_f")
#'
r_effect <- function(d = NULL,
                     n1 = NULL,
                     n2 = NULL,
                     dfm = NULL,
                     dfe = NULL,
                     msm = NULL,
                     mse = NULL,
                     sst = NULL,
                     f_value = NULL,
                     a = 0.05,
                     design,
                     ...) {

  design <- match.arg(design, choices = c("d_to_r", "epsilon_full_ss", "eta_f"))

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
    if (is.null(dfm) || is.null(dfe) || is.null(Fvalue)) {
      stop(
        "For design = 'eta_f', you must supply dfm, dfe, and Fvalue."
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
}