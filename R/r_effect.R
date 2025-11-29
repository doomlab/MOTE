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
#' @param d Cohen's d value for the contrast of interest (used when
#'   `design = "d_to_r"`).
#' @param n1 Sample size for group one (used when `design = "d_to_r"`).
#' @param n2 Sample size for group two (used when `design = "d_to_r"`).
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
#'
r_effect <- function(d = NULL,
                     n1 = NULL,
                     n2 = NULL,
                     a = 0.05,
                     design,
                     ...) {

  design <- match.arg(design, choices = c("d_to_r"))

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
}