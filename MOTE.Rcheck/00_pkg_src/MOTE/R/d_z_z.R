#' Cohen's d from z-statistic for Z-test
#'
#' Compute Cohen's \eqn{d} from a z-statistic for a Z-test.
#'
#' @details
#' The effect size is computed as:
#' \deqn{d = \frac{z}{\sqrt{n}},}
#' where \eqn{n} is the sample size.
#'
#' The confidence interval bounds assume a normal-theory standard error for
#' \eqn{d} of \eqn{1 / \sqrt{n}} (given that \eqn{d = z / \sqrt{n}}). Thus:
#' \deqn{d_{\mathrm{low}} = d - z_{\alpha/2} \cdot 1/\sqrt{n}}
#' \deqn{d_{\mathrm{high}} = d + z_{\alpha/2} \cdot 1/\sqrt{n}}
#' where \eqn{z_{\alpha/2}} is the critical value from the standard normal
#' distribution.
#'
#' The population standard deviation (\eqn{\sigma}) is retained for descriptive
#' purposes but is not required for computing confidence intervals for \eqn{d}.
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/zz.html}{Learn more on our example page.}
#'
#' @param z z-statistic from a Z-test.
#' @param sig Population standard deviation (\eqn{\sigma}). This value is
#'   retained for descriptive purposes but is not required to compute the
#'   confidence interval for \eqn{d}.
#' @param n Sample size.
#' @param a Significance level (alpha) for the confidence interval.
#'  Must be in (0, 1).
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Effect size.}
#'   \item{dlow}{Lower confidence interval bound for \eqn{d}.}
#'   \item{dhigh}{Upper confidence interval bound for \eqn{d}.}
#'   \item{sigma}{Population standard deviation (\eqn{\sigma}).}
#'   \item{z}{z-statistic.}
#'   \item{p}{Two-tailed p-value.}
#'   \item{n}{Sample size.}
#'   \item{estimate}{The \eqn{d} statistic and confidence interval in
#' APA style for markdown printing.}
#'   \item{statistic}{The Z-statistic in APA style for markdown printing.}
#' }
#'
#' @keywords effect size z-test
#' @import stats
#' @export
#' @examples
#'
#' # A recent study suggested that students (N = 100) learning
#' # statistics improved their test scores with the use of
#' # visual aids (Z = 2.5). The population standard deviation is 4.
#'
#' # You can type in the numbers directly as shown below,
#' # or refer to your dataset within the function.
#'
#'     d_z_z(z = 2.5, sig = 4, n = 100, a = .05)
#'
#'     d_z_z(z = 2.5, n = 100, a = .05)
#'
#'     d.z.z(2.5, 4, 100, .05)


d_z_z <- function(z, n, a = .05, sig = NA) {

  if (missing(z)) {
    stop("Be sure to include z from the z-statistic.")
  }

  if (missing(n)) {
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- z / sqrt(n)

  # Standard error for d when d = z / sqrt(n)
  se_d <- 1 / sqrt(n)
  crit <- qnorm(a / 2, lower.tail = FALSE)

  dlow  <- d - crit * se_d
  dhigh <- d + crit * se_d

  p <- pnorm(abs(z), lower.tail = FALSE) * 2

  if (p < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$d$ = ", apa(d, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(dlow, 2, TRUE), ", ", apa(dhigh, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$Z$ = ", apa(z, 2, TRUE), ", $p$ ", reportp,
    sep = ""
  )

  output <- list(
    # Legacy names
    d     = d,
    dlow  = dlow,
    dhigh = dhigh,
    sigma = sig,
    z     = z,
    p     = p,
    n     = n,
    estimate  = estimate,
    statistic = statistic,

    # Snake_case aliases
    d_lower_limit = dlow,
    d_upper_limit = dhigh,
    sigma_value   = sig,
    population_sd = sig,
    z_value       = z,
    p_value       = p,
    sample_size   = n
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname d_z_z
#' @export
d.z.z <- function(z, sig = NA, n, a = .05) { # nolint
  d_z_z(z = z, n = n, a = a, sig = sig)
}
