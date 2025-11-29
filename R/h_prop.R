#' Cohen's h for Independent Proportions
#'
#' This function computes Cohen's \eqn{h} effect size for the difference
#' between two independent proportions. Cohen's \eqn{h} is defined as a
#' difference between arcsine-transformed proportions:
#'
#' \deqn{h = 2 \arcsin \sqrt{p_1} - 2 \arcsin \sqrt{p_2}}
#'
#' where \eqn{p_1} and \eqn{p_2} are proportions for groups 1 and 2,
#' respectively.
#'
#' Using a simple large-sample approximation (via the delta method), the
#' standard error of \eqn{h} can be taken as:
#'
#' \deqn{\mathrm{SE}(h) \approx \sqrt{1 / n_1 + 1 / n_2}},
#'
#' which leads to a \eqn{(1 - \alpha)} confidence interval for \eqn{h}:
#'
#' \deqn{h \pm z_{1 - \alpha/2} \, \mathrm{SE}(h).}
#'
#' This effect size is commonly recommended for differences in proportions
#' (Cohen, 1988) and is particularly useful for power analysis and
#' meta-analysis when working directly with proportions.
#'
#' @param p1 Proportion for group one (between 0 and 1).
#' @param p2 Proportion for group two (between 0 and 1).
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level used for confidence intervals. Defaults to 0.05.
#'
#' @return
#' A list containing Cohen's \eqn{h} effect size and related statistics:
#' \itemize{
#'   \item `h` – Cohen's h.
#'   \item `hlow`, `hhigh` – lower and upper confidence interval limits.
#'   \item `h_lower_limit`, `h_upper_limit` – snake_case aliases for the
#'     confidence limits.
#'   \item `p1`, `p2` – input proportions for each group.
#'   \item `n1`, `n2` – sample sizes for each group, with snake_case
#'     aliases `sample_size_1`, `sample_size_2`.
#'   \item `z`, `p` – z statistic and p value for the difference in
#'     proportions using a pooled-proportion standard error.
#'   \item `z_value`, `p_value` – snake_case aliases for the z statistic
#'     and p value.
#'   \item `estimate` – APA-style formatted string for
#'     Cohen's h and its confidence interval.
#'   \item `statistic` – APA-style formatted string
#'     for the z test of the difference in proportions.
#' }
#'
#' @export
#' @examples
#' h_prop(p1 = .25, p2 = .35, n1 = 100, n2 = 100, a = .05)

h_prop <- function(p1, p2, n1, n2, a = .05) {

  # --- Argument checks / warnings ---
  if (missing(p1)) {
    stop("Be sure to include p1 for the first proportion.")
  }

  if (missing(p2)) {
    stop("Be sure to include p2 for the second proportion.")
  }

  if (p1 > 1 || p2 > 1 || p1 < 0 || p2 < 0) {
    stop(
      "Be sure to enter your values as proportions (between 0 and 1). ",
      "Do not enter percentages, and make sure all 
      proportion values are positive."
    )
  }

  if (missing(n1)) {
    stop("Be sure to include the sample size n1 for the first group.")
  }

  if (missing(n2)) {
    stop("Be sure to include the sample size n2 for the second group.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  # --- Cohen's h computation ---
  h <- 2 * asin(sqrt(p1)) - 2 * asin(sqrt(p2))

  # Delta-method SE: var(h) ≈ 1/n1 + 1/n2
  se_h <- sqrt(1 / n1 + 1 / n2)

  crit <- qnorm(a / 2, lower.tail = FALSE)

  hlow  <- h - crit * se_h
  hhigh <- h + crit * se_h

  # Difference in proportions test using z (for reporting)
  ppooled <- (p1 * n1 + p2 * n2) / (n1 + n2)

  se_z <- sqrt(ppooled * (1 - ppooled) * ((1 / n1) + (1 / n2)))

  z_value <- (p1 - p2) / se_z

  p_value <- pnorm(abs(z_value), lower.tail = FALSE) * 2

  if (p_value < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  estimate <- paste(
    "$h$ = ", apa(h, 2, TRUE), ", ", (1 - a) * 100, "\\% CI [",
    apa(hlow, 2, TRUE), ", ", apa(hhigh, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$Z$ = ", apa(z_value, 2, TRUE), ", $p$ ", reportp,
    sep = ""
  )

  output <- list(
    # Effect size (original)
    h    = h,
    hlow = hlow,
    hhigh = hhigh,

    # Snake_case aliases for h
    h_lower_limit = hlow,
    h_upper_limit = hhigh,

    # Proportions
    p1 = p1,
    p2 = p2,

    # Sample sizes
    n1 = n1,
    n2 = n2,

    # Sample sizes snake_case
    sample_size_1 = n1,
    sample_size_2 = n2,

    # Test statistics (z-based for proportions)
    z = z_value,
    p = p_value,

    # Snake_case test statistics
    z_value = z_value,
    p_value = p_value,

    # APA strings
    estimate  = estimate,
    statistic = statistic
  )

  return(output)
}

# Backward compatibility wrapper
#' @rdname h_prop
#' @export
h.prop <- function(p1, p2, n1, n2, a = .05) { # nolint
  h_prop(p1 = p1, p2 = p2, n1 = n1, n2 = n2, a = a)
}