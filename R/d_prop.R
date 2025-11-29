#' Cohen's d (SMD) for Independent Proportions (Binary Outcomes)
#'
#' This function computes a standardized mean difference effect size for two
#' independent proportions by treating each as the mean of a Bernoulli
#' (0/1) variable and computing a standardized mean difference (SMD)
#' directly using the pooled Bernoulli standard deviation. This follows
#' the same logic as Cohen's d for continuous variables, but applied to
#' binary outcomes:
#'
#' \deqn{d = \frac{p_1 - p_2}{s_{\mathrm{pooled}}}}
#'
#' where
#'
#' \deqn{s_{\mathrm{pooled}} = \sqrt{\frac{(n_1 - 1)p_1(1 - p_1) +
#'                                      (n_2 - 1)p_2(1 - p_2)}
#'                                      {n_1 + n_2 - 2}}}
#'
#' This replaces the original z‐based formulation used in older versions
#' of MOTE. The SMD effect size is directly comparable to all other d‐type
#' effect sizes in the package.
#'
#' @param p1 Proportion for group one (between 0 and 1).
#' @param p2 Proportion for group two (between 0 and 1).
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level used for confidence intervals. Defaults to 0.05.
#'
#' @return
#' A list with the same structure as [d_ind_t()], containing the standardized
#' mean difference and its confidence interval, along with auxiliary
#' statistics. The list is augmented with explicit entries `p1`, `p2`,
#' `p1_value`, and `p2_value` to emphasize that the original inputs were
#' proportions.
#'
#' @export
#' @examples
#' d_prop(p1 = .25, p2 = .35, n1 = 100, n2 = 100, a = .05)
d_prop <- function(p1, p2, n1, n2, a = .05) {

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
      "Do not enter percentages, and make sure all proportion values
      are positive."
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

  # --- Treat proportions as means of Bernoulli variables ---
  m1 <- p1
  m2 <- p2

  var1 <- p1 * (1 - p1)
  var2 <- p2 * (1 - p2)

  sd1 <- sqrt(var1)
  sd2 <- sqrt(var2)

  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)

  # Pooled SD for standardized mean difference
  df <- n1 + n2 - 2

  spooled <- sqrt(
    ((n1 - 1) * var1 + (n2 - 1) * var2) /
      df
  )

  # Standard error of the mean difference using pooled SD
  se_pooled <- spooled * sqrt((1 / n1) + (1 / n2))

  # Cohen's d-style SMD for proportions
  d <- (m1 - m2) / spooled

  # Use noncentral t to obtain CI limits for d,
  # analogous to the independent-groups d implementation.
  t_value <- (m1 - m2) / se_pooled

  ncp_limits <- noncentral_t(
    ncp          = t_value,
    df           = df,
    conf_level   = 1 - a,
    sup_int_warns = TRUE
  )

  n_eff <- (n1 * n2) / (n1 + n2)

  dlow  <- ncp_limits$lower_limit / sqrt(n_eff)
  dhigh <- ncp_limits$upper_limit / sqrt(n_eff)

  # Difference in proportions test using z (for reporting only)
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
    "$d_{prop}$ = ", apa(d, 2, TRUE), ", ",
    (1 - a) * 100, "\\% CI [",
    apa(dlow, 2, TRUE), ", ", apa(dhigh, 2, TRUE), "]",
    sep = ""
  )

  statistic <- paste(
    "$Z$ = ", apa(z_value, 2, TRUE),
    ", $p$ ", reportp,
    sep = ""
  )

  output <- list(
    # Effect size (original)
    d     = d,
    dlow  = dlow,
    dhigh = dhigh,

    # Snake_case aliases for d
    d_lower_limit = dlow,
    d_upper_limit = dhigh,

    # Group 1 stats
    p1    = p1,
    se1   = se1,

    # Group 1 snake_case
    p1_value  = p1,
    se1_value = se1,

    # Group 2 stats
    p2    = p2,
    se2   = se2,

    # Group 2 snake_case
    p2_value  = p2,
    se2_value = se2,

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
#' @rdname d_prop
#' @export
d.prop <- function(p1, p2, n1, n2, a = .05) { # nolint
  d_prop(p1 = p1, p2 = p2, n1 = n1, n2 = n2, a = a)
}