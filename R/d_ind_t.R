#' Cohen's d for Independent Samples Using the Pooled SD
#'
#' Compute Cohen's \eqn{d_s} for between-subjects designs and a noncentral-t
#' confidence interval using the **pooled standard deviation**
#' as the denominator.
#'
#' @details
#' The pooled standard deviation is:
#' \deqn{s_{pooled} = \sqrt{ \frac{ (n_1 - 1)s_1^2 + (n_2 - 1)s_2^2 }
#' {n_1 + n_2 - 2} }}
#'
#' Cohen's \eqn{d_s} is then:
#' \deqn{d_s = \frac{m_1 - m_2}{s_{pooled}}}
#'
#' The corresponding t-statistic is:
#' \deqn{t = \frac{m_1 - m_2}{ \sqrt{ s_{pooled}^2/n_1 + s_{pooled}^2/n_2 }}}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/indtm.html}{Learn more on our example page.}
#'
#' @param m1 Mean of group one.
#' @param m2 Mean of group two.
#' @param sd1 Standard deviation of group one.
#' @param sd2 Standard deviation of group two.
#' @param n1 Sample size of group one.
#' @param n2 Sample size of group two.
#' @param a Significance level (alpha) for the confidence interval.
#' Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_s}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval
#' for \eqn{d_s}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval
#' for \eqn{d_s}.}
#'   \item{M1, M2}{Group means.}
#'   \item{sd1, sd2}{Standard deviations for each group.}
#'   \item{se1, se2}{Standard errors for each group mean.}
#'   \item{M1low, M1high, M2low, M2high}{Confidence interval bounds
#' for each group mean.}
#'   \item{spooled}{Pooled standard deviation.}
#'   \item{sepooled}{Pooled standard error.}
#'   \item{n1, n2}{Group sample sizes.}
#'   \item{df}{Degrees of freedom (\eqn{n_1 - 1 + n_2 - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting
#' \eqn{d_s} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting
#' the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, independent t-test, between-subjects,
#' pooled standard deviation
#' @import stats
#' @export
#'
#' @examples
#' # The following example is derived from the "indt_data" dataset
#' # included in MOTE.
#'
#' # A forensic psychologist examined whether being hypnotized during recall
#' # affects how well a witness remembers facts about an event.
#'
#' t.test(correctq ~ group, data = indt_data)
#'
#' # Direct entry of summary statistics:
#' d_ind_t(m1 = 17.75, m2 = 23, sd1 = 3.30,
#'         sd2 = 2.16, n1 = 4, n2 = 4, a = .05)
#'
#' # Equivalent shorthand:
#' d_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#' # Using raw data from the dataset:
#' d_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
#'         mean(indt_data$correctq[indt_data$group == 2]),
#'         sd(indt_data$correctq[indt_data$group == 1]),
#'         sd(indt_data$correctq[indt_data$group == 2]),
#'         length(indt_data$correctq[indt_data$group == 1]),
#'         length(indt_data$correctq[indt_data$group == 2]),
#'         .05)

d_ind_t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) {

  if (missing(m1)) {
    stop("Be sure to include m1 for the first mean.")
  }

  if (missing(m2)) {
    stop("Be sure to include m2 for the second mean.")
  }

  if (missing(sd1)) {
    stop("Be sure to include sd1 for the first mean.")
  }

  if (missing(sd2)) {
    stop("Be sure to include sd2 for the second mean.")
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

  spooled <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- (m1 - m2) / spooled
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ncp_limits <- noncentral_t(
    ncp        = t,
    df         = (n1 - 1 + n2 - 1),
    conf_level = 1 - a,
    sup_int_warns = TRUE
  )

  dlow  <- ncp_limits$lower_limit / sqrt((n1 * n2) / (n1 + n2))
  dhigh <- ncp_limits$upper_limit / sqrt((n1 * n2) / (n1 + n2))

  m1_low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  m1_high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  m2_low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  m2_high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = FALSE) * 2

  if (p < .001) {
    reportp <- "< .001"
  } else {
    reportp <- paste("= ", apa(p, 3, FALSE), sep = "")
  }

  output <- list(
    # ---- Original names (backward compatible) ----
    d      = d,
    dlow   = dlow,
    dhigh  = dhigh,

    M1     = m1,
    sd1    = sd1,
    se1    = se1,
    M1low  = m1_low,
    M1high = m1_high,

    M2     = m2,
    sd2    = sd2,
    se2    = se2,
    M2low  = m2_low,
    M2high = m2_high,

    spooled  = spooled,
    sepooled = sepooled,

    n1     = n1,
    n2     = n2,
    df     = (n1 - 1 + n2 - 1),

    t      = t,
    p      = p,

    estimate = paste(
      "$d_s$ = ", apa(d, 2, TRUE), ", ",
      (1 - a) * 100, "\\% CI [",
      apa(dlow, 2, TRUE), ", ",
      apa(dhigh, 2, TRUE), "]",
      sep = ""
    ),

    statistic = paste(
      "$t$(", (n1 - 1 + n2 - 1), ") = ",
      apa(t, 2, TRUE), ", $p$ ", reportp,
      sep = ""
    ),

    # ---- Snake_case aliases ----
    d_lower_limit        = dlow,
    d_upper_limit        = dhigh,

    m1_value             = m1,
    sd1_value            = sd1,
    se1_value            = se1,
    m1_lower_limit       = m1_low,
    m1_upper_limit       = m1_high,

    m2_value             = m2,
    sd2_value            = sd2,
    se2_value            = se2,
    m2_lower_limit       = m2_low,
    m2_upper_limit       = m2_high,

    pooled_sd            = spooled,
    pooled_se            = sepooled,

    sample_size_1        = n1,
    sample_size_2        = n2,
    degrees_freedom      = (n1 - 1 + n2 - 1),

    t_value              = t,
    p_value              = p
  )

  return(output)
}

# Backward compatibility wrapper
d.ind.t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) { # nolint
  d_ind_t(m1, m2, sd1, sd2, n1, n2, a)
}
