#' \eqn{d_g} Corrected for Independent \eqn{t}
#'
#'
#' This function displays \eqn{d_g} (Hedges' g) corrected
#' and the non-central confidence interval for independent \eqn{t}.
#'
#' The small-sample correction factor is:
#'
#' \deqn{\mathrm{correction} = 1 - \frac{3}{4(n_1 + n_2) - 9}}
#'
#' \eqn{d_g} is computed as the standardized mean difference multiplied
#' by the correction:
#'
#' \deqn{d_g = \frac{m_1 - m_2}{s_{\mathrm{pooled}}} \times \mathrm{correction}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtg.html}{Learn more on our example page.}
#'
#' @param m1 mean group one
#' @param m2 mean group two
#' @param sd1 standard deviation group one
#' @param sd2 standard deviation group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return \describe{
#'   \item{d}{\eqn{d_g} corrected effect size}
#'   \item{dlow}{lower level confidence interval for \eqn{d_g}}
#'   \item{dhigh}{upper level confidence interval for \eqn{d_g}}
#'   \item{M1}{mean of group one}
#'   \item{sd1}{standard deviation of group one}
#'   \item{se1}{standard error of group one}
#'   \item{M1low}{lower level confidence interval of mean one}
#'   \item{M1high}{upper level confidence interval of mean one}
#'   \item{M2}{mean of group two}
#'   \item{sd2}{standard deviation of group two}
#'   \item{se2}{standard error of group two}
#'   \item{M2low}{lower level confidence interval of mean two}
#'   \item{M2high}{upper level confidence interval of mean two}
#'   \item{spooled}{pooled standard deviation}
#'   \item{sepooled}{pooled standard error}
#'   \item{correction}{Hedges' small-sample correction factor}
#'   \item{n1}{sample size of group one}
#'   \item{n2}{sample size of group two}
#'   \item{df}{degrees of freedom (\eqn{n_1 - 1 + n_2 - 1})}
#'   \item{t}{\eqn{t}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{d_g} statistic and confidence interval
#' in APA style for markdown printing}
#'   \item{statistic}{the \eqn{t}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size#' @keywords effect size omega ANOVA independent t correction
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "indt_data"
#' # dataset, included in the MOTE library.
#'
#' # A forensic psychologist conducted a study to examine whether
#' # being hypnotized during recall affects how well a witness
#' # can remember facts about an event. Eight participants
#' # watched a short film of a mock robbery, after which
#' # each participant was questioned about what he or she had
#' # seen. The four participants in the experimental group
#' # were questioned while they were hypnotized. The four
#' # participants in the control group received the same
#' # questioning without hypnosis.
#'
#'    t.test(correctq ~ group, data = indt_data)
#'
#' # You can type in the numbers directly, or refer to the dataset,
#' # as shown below.
#'
#'     g_ind_t(m1 = 17.75, m2 = 23, sd1 = 3.30,
#'            sd2 = 2.16, n1 = 4, n2 = 4, a = .05)
#'
#'     g_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     g_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
#'             mean(indt_data$correctq[indt_data$group == 2]),
#'             sd(indt_data$correctq[indt_data$group == 1]),
#'             sd(indt_data$correctq[indt_data$group == 2]),
#'             length(indt_data$correctq[indt_data$group == 1]),
#'             length(indt_data$correctq[indt_data$group == 2]),
#'             .05)
#'
#' # Contrary to the hypothesized result, the group that underwent hypnosis were
#' # significantly less accurate while reporting facts than the control group
#' # with a large effect size, t(6) = -2.66, p = .038, d_g = 1.64.
#'

g_ind_t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) {

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

  correction <- 1 - (3 / (4 * (n1 + n2) - 9))
  s_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

  d_value <- ((m1 - m2) / s_pooled) * correction

  se_1 <- sd1 / sqrt(n1)
  se_2 <- sd2 / sqrt(n2)

  se_pooled <- sqrt((s_pooled^2 / n1 + s_pooled^2 / n2))

  t_value <- (m1 - m2) / se_pooled

  ncp_limits <- noncentral_t(
    t_value,
    (n1 - 1 + n2 - 1),
    conf_level = (1 - a),
    sup_int_warns = TRUE
  )

  d_lower <- correction * (
    ncp_limits$lower_limit / sqrt((n1 * n2) / (n1 + n2))
  )
  d_upper <- correction * (
    ncp_limits$upper_limit / sqrt((n1 * n2) / (n1 + n2))
  )

  m1_lower <- m1 - se_1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  m1_upper <- m1 + se_1 * qt(a / 2, n1 - 1, lower.tail = FALSE)

  m2_lower <- m2 - se_2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  m2_upper <- m2 + se_2 * qt(a / 2, n2 - 1, lower.tail = FALSE)

  p_value <- pt(abs(t_value), (n1 - 1 + n2 - 1), lower.tail = FALSE) * 2

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, TRUE), sep = "")
  }

  estimate <- paste(
    "$d_{g}$ = ",
    apa(d_value, 2, TRUE),
    ", ",
    (1 - a) * 100,
    "\\% CI [",
    apa(d_lower, 2, TRUE),
    ", ",
    apa(d_upper, 2, TRUE),
    "]",
    sep = ""
  )

  statistic <- paste(
    "$t$(",
    (n1 - 1 + n2 - 1),
    ") = ",
    apa(t_value, 2, TRUE),
    ", $p$ ",
    report_p,
    sep = ""
  )

  output <- list(
    # Legacy names
    d        = d_value,
    dlow     = d_lower,
    dhigh    = d_upper,
    M1       = m1,
    sd1      = sd1,
    se1      = se_1,
    M1low    = m1_lower,
    M1high   = m1_upper,
    M2       = m2,
    sd2      = sd2,
    se2      = se_2,
    M2low    = m2_lower,
    M2high   = m2_upper,
    spooled  = s_pooled,
    sepooled = se_pooled,
    correction = correction,
    n1       = n1,
    n2       = n2,
    df       = (n1 - 1 + n2 - 1),
    t        = t_value,
    p        = p_value,
    estimate = estimate,
    statistic = statistic,

    # Snake_case aliases
    d_value        = d_value,
    d_lower_limit  = d_lower,
    d_upper_limit  = d_upper,
    mean1_value    = m1,
    mean1_lower    = m1_lower,
    mean1_upper    = m1_upper,
    mean2_value    = m2,
    mean2_lower    = m2_lower,
    mean2_upper    = m2_upper,
    sample_sd1     = sd1,
    sample_sd2     = sd2,
    sample_se1     = se_1,
    sample_se2     = se_2,
    pooled_sd      = s_pooled,
    pooled_se      = se_pooled,
    correction_value = correction,
    sample_size1   = n1,
    sample_size2   = n2,
    degrees_freedom = (n1 - 1 + n2 - 1),
    t_value        = t_value,
    p_value        = p_value
  )
  return(output)
}

# Backwards compatibility with previous naming convention
#' @rdname g_ind_t
#' @export
g.ind.t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) { #nolint
  g_ind_t(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, a = a)
}
