#' \eqn{d_{\delta}} for Between Subjects with Control Group SD Denominator
#'
#' This function displays \eqn{d_{\delta}} for between subjects data
#' and the non-central confidence interval using the
#' control group standard deviation as the denominator.
#'
#' To calculate \eqn{d_{\delta}}, the mean of the experimental group
#' is subtracted from the mean of the control group, which
#' is divided by the standard deviation of the control group.
#'
#' \deqn{d_{\delta} = \frac{m_1 - m_2}{sd_1}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtdelta.html}{Learn more on our example page.}
#'
#' @param m1 mean from control group
#' @param m2 mean from experimental group
#' @param sd1 standard deviation from control group
#' @param sd2 standard deviation from experimental group
#' @param n1 sample size from control group
#' @param n2 sample size from experimental group
#' @param a significance level
#' @return Provides the effect size (Cohen's d) with associated
#' confidence intervals, the t-statistic, the confidence intervals
#' associated with the means of each group, as well as the
#' standard deviations and standard errors of the means for each group.
#'
#' \item{d}{d-delta effect size}
#' \item{dlow}{lower level confidence interval of d-delta value}
#' \item{dhigh}{upper level confidence interval of d-delta value}
#' \item{M1}{mean of group one}
#' \item{sd1}{standard deviation of group one mean}
#' \item{se1}{standard error of group one mean}
#' \item{M1low}{lower level confidence interval of group one mean}
#' \item{M1high}{upper level confidence interval of group one mean}
#' \item{M2}{mean of group two}
#' \item{sd2}{standard deviation of group two mean}
#' \item{se2}{standard error of group two mean}
#' \item{M2low}{lower level confidence interval of group two mean}
#' \item{M2high}{upper level confidence interval of group two mean}
#' \item{spooled}{pooled standard deviation}
#' \item{sepooled}{pooled standard error}
#' \item{n1}{sample size of group one}
#' \item{n2}{sample size of group two}
#' \item{df}{degrees of freedom (n1 - 1 + n2 - 1)}
#' \item{t}{t-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size delta independent t
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
#'     hyp <- t.test(correctq ~ group, data = indt_data)
#'
#' # You can type in the numbers directly, or refer to the dataset,
#' # as shown below.
#'
#'     delta_ind_t(m1 = 17.75, m2 = 23,
#'                sd1 = 3.30, sd2 = 2.16,
#'                 n1 = 4, n2 = 4, a = .05)
#'
#'     delta_ind_t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     delta_ind_t(mean(indt_data$correctq[indt_data$group == 1]),
#'             mean(indt_data$correctq[indt_data$group == 2]),
#'             sd(indt_data$correctq[indt_data$group == 1]),
#'             sd(indt_data$correctq[indt_data$group == 2]),
#'             length(indt_data$correctq[indt_data$group == 1]),
#'             length(indt_data$correctq[indt_data$group == 2]),
#'             .05)
#'
#' # Contrary to the hypothesized result, the group that underwent hypnosis were
#' # significantly less accurate while reporting facts than the control group
#' # with a large effect size, t(6) = -2.66, p = .038, d_delta = 1.59.
#'
delta_ind_t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) {

  s_pooled <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  d_value <- (m1 - m2) / sd1
  se_1 <- sd1 / sqrt(n1)
  se_2 <- sd2 / sqrt(n2)
  se_pooled <- sqrt((s_pooled^2 / n1 + s_pooled^2 / n2))
  t_value <- (m1 - m2) / se_pooled

  ncp_limits <- noncentral_t(t_value, (n1 - 1 + n2 - 1),
                             conf_level = (1 - a), sup_int_warns = TRUE)
  d_lower <- ncp_limits$lower_limit / sqrt((n1 * n2) / (n1 + n2))
  d_upper <- ncp_limits$upper_limit / sqrt((n1 * n2) / (n1 + n2))

  m1_lower <- m1 - se_1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  m1_upper <- m1 + se_1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  m2_lower <- m2 - se_2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  m2_upper <- m2 + se_2 * qt(a / 2, n2 - 1, lower.tail = FALSE)

  p_value <- pt(abs(t_value), (n1 - 1 + n2 - 1), lower.tail = FALSE) * 2

  if (p_value < .001) {
    report_p <- "< .001"
  } else {
    report_p <- paste("= ", apa(p_value, 3, FALSE), sep = "")
  }

  output <- list(
    # Legacy names
    d       = d_value,
    dlow    = d_lower,
    dhigh   = d_upper,
    M1      = m1,
    sd1     = sd1,
    se1     = se_1,
    M1low   = m1_lower,
    M1high  = m1_upper,
    M2      = m2,
    sd2     = sd2,
    se2     = se_2,
    M2low   = m2_lower,
    M2high  = m2_upper,
    spooled = s_pooled,
    sepooled = se_pooled,
    n1      = n1,
    n2      = n2,
    df      = (n1 - 1 + n2 - 1),
    t       = t_value,
    p       = p_value,
    estimate = paste(
      "$d_{delta}$ = ", apa(d_value, 2, TRUE), ", ", (1 - a) * 100,
      "\\% CI [", apa(d_lower, 2, TRUE), ", ", apa(d_upper, 2, TRUE), "]",
      sep = ""
    ),
    statistic = paste(
      "$t$(", (n1 - 1 + n2 - 1), ") = ", apa(t_value, 2, TRUE),
      ", $p$ ", report_p,
      sep = ""
    ),

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
    sample_size1   = n1,
    sample_size2   = n2,
    t_value        = t_value,
    p_value        = p_value
  )

  return(output)

}

# Backward compatibility wrapper
#' @rdname delta_ind_t
#' @export
delta.ind.t <- function(m1, m2, sd1, sd2, n1, n2, a = .05) { # nolint
  delta_ind_t(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2, n1 = n1, n2 = n2, a = a)
}
