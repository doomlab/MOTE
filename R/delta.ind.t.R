#' d-delta for Between Subjects with Control Group SD Denominator
#'
#' This function displays d-delta for between subjects data
#' and the non-central confidence interval using the
#' control group standard deviation as the denominator.
#'
#' @param m1 mean from control group
#' @param m2 mean from experimental group
#' @param sd1 standard deviation from control group
#' @param sd2 standard deviation from experimental group
#' @param n1 sample size from control group
#' @param n2 sample size from experimental group
#' @param a significance level
#' @return Provides the effect size (Cohen's d) with associated confidence intervals,
#' the t-statistic, the confidence intervals associated with the means of each group, as well as the
#' standard deviations and standard errors of the means for each group.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval of d value}
#' \item{dhigh}{upper level confidence interval of d value}
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
#'
#' @keywords effect size, delta, independent t
#' @export
#' @examples
#'
#' #The following example is derived from the "indt_data" dataset, included
#' in the MOTE library.
#'
#' #A forensic psychologist conducted a study to examine whether
#' being hypnotized during recall affects how well a witness
#' can remember facts about an event. Eight participants
#' watched a short film of a mock robbery, after which
#' each participant was questioned about what he or she had
#' seen. The four participants in the experimental group
#' were questioned while they were hypnotized. The four
#' participants in the control group recieved the same
#' questioning without hypnosis.
#'
#'     hyp = t.test(correctq ~ group, data = indt_data)
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' as shown below.
#'
#'     delta.ind.t(m1 = 17.75, m2 = 23,
#'                sd1 = 3.30, sd2 = 2.16,
#'                 n1 = 4, n2 = 4, a = .05)
#'
#'     delta.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     delta.ind.t(hyp$estimate[1], hyp$estimate[2],
#'             sd(indt_data[1:4, ('correctq')]),
#'             sd(indt_data[5:8, ('correctq')]),
#'             length(indt_data[1:4, ('correctq')]),
#'             length(indt_data[5:8, ('correctq')]),
#'             .05)
#'
#' Contrary to the hypothesized result, the group that underwent hypnosis were
#' significantly less accurate while reporting facts than the control group
#' with a large effect size, t(7) = -2.66, p = .043, d = 1.88.
#' delta.ind.t(m1 = 20, m2 = 17, sd1 = 4, sd2 = 5, n1 = 100, n2 = 100, a = .05)


delta.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05) {
  # This function displays d-delta for between subjects data
  # and the non-central confidence interval using the
  # control group standard deviation as the denominator.
  #
  # Args:
  #   m1 : mean from control group
  #   m2 : mean from experimental group
  #   sd1: standard deviation from control group
  #   sd2: standard deviation from experimental group
  #   n1 : sample size from control group
  #   n2 : sample size from experimental group
  #   a  : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics

  library(MBESS)

  spooled <- sqrt(((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- (m1 - m2) / sd1
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "M1" = m1, #control group stats
                "sd1" = sd1,
                "se1" = se1,
                "M1low" = M1low,
                "M1high" = M1high,
                "M2" = m2, #experimental group stats
                "sd2" = sd2,
                "se2" = se2,
                "M2low" = M2low,
                "M2high" = M2high,
                "spooled" = spooled,
                "sepooled" = sepooled,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p)

  return(output)
}
