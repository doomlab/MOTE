#' $d_g$ Corrected for Independent t
#'
#'
#' This function displays $d_g$ (Hedges' g) corrected
#' and the non-central confidence interval for independent $t$.
#'
#' The small-sample correction factor is:
#'
#'     $$\text{correction} = 1 - \frac{3}{4(n_1 + n_2) - 9}$$
#'
#' $d_g$ is computed as the standardized mean difference multiplied by the correction:
#'
#'     $$d_g = \frac{m_1 - m_2}{s_\text{pooled}} \times \text{correction}$$
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
#'   \item{d}{$d_g$ corrected effect size}
#'   \item{dlow}{lower level confidence interval for $d_g$}
#'   \item{dhigh}{upper level confidence interval for $d_g$}
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
#'   \item{df}{degrees of freedom ($n_1 - 1 + n_2 - 1$)}
#'   \item{t}{t-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the $d_g$ statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the t-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, independent t, correction
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
#'     g.ind.t(m1 = 17.75, m2 = 23, sd1 = 3.30,
#'            sd2 = 2.16, n1 = 4, n2 = 4, a = .05)
#'
#'     g.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     g.ind.t(mean(indt_data$correctq[indt_data$group == 1]),
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

g.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05) {

  if (missing(m1)){
    stop("Be sure to include m1 for the first mean.")
  }

  if (missing(m2)){
    stop("Be sure to include m2 for the second mean.")
  }

  if (missing(sd1)){
    stop("Be sure to include sd1 for the first mean.")
  }

  if (missing(sd2)){
    stop("Be sure to include sd2 for the second mean.")
  }

  if (missing(n1)){
    stop("Be sure to include the sample size n1 for the first group.")
  }

  if (missing(n2)){
    stop("Be sure to include the sample size n2 for the second group.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  correction <- 1 - (3 / (4 * (n1 + n2) - 9))
  spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- ((m1 - m2) / spooled) * correction
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ncpboth <- noncentral_t(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- correction * (ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2))))
  dhigh <- correction * (ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2))))
  M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = FALSE) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,TRUE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "M1" = m1, #group 1 stats
                "sd1" = sd1,
                "se1" = se1,
                "M1low" = M1low,
                "M1high" = M1high,
                "M2" = m2, #group 2 stats
                "sd2" = sd2,
                "se2" = se2,
                "M2low" = M2low,
                "M2high" = M2high,
                "spooled" = spooled,
                "sepooled" = sepooled,
                "correction" = correction,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p,
                "estimate" = paste("$d_{g}$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n1 - 1 + n2 - 1), ") = ", apa(t,2,TRUE), ", $p$ ", reportp, sep = "")
  )
  return(output)
}


