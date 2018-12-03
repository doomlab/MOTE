#' d for Between Subjects with Pooled SD Denominator
#'
#' This function displays d for between subjects data
#' and the non-central confidence interval using the
#' pooled standard deviation as the denominator.
#'
#' To calculate d, mean two is subtracted from mean one and divided
#' by the pooled standard deviation.
#'
#'      d_s = (m1 - m2) / spooled
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtm.html}{Learn more on our example page.}
#'
#' @param m1 mean group one
#' @param m2 mean group two
#' @param sd1 standard deviation group one
#' @param sd2 standard deviation group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return Provides the effect size (Cohen's d) with associated confidence intervals,
#' the t-statistic, the confidence intervals associated with the means of
#' each group, as well as the standard deviations and standard errors
#' of the means for each group.
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
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, independent t, between-subjects, pooled
#' standard deviation, pooled sd
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "indt_data" dataset, included
#' #in the MOTE library.
#'
#' #A forensic psychologist conducted a study to examine whether
#' #being hypnotized during recall affects how well a witness
#' #can remember facts about an event. Eight participants
#' #watched a short film of a mock robbery, after which
#' #each participant was questioned about what he or she had
#' #seen. The four participants in the experimental group
#' #were questioned while they were hypnotized. The four
#' #participants in the control group recieved the same
#' #questioning without hypnosis.
#'
#'     t.test(correctq ~ group, data = indt_data)
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' #as shown below.
#'
#'     d.ind.t(m1 = 17.75, m2 = 23, sd1 = 3.30,
#'            sd2 = 2.16, n1 = 4, n2 = 4, a = .05)
#'
#'     d.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     d.ind.t(mean(indt_data$correctq[indt_data$group == 1]),
#'             mean(indt_data$correctq[indt_data$group == 2]),
#'             sd(indt_data$correctq[indt_data$group == 1]),
#'             sd(indt_data$correctq[indt_data$group == 2]),
#'             length(indt_data$correctq[indt_data$group == 1]),
#'             length(indt_data$correctq[indt_data$group == 2]),
#'             .05)
#'
#' #Contrary to the hypothesized result, the group that underwent hypnosis were
#' #significantly less accurate while reporting facts than the control group
#' #with a large effect size, t(6) = -2.66, p = .038, d_s = 1.88.
#'

d.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05) {

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

  spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- (m1 - m2) / spooled
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

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

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
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p,
                "estimate" = paste("$d_s$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$t$(", (n1 - 1 + n2 - 1), ") = ", apa(t,2,T), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

#' @rdname d.ind.t
#' @export
