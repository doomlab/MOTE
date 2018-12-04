#' d from t for Between Subjects
#'
#' This function displays d for between subjects data
#' and the non-central confidence interval estimating from the t-statistic.
#'
#' To calculate d, the t-statistic is multiplied by two then divided by
#' the square root of the degrees of freedom.
#'
#'      d_s = 2 * t / sqrt(n1 + n2 - 2)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtt.html}{Learn more on our example page.}
#'
#' @param t t-test value
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return Provides the effect size (Cohen's d) with associated confidence intervals,
#' degrees of freedom, t-statistic, and p-value.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval of d value}
#' \item{dhigh}{upper level confidence interval of d value}
#' \item{n1}{sample size}
#' \item{n2}{sample size}
#' \item{df}{degrees of freedom (n1 - 1 + n2 - 1)}
#' \item{t}{t-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA for the t-test}
#'
#' @keywords effect size, independent t
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
#'     hyp = t.test(correctq ~ group, data = indt_data)
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' #as shown below.
#'
#'     d.ind.t.t(t = -2.6599, n1 = 4, n2 = 4, a = .05)
#'
#'     d.ind.t.t(-2.6599, 4, 4, .05)
#'
#'     d.ind.t.t(hyp$statistic,
#'               length(indt_data$group[indt_data$group == 1]),
#'               length(indt_data$group[indt_data$group == 2]),
#'               .05)
#'
#' #Contrary to the hypothesized result, the group that underwent hypnosis were
#' #significantly less accurate while reporting facts than the control group
#' #with a large effect size, t(6) = -2.66, p = .038, d_s = 2.17.
#'

d.ind.t.t <- function (t, n1, n2, a = .05) {

  if (missing(t)){
    stop("Be sure to include the t-value found from your t-test.")
  }

  if (missing(n1)){
    stop("Be sure to include the sample size n1 for group 1.")
  }

  if (missing(n2)){
    stop("Be sure to include the sample size n2 for group 2.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }


  d <- 2 * t / sqrt(n1 + n2 - 2)
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  dhigh <- ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2)))
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p,
                "estimate" = paste("$d_s$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$t$ = (", (n1-1+n2-1), ") = ",
                                    apa(t,2,T), ", $p$ ", reportp, sep = "")
                )

  return(output)
}

#' @rdname d.ind.t.t
#' @export

