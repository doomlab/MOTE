#' d for Single t from t
#'
#' This function displays d and non-central confidence interval for single t
#' estimated from the t-statistic.
#'
#' To calculate d, the t-statistic is divided by the square root of the sample size.
#'
#'      d = t / sqrt(n)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/singlett.html}{Learn more on our example page.}
#'
#'
#' @param t t-test value
#' @param n sample size
#' @param a significance level
#' @return The effect size (Cohen's d) with associated confidence intervals
#' and relevant statistics.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{n}{sample size}
#' \item{df}{degrees of freedom (sample size - 1)}
#' \item{t}{sig stats}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, single t
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #A school has a gifted/honors program that they claim is
#' #significantly better than others in the country. The gifted/honors
#' #students in this school scored an average of 1370 on the SAT,
#' #with a standard deviation of 112.7, while the national average
#' #for gifted programs is a SAT score of 1080.
#'
#'     gift = t.test(singt_data, mu = 1080, alternative = "two.sided")
#'
#' #According to a single-sample t-test, the scores of the students
#' #from the program were significantly higher than the national
#' #average, t(14) = 9.97, p < .001.
#'
#' #You can type in the numbers directly as shown below, or refer
#' #to your dataset within the function.
#'
#'     d.single.t.t(t = 9.968, n = 15, a = .05)
#'
#'     d.single.t.t(9.968, 15, .05)
#'
#'     d.single.t.t(gift$statistic, length(singt_data$SATscore), .05)


d.single.t.t <- function (t, n, a = .05) {

  if (missing(t)){
    stop("Be sure to include t from the t-test statistic.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- t / sqrt(n)
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats
                "p" = p,
                "estimate" = paste("$d$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$t$(", (n - 1), ") = ", apa(t,2,T), ", $p$ ",
                                    reportp, sep = "")
                )

  return(output)

}

#' @rdname d.single.t.t
#' @export
