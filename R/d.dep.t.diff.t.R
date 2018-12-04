#' d from t for Repeated Measures with SD Difference Scores Denominator
#'
#' This function displays d for repeated measures data
#' and the non-central confidence interval using the
#' standard deviation of the differences as the denominator
#' estimating from the t-statistic.
#'
#' To calculate d, the t-statistic is divided by the square root of the sample size.
#'
#'      d_z = t / sqrt(n)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdifft.html}{Learn more on our example page.}
#'
#' @param t t-test value
#' @param n sample size
#' @param a significance level
#' @return
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{n}{sample size}
#' \item{df}{degrees of freedom (sample size - 1)}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in APA
#' style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, dependent t-test, paired sample,
#' repeated measures, t-test
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "dept_data" dataset included
#' #in the MOTE library.
#'
#' #In a study to test the effects of science fiction movies on people’s belief
#' #in the supernatural, seven people completed a measure of belief in
#' #the supernatural before and after watching a popular science
#' #fiction movie. Higher scores indicated higher levels of belief.
#'
#'     scifi = t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' #The t-test value was 1.43. You can type in the numbers directly,
#' #or refer to the dataset, as shown below.
#'
#'     d.dep.t.diff.t(t = 1.43, n = 7, a = .05)
#'
#'     d.dep.t.diff.t(1.43, 7, .05)
#'
#'     d.dep.t.diff.t(scifi$statistic, length(dept_data$before), .05)
#'
#' #The mean measure of belief on the pretest was 5.57, with a standard
#' #deviation of 1.99. The posttest scores appeared lower (M = 4.43, SD = 2.88)
#' #but the dependent t-test was not significant using alpha = .05,
#' #t(7) = 1.43, p = .203, d_z = 0.54. The effect size was a medium effect suggesting
#' #that the movie may have influenced belief in the supernatural.

d.dep.t.diff.t <- function (t, n, a = .05) {

  if (missing(t)){
    stop("Be sure to include your t-value from your dependent t-test.")
  }

  if (missing(n)){
    stop("Be sure to include your sample size value n.")
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
                "estimate" = paste("$d_z$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$t$(", (n-1), ") = ", apa(t,2,T), ", $p$ ", reportp, sep = "")
                )

  return(output)
}

#' @rdname d.dep.t.diff.t
#' @export
