#' d for Dependent t with SD Difference Scores Denominator
#'
#' This function displays d and the non-central confidence interval
#' for repeated measures data, using the standard deviation
#' of the difference score as the denominator.
#'
#' To calculate d, the mean difference score is divided by
#' divided by the standard deviation of the difference scores.
#'
#'      d_z = mdiff / sddiff
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdiffm.html}{Learn more on our example page.}
#'
#' @param mdiff mean difference score
#' @param sddiff standard deviation of the difference scores
#' @param n sample size
#' @param a significance level
#' @return The effect size (Cohen's d) with associated confidence intervals, mean
#' differences with associated confidence intervals, standard
#' deviation of the differences, standard error, sample size, degrees of
#' freedom, the t-statistic, and the p-value.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{mdiff}{mean difference score}
#' \item{Mlow}{lower level of confidence interval of the mean}
#' \item{Mhigh}{upper level of confidence interval of the mean}
#' \item{sddiff}{standard deviation of the difference scores}
#' \item{n}{sample size}
#' \item{df}{degrees of freedom (sample size - 1)}
#' \item{t}{t-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, dependent t-test
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "dept_data" dataset included
#' #in the MOTE library.
#'
#' #In a study to test the effects of science fiction movies on people's
#' #belief in the supernatural, seven people completed a measure of belief
#' #in the supernatural before and after watching a popular science fiction movie.
#' #Higher scores indicated higher levels of belief. The mean difference score was 1.14,
#' #while the standard deviation of the difference scores was 2.12.
#'
#' #You can type in the numbers directly as shown below,
#' #or refer to your dataset within the function.
#'
#'     d.dep.t.diff(mdiff = 1.14, sddiff = 2.12, n = 7, a = .05)
#'
#'     d.dep.t.diff(1.14, 2.12, 7, .05)
#'
#'     d.dep.t.diff(mdiff = mean(dept_data$before - dept_data$after),
#'                  sddiff = sd(dept_data$before - dept_data$after),
#'                  n = length(dept_data$before),
#'                  a = .05)
#'
#' #The mean measure of belief on the pretest was 5.57, with a standard
#' #deviation of 1.99. The posttest scores appeared lower (M = 4.43, SD = 2.88)
#' #but the dependent t-test was not significant using alpha = .05,
#' #t(7) = 1.43, p = .203, d_z = 0.54. The effect size was a medium
#' #effect suggesting that the movie may have influenced belief
#' #in the supernatural.
#'

d.dep.t.diff <- function (mdiff, sddiff, n, a = .05) {

  if (missing(mdiff)) {
    stop("Be sure to include the mean difference score mdiff.")
  }

  if (missing(sddiff)){
    stop("Be sure to include the standard deviation of the difference scores sddiff.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- mdiff / sddiff
  se <- sddiff / sqrt(n)
  t <- mdiff / se
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  Mlow <- mdiff - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- mdiff + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "mdiff" = mdiff, #mean stats
                "Mlow" = Mlow,
                "Mhigh" = Mhigh,
                "sddiff" = sddiff,
                "se" = se,
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

#' @rdname d.dep.t.diff
#' @export
