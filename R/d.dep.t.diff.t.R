#' d from t for Repeated Measures with SD Difference Scores Denominator
#'
#' This function displays d for repeated measures data
#' and the non-central confidence interval using the
#' standard deviation of the differences as the denominator
#' estimating from the t-statistic.
#'
#'  To calculate d, the t-statistic is divided by the square root of the sample size.
#' \href{Learn more on our example page.}{https://www.aggieerin.com/shiny-server/tests/deptdifft.html}
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
#'
#' @keywords effect size, dependent t-test, paired sample, repeated measures, t-test
#' @export
#' @examples
#'
#' #The following example is derived from the "dept_data" dataset included
#' in the MOTE library.
#'
#' #In a study to test the effects of science fiction movies on people’s belief
#' in the supernatural, seven people completed a measure of belief in
#' the supernatural before and after watching a popular science
#' fiction movie. Higher scores indicated higher levels of belief.
#'
#'     scifi = t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' #The t-test value was 1.4292. You can type in the numbers directly,
#' or refer to the dataset, as shown below.
#'
#'     d.dep.t.diff.t(1.429179, n = 7, a = .05)
#'
#'     d.dep.t.diff.t(1.429179, 7, .05)
#'
#'     d.dep.t.diff.t(scifi$statistic, length(dept_data$before), .05)
#'
#' #The mean measure of belief on the pretest (dept_data$before)
#' was 5.57, with a standard deviation of 1.99. The posttest
#' (dept_data$after) scores appeared lower (M = 4.43, SD = 2.88) but did not reach
#' significance, (t(7) = 1.429, p = .20, d = .54), likely due to the small sample size.
#' The effect size was moderate (d = 0.54) suggesting the movie may
#' have influenced belief in the supernatural.

d.dep.t.diff.t <- function (t, n, a = .05) {
  # Displays d and non-central confidence interval for repeated measures
  # using the standard deviation of the differences as the denominator
  # estimating from the t-statistic.
  #
  # Args:
  #   t : t-test value
  #   n : sample size
  #   a  : significance level
  #
  # Returns:
  #   List of d and sample size statistics

  library(MBESS)

  d <- t / sqrt(n)
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats
                "p" = p
                )

  return(output)
}
