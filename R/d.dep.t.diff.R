#' d for Dependent t with SD Difference Scores Denominator
#'
#' This function displays d and the non-central confidence interval
#' for repeated measures data, using the standard deviation
#' of the difference score as the denominator.
#'
#' To calculate d, mean two is subtracted from mean one, which is
#' divided by the standard deviation of the difference scores.
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdifft.html}{Learn more on our example page.}
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
#'
#' @keywords effect size, dependent t-test
#' @export
#' @examples
#'
#' #The following example is derived from the "dept_data" dataset included
#' in the MOTE library.
#'
#' #In a study to test the effects of science fiction movies on people's
#' belief in the supernatural, seven people completed a measure of belief
#' in the supernatural before and after watching a popular science fiction movie.
#' Higher scores indicated higher levels of belief. The mean difference score was 1.14,
#' while the standard deviation of the difference scores was 2.12.
#'
#' #You can type in the numbers directly as shown below,
#' or refer to your dataset within the function.
#'
#'     d.dep.t.diff(mdiff = 1.143, sddiff = 2.116, n = 7, a = .05)
#'
#'     d.dep.t.diff(1.143, 2.116, 7, .05)
#'

d.dep.t.diff <- function (mdiff, sddiff, n, a = .05) {

  library(MBESS)

  d <- mdiff / sddiff
  se <- sddiff / sqrt(n)
  t <- mdiff / se
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  Mlow <- mdiff - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- mdiff + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2

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
                "p" = p
                )

  return(output)
}

#' @rdname d.dep.t.diff
#' @export
