#' Cohen's d for Paired t Using the SD of Difference Scores
#'
#' Compute Cohen's \eqn{d_z} and a noncentral-t confidence interval for
#' repeated-measures (paired-samples) designs using the **standard deviation
#' of the difference scores** as the denominator.
#'
#' @details
#' The effect size is defined as:
#' \deqn{d_z = \frac{\bar{X}_D}{s_D}}
#' where \eqn{\bar{X}_D} is the mean of the difference scores and \eqn{s_D} is
#' the standard deviation of the difference scores.
#'
#' The corresponding t statistic for the paired-samples t-test is:
#' \deqn{t = \frac{\bar{X}_D}{s_D / \sqrt{n}}}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/deptdiffm.html}{Learn more on our example page.}
#'
#' @param mdiff Mean of the difference scores.
#' @param sddiff Standard deviation of the difference scores.
#' @param n Sample size (number of paired observations).
#' @param a Significance level (alpha) for the confidence interval. Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_z}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_z}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_z}.}
#'   \item{mdiff}{Mean difference score.}
#'   \item{Mlow, Mhigh}{Confidence interval bounds for the mean difference.}
#'   \item{sddiff}{Standard deviation of the difference scores.}
#'   \item{se}{Standard error of the difference scores.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{t}{t-statistic.}
#'   \item{p}{p-value.}
#'   \item{estimate}{APA-style formatted string for reporting \eqn{d_z} and its CI.}
#'   \item{statistic}{APA-style formatted string for reporting the t-statistic and p-value.}
#' }
#'
#' @keywords effect size, dependent t-test, cohen's d, repeated measures
#' @import stats
#' @export
#'
#' @examples
#' # Example derived from the "dept_data" dataset included in MOTE
#'
#' # Suppose seven people completed a measure of belief in the supernatural
#' # before and after watching a sci-fi movie. Higher scores indicate stronger belief.
#'
#' t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' # Direct entry of summary statistics:
#' d.dep.t.diff(mdiff = 1.14, sddiff = 2.12, n = 7, a = .05)
#'
#' # Equivalent shorthand:
#' d.dep.t.diff(1.14, 2.12, 7, .05)
#'
#' # Using raw data from the dataset:
#' d.dep.t.diff(mdiff = mean(dept_data$before - dept_data$after),
#'              sddiff = sd(dept_data$before - dept_data$after),
#'              n = length(dept_data$before),
#'              a = .05)

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
  ncpboth <- noncentral_t(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  Mlow <- mdiff - se * qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh <- mdiff + se * qt(a / 2, n - 1, lower.tail = FALSE)
  p <- pt(abs(t), n - 1, lower.tail = F) * 2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

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
                "estimate" = paste("$d_z$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$t$(", (n-1), ") = ", apa(t,2,TRUE), ", $p$ ", reportp, sep = "")
                )

  return(output)
}
