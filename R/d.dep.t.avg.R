#' Cohen's d for Paired t Using the Average SD Denominator
#'
#' Compute Cohen's \eqn{d_{av}} and a noncentral-t confidence interval for
#' repeated-measures (paired-samples) designs using the **average of the two
#' standard deviations** as the denominator.
#'
#' @details
#' The effect size is defined as the mean difference divided by the average SD:
#' \deqn{d_{av} = \frac{m_1 - m_2}{\left( s_1 + s_2 \right)/2}.}
#'
#' The test statistic used for the noncentral-t confidence interval is based on
#' the average of the two standard errors, \eqn{se_i = s_i/\sqrt{n}}:
#' \deqn{t = \frac{m_1 - m_2}{\left( \frac{s_1}{\sqrt{n}} + \frac{s_2}{\sqrt{n}} \right) / 2}.}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/deptavgm.html}{Learn more on our example page.}
#'
#' @param m1 Mean from the first level/occasion.
#' @param m2 Mean from the second level/occasion.
#' @param sd1 Standard deviation from the first level/occasion.
#' @param sd2 Standard deviation from the second level/occasion.
#' @param n Sample size (number of paired observations).
#' @param a Significance level (alpha) for the confidence interval. Must be in (0, 1).
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Cohen's \eqn{d_{av}}.}
#'   \item{dlow}{Lower limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_{av}}.}
#'   \item{dhigh}{Upper limit of the \eqn{(1-\alpha)} confidence interval for \eqn{d_{av}}.}
#'   \item{M1, M2}{Group means.}
#'   \item{M1low, M1high, M2low, M2high}{Confidence interval bounds for each mean.}
#'   \item{sd1, sd2}{Standard deviations.}
#'   \item{se1, se2}{Standard errors of the means.}
#'   \item{n}{Sample size.}
#'   \item{df}{Degrees of freedom (\eqn{n - 1}).}
#'   \item{estimate}{APA-style formatted string for reporting \eqn{d_{av}} and its CI.}
#' }
#'
#' @keywords effect size, dependent t-test, cohen's d, d average, paired-sample, repeated measures
#' @import stats
#' @export
#'
#' @examples
#' # The following example is derived from the "dept_data" dataset included
#' # in the MOTE package.
#'
#' # Suppose seven people completed a measure of belief in the supernatural
#' # before and after watching a sci-fi movie. Higher scores indicate stronger belief.
#'
#'     t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' # You can type in the numbers directly, or refer to the dataset, as shown below.
#'
#'     d.dep.t.avg(m1 = 5.57, m2 = 4.43, sd1 = 1.99,
#'                 sd2 = 2.88, n = 7, a = .05)
#'
#'     d.dep.t.avg(5.57, 4.43, 1.99, 2.88, 7, .05)
#'
#'     d.dep.t.avg(mean(dept_data$before), mean(dept_data$after),
#'                 sd(dept_data$before), sd(dept_data$after),
#'                 length(dept_data$before), .05)

d.dep.t.avg <- function (m1, m2, sd1, sd2, n, a = .05) {

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

  if (missing(n)){
    stop("Be sure to include the sample size n.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- (m1 - m2) / ((sd1 + sd2) / 2)
  se1 <- sd1 / sqrt(n)
  se2 <- sd2 / sqrt(n)
  t <- (m1 - m2) / ((se1 + se2) / 2)
  ncpboth <- noncentral_t(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- ncpboth$Lower.Limit / sqrt(n)
  dhigh <- ncpboth$Upper.Limit / sqrt(n)
  M1low <- m1 - se1 * qt(a / 2, n - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n - 1, lower.tail = FALSE)
  M2high <- m2 + se2 *  qt(a / 2, n - 1, lower.tail = FALSE)

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "M1" = m1, #level 1 stats
                "sd1" = sd1,
                "se1" = se1,
                "M1low" = M1low,
                "M1high" = M1high,
                "M2" = m2, #level 2 stats
                "sd2" = sd2,
                "se2" = se2,
                "M2low" = M2low,
                "M2high" = M2high,
                "n" = n, #sample stats
                "df" = (n - 1),
                "estimate" = paste("$d_{av}$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = "")
                                   ) #no t/p as not appropriate for sig testing

  return(output)
}
