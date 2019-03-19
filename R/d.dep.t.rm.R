#' d for Repeated Measures with Average SD Denominator
#'
#' This function displays d and the non-central confidence interval
#' for repeated measures data, using the average standard deviation of
#' each level as the denominator, but controlling for r.
#'
#' To calculate d, mean two is subtracted from mean one, which is
#' divided by the average standard deviation, while mathematically
#' controlling for the correlation coefficient (r).
#'
#'      d_rm = ((m1 - m2) / sqrt(( sd1^2 + sd2^2 ) - (2 x r x sd1 x sd2))) x sqrt(2 x (1-r))
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/deptrm.html}{Learn more on our example page.}
#'
#' @param m1 mean from first level
#' @param m2 mean from second level
#' @param sd1 standard deviation from first level
#' @param sd2 standard deviation from second level
#' @param r correlation between first and second level
#' @param n sample size
#' @param a significance level
#' @return Controls for correlation and provides the effect size (Cohen's d)
#' with associated confidence intervals,m the confidence intervals associated
#' with the means of each group,mstandard deviations and standard errors of
#' the means for each group.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{M1}{mean one}
#' \item{sd1}{standard deviation of mean one}
#' \item{se1}{standard error of mean one}
#' \item{M1low}{lower level confidence interval of mean one}
#' \item{M1high}{upper level confidence interval of mean one}
#' \item{M2}{mean two}
#' \item{sd2}{standard deviation of mean two}
#' \item{se2}{standard error of mean two}
#' \item{M2low}{lower level confidence interval of mean two}
#' \item{M2high}{upper level confidence interval of mean two}
#' \item{r}{correlation}
#' \item{n}{sample size}
#' \item{df}{degrees of freedom (sample size - 1)}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#'
#' @keywords effect size, dependent t-test, cohen's d, paired-sample,
#' repeated measures, correlation
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
#' #in the supernatural before and after watching a popular science fiction
#' #movie. Higher scores indicated higher levels of belief.
#'
#'     t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#'     scifi_cor = cor(dept_data$before, dept_data$after, method = "pearson",
#'                 use = "pairwise.complete.obs")
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' #as shown below.
#'
#'     d.dep.t.rm(m1 = 5.57, m2 = 4.43, sd1 = 1.99,
#'                 sd2 = 2.88, r = .68, n = 7, a = .05)
#'
#'     d.dep.t.rm(5.57, 4.43, 1.99, 2.88, .68, 7, .05)
#'
#'     d.dep.t.rm(mean(dept_data$before), mean(dept_data$after),
#'                 sd(dept_data$before), sd(dept_data$after),
#'                 scifi_cor, length(dept_data$before), .05)
#'
#' #The mean measure of belief on the pretest was 5.57, with a standard
#' #deviation of 1.99. The posttest scores appeared lower (M = 4.43, SD = 2.88)
#' #but the dependent t-test was not significant using alpha = .05,
#' #t(7) = 1.43, p = .203, d_rm = 0.43. The effect size was a medium effect suggesting
#' #that the movie may have influenced belief in the supernatural.
#'

d.dep.t.rm <- function (m1, m2, sd1, sd2, r, n, a = .05) {

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

  if (missing(r)){
    stop("Be sure to include the correlation r between the two levels.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- ((m1 - m2) / sqrt((sd1^2+sd2^2) - (2*r*sd1*sd2))) * sqrt(2*(1-r))
  se1 <- sd1 / sqrt(n)
  se2 <- sd2 / sqrt(n)
  t <- ((m1 - m2) / (sqrt((sd1^2+sd2^2) - (2*r*sd1*sd2)))/sqrt(n)) * sqrt(2*(1-r))
  ncpboth <- conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
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
                "r" = r,
                "n" = n, #sample stats
                "df" = (n - 1),
                "estimate" = paste("$d_{rm}$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = "")
                ) #no t/p as not appropriate for sig testing

  return(output)
  }

#' @rdname d.dep.t.rm
#' @export
