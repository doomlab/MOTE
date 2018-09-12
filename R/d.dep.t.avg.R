#' d for Dependent t with Average SD Denominator
#'
#' This function displays d for repeated measures data
#' and the non-central confidence interval using the
#' average standard deviation of each level as the denominator.
#'
#' @details
#'
#' To calculate d, mean two is subtracted from mean one, which is
#' divided by the average standard deviation.
#'
#' Provided values are:
#' \itemize{
#'   \item d: the effect size
#'   \item dlow: the lower level confidence interval d value
#'   \item dhigh: the upper level confidence interval d value
#'   \item M1/M2: mean one and two
#'   \item M1low/M2low: the lower level confidence interval of mean one or two
#'   \item M1high/M2high: the upper level confidence interval of mean one or two
#'   \item sd1/sd2: the standard deviation of mean one and two
#'   \item se1/se2: the standard error of mean one and two
#'   \item n: the sample size
#'   \item df: the degrees of freedom (sample size - 1)
#'   }
#'
#' @param m1 mean from first level
#' @param m2 mean from second level
#' @param sd1 standard deviation from first level
#' @param sd2 standard deviation from second level
#' @param n sample size
#' @param a significance level
#'
#' @keywords effect size, dependent t-test, cohen's d
#' @export
#' @examples
#'
#' #The following example is derived from the "dept_data" dataset included
#' in the MOTE library.
#'
#' #In a study to test the effects of science fiction movies on people's
#' belief in the supernatural, seven people completed a measure of belief
#' in the supernatural before and after watching a popular science fiction
#' movie. Higher scores indicated higher levels of belief.
#'
#'     t.test(dept_data$before, dept_data$after, paired = TRUE)
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' as shown below.
#'
#'     d.dep.t.avg(m1 = 5.571, m2 = 4.429, sd1 = 1.988,
#'                 sd2 = 2.878, n = 14, a = .05)
#'
#'     d.dep.t.avg(5.571, 4.429, 1.988, 2.878, 14, .05)
#'
#'     d.dep.t.avg(mean(dept_data$before), mean(dept_data$after),
#'                 sd(dept_data$before), sd(dept_data$after),
#'                 length(dept_data$before), .05)
#'
#' #The mean measure of belief on the pretest (dept_data$before)
#' was 5.571, with a standard deviation of 1.988. The posttest
#' (dept_data$after) scores appeared lower but did not reach significance,
#' likely due to the small sample size (M = 4.429, SD = 2.878, p = .20).
#' The effect size was moderate (d = .47)suggesting the movie may
#' have influenced belief in the supernatural.

d.dep.t.avg <- function (m1, m2, sd1, sd2, n, a = .05) {

  library(MBESS)

  if (missing(m1) || missing(m2) || missing(sd1) || missing(sd2) || missing(n)) {
    stop("Be sure you enter mean 1, mean 2, sd 1, sd 2, and n values.")
    }

  d <- (m1 - m2) / ((sd1 + sd2) / 2)
  se1 <- sd1 / sqrt(n)
  se2 <- sd2 / sqrt(n)
  t <- (m1 - m2) / ((se1 + se2) / 2)
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
                "n" = n, #sample stats
                "df" = (n - 1)) #no t/p as not appropriate for sig testing

  return(output)
}

#' @rdname d.dep.t.avg
#' @export
