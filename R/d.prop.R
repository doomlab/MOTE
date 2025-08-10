#' Cohen's d for Independent Proportions
#'
#' Compute Cohen's \eqn{d_{prop}} for independent proportions and a central confidence interval.
#' Independent proportions are two percentages from different groups of participants.
#'
#' The z-statistic is:
#' \deqn{z = \frac{p_1 - p_2}{SE_{pooled}}}
#' where the pooled standard error is based on the pooled proportion:
#' \deqn{SE_{pooled} = \sqrt{\hat{p} (1 - \hat{p}) \left( \frac{1}{n_1} + \frac{1}{n_2} \right) }}
#' and \eqn{\hat{p} = \frac{p_1 n_1 + p_2 n_2}{n_1 + n_2}}.
#'
#' For each group, the z-statistic is computed as:
#' \deqn{z_1 = \frac{p_1}{SE_1}, \quad SE_1 = \sqrt{ \frac{p_1 (1 - p_1)}{n_1} }}
#' \deqn{z_2 = \frac{p_2}{SE_2}, \quad SE_2 = \sqrt{ \frac{p_2 (1 - p_2)}{n_2} }}
#'
#' Cohen's \eqn{d_{prop}} is then:
#' \deqn{d_{prop} = z_1 - z_2}
#'
#' See the online example for additional context:
#' \href{https://www.aggieerin.com/shiny-server/tests/indtprop.html}{Learn more on our example page.}
#'
#' @param p1 Proportion for group one.
#' @param p2 Proportion for group two.
#' @param n1 Sample size for group one.
#' @param n2 Sample size for group two.
#' @param a Significance level.
#' @return A list with the following elements:
#' \describe{
#'   \item{d}{Effect size.}
#'   \item{dlow}{Lower level confidence interval d value.}
#'   \item{dhigh}{Upper level confidence interval d value.}
#'   \item{p1}{Proportion of group one.}
#'   \item{se1}{Standard error of the proportion of group one.}
#'   \item{z1}{z-statistic for group one.}
#'   \item{z1low}{Lower level confidence interval of z for group one.}
#'   \item{z1high}{Upper level confidence interval of z for group one.}
#'   \item{p2}{Proportion of group two.}
#'   \item{se2}{Standard error of the proportion of group two.}
#'   \item{z2}{z-statistic for group two.}
#'   \item{z2low}{Lower level confidence interval of z for group two.}
#'   \item{z2high}{Upper level confidence interval of z for group two.}
#'   \item{n1}{Sample size for group one.}
#'   \item{n2}{Sample size for group two.}
#'   \item{z}{z-statistic for the difference.}
#'   \item{ppooled}{Pooled proportion used to calculate standard error.}
#'   \item{se}{Standard error.}
#'   \item{p}{p-value for the difference.}
#'   \item{estimate}{The d statistic and confidence interval in APA style for markdown printing.}
#'   \item{statistic}{The z-statistic in APA style for markdown printing.}
#' }
#'
#' @keywords effect size, prop test, proportions, independent proportions
#' @import stats
#' @export
#' @examples
#'
#' # Several researchers were examining the data on the number
#' # of students who retake a course after they receive a D, F,
#' # or withdraw from the course. They randomly sampled from
#' # a large university two groups of students: traditional
#' # (less than 25 years old) and non-traditional (25 and older).
#' # Each group included 100 participants. About 25% of students
#' # of the traditional group reported they would retake a course,
#' # while the non-traditional group showed about 35% would
#' # retake the course.
#'
#' # You can type in the numbers directly as shown below,
#' # or refer to your dataset within the function.
#'
#'     d.prop(p1 = .25, p2 = .35, n1 = 100, n2 = 100, a = .05)
#'
#'     d.prop(.25, .35, 100, 100, .05)


d.prop <- function (p1, p2, n1, n2, a = .05) {

  if (missing(p1)){
    stop("Be sure to include p1 for the first proportion.")
  }

  if (p1 > 1 | p2 > 1 | p1 < 0 | p2 < 0){
    stop("Be sure to enter your values as proportions,
         rather than percentages, values should be less than 1.
         Also make sure all proportion values are positive.")
  }

  if (missing(p2)){
    stop("Be sure to include p2 for the second proportion.")
  }

  if (missing(n1)){
    stop("Be sure to include the sample size n1 for the first group.")
  }

  if (missing(n2)){
    stop("Be sure to include the sample size n2 for the second group.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  ppooled <- (p1 * n1 + p2 * n2) / (n1 + n2)
  se <- sqrt(ppooled * (1 - ppooled) * ((1 / n1) + (1 / n2)))
  z <- (p1 - p2)/ se
  p <- pnorm(abs(z), lower.tail = FALSE) * 2
  se1 <- sqrt((p1 * (1 - p1) / n1))
  se2 <- sqrt((p2 * (1 - p2) / n2))
  z1 <- p1 / se1
  z2 <- p2 / se2
  z1low <- z1 - qnorm(a / 2, lower.tail = FALSE) * se1
  z1high <- z1 + qnorm(a / 2, lower.tail = FALSE) * se1
  z2low <- z2 - qnorm(a / 2, lower.tail = FALSE) * se2
  z2high <- z2 + qnorm(a / 2, lower.tail = FALSE) * se2
  d <- z1 - z2
  dlow <- d - qnorm(a / 2, lower.tail = FALSE) * se
  dhigh <- d + qnorm(a / 2, lower.tail = FALSE) * se

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "p1" = p1, #group 1 stats
                "se1" = se1,
                "z1" = z1,
                "z1low" = z1low,
                "z1high" = z1high,
                "p2" = p2, #group 2 stats
                "se2" = se2,
                "z2" = z2,
                "z2low" = z2low,
                "z2high" = z2high,
                "n1" = n1, #sample stats
                "n2" = n2,
                "z" = z, #sig stats,
                "ppooled" = ppooled,
                "se" = se,
                "p" = p,
                "estimate" = paste("$d_prop$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$Z$", " = ", apa(z,2,TRUE), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

