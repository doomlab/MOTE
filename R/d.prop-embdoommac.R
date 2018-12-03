#' d for Independent Proportions
#'
#' This function displays d and central confidence interval
#' calculated from differences in independent proportions.
#' Independent proportions are two percentages that are from
#' different groups of participants.
#'
#' To calculate z, the proportion of group two is substracted
#' from group one, which is then divided by the standard error.
#'
#'      z = (p1 - p2) / se
#'
#' To calculate d, the proportion of group two is divided by
#' the standard error of group two which is then subtracted
#' from the proportion of group one divided by the standard
#' error of group one.
#'
#'     z1 = p1 / se1
#'
#'     z2 = p2 / se2
#'
#'      d = z1 - z2
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtprop.html}{Learn more on our example page.}
#'
#' @param p1 proportion for group one
#' @param p2 proportion for group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{p1}{proportion of group one}
#' \item{se1}{standard error of the proportion of group one}
#' \item{z1}{z-statistic group one}
#' \item{z1low}{lower level confidence interval of z}
#' \item{z1high}{upper level confidence interval of z}
#' \item{p2}{proportion of group two}
#' \item{se2}{standard error of the proportion of group two}
#' \item{z2}{z-statistic of group two}
#' \item{z2low}{lower level confidence interval of z}
#' \item{z2high}{upper level confidence interval of z}
#' \item{n1}{sample size group one}
#' \item{n2}{sample size group two}
#' \item{z}{z-statistic for the differences}
#' \item{ppooled}{pooled proportion to calculate standard error}
#' \item{se}{standard error}
#' \item{p}{p-value for the differences}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, prop test, proportions, independent proportions
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #Several researchers were examining the data on the number
#' #of students who retake a course after they receive a D, F,
#' #or withdraw from the course. They randomly sampled form
#' #a large university two groups of students: traditional
#' #(less than 25 years old) and non-traditional (25 and older).
#' #Each group included 100 participants. About 25% of students
#' #of the traditional group reported they would retake a course,
#' #while the non-traditional group showed about 35% would
#' #retake the course.
#'
#' #You can type in the numbers directly as shown below,
#' #or refer to your dataset within the function.
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
  p <- pnorm(abs(z), lower.tail = F) * 2
  se1 <- sqrt((p1 * (1 - p1) / n1))
  se2 <- sqrt((p2 * (1 - p2) / n2))
  z1 <- p1 / se1
  z2 <- p2 / se2
  z1low <- z1 - qnorm(a / 2, lower.tail = F) * se1
  z1high <- z1 + qnorm(a / 2, lower.tail = F) * se1
  z2low <- z2 - qnorm(a / 2, lower.tail = F) * se2
  z2high <- z2 + qnorm(a / 2, lower.tail = F) * se2
  d <- z1 - z2
  dlow <- d - qnorm(a / 2, lower.tail = F) * se
  dhigh <- d + qnorm(a / 2, lower.tail = F) * se

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

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
                "estimate" = paste("$d_prop$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$Z$", " = ", apa(z,2,T), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

#' @rdname d.prop
#' @export
