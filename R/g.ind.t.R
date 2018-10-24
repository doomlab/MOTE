#' d-g Corrected for Independent t
#'
#' This function displays d-g corrected
#' and the non-central confidence interval for independent t.
#'
#' The correction is calculated by dividing three by the sum of both
#' sample sizes after multiplying by four and subtracting nine.
#' This amount is deducted from one.
#'
#'      correction = 1 - (3 / (4 * (n1 + n2) - 9))
#'
#' D-g corrected is calculated by substracting mean two from mean one,
#' dividing by the pooled standard deviation which is multiplied
#' by the correction above.
#'
#'      d-g corrected = ((m1 - m2) / spooled) * correction
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/indtg.html}{Learn more on our example page.}
#'
#' @param m1 mean group one
#' @param m2 mean group two
#' @param sd1 standard deviation group one
#' @param sd2 standard deviation group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return D-g corrected with associated confidence intervals,
#' the confidence intervals associated with the means of each group,
#' standard deviations of the means for each group, relevant statistics.
#'
#' \item{d}{d-g corrected effect size}
#' \item{dlow}{lower level confidence interval d-g corrected}
#' \item{dhigh}{upper level confidence interval d-g corrected}
#' \item{M1}{mean group one}
#' \item{sd1}{standard deviation of group one}
#' \item{se1}{standard error of group one}
#' \item{M1low}{lower level confidence interval of mean one}
#' \item{M1high}{upper level confidence interval of mean one}
#' \item{M2}{mean two}
#' \item{sd2}{standard deviation of mean two}
#' \item{se1}{standard error of mean two}
#' \item{M2low}{lower level confidence interval of mean two}
#' \item{M2high}{upper level confidence interval of mean two}
#' \item{spooled}{pooled standard deviation}
#' \item{sepooled}{pooled standard error}
#' \item{correction}{g corrected}
#' \item{n1}{size of sample one}
#' \item{n2}{size of sample two}
#' \item{df}{degrees of freedom}
#' \item{t}{t-statistic}
#' \item{p}{p-value}

#' @keywords effect size, independent t, correction
#' @export
#' @examples
#' g.ind.t(m1 = 20, m2 = 17, sd1 = 4, sd2 = 5, n1 = 100, n2 = 100, a = .05)


g.ind.t <- function (m1, m2, sd1, sd2, n1, n2, a = .05) {
  # This function displays d-g corrected
  # and the non-central confidence interval for independent t.
  #
  # Args:
  #   m1 : mean group one
  #   m2 : mean group two
  #   sd1: standard deviation group one
  #   sd2: standard deviation group two
  #   n1 : sample size group one
  #   n2 : sample size group two
  #   a  : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics

  correction <- 1 - (3 / (4 * (n1 + n2) - 9))
  spooled <- sqrt( ((n1 - 1) * sd1 ^ 2 + (n2 - 1) * sd2 ^ 2) / (n1 + n2 - 2))
  d <- ((m1 - m2) / spooled) * correction
  se1 <- sd1 / sqrt(n1)
  se2 <- sd2 / sqrt(n2)
  sepooled <- sqrt((spooled ^ 2 / n1 + spooled ^ 2 / n2))
  t <- (m1 - m2) / sepooled
  ncpboth <- conf.limits.nct(t, (n1 - 1 + n2 - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow <- correction * (ncpboth$Lower.Limit / sqrt(((n1 * n2) / (n1 + n2))))
  dhigh <- correction * (ncpboth$Upper.Limit / sqrt(((n1 * n2) / (n1 + n2))))
  M1low <- m1 - se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M1high <- m1 + se1 * qt(a / 2, n1 - 1, lower.tail = FALSE)
  M2low <- m2 - se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  M2high <- m2 + se2 * qt(a / 2, n2 - 1, lower.tail = FALSE)
  p <- pt(abs(t), (n1 - 1 + n2 - 1), lower.tail = F) * 2

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "M1" = m1, #group 1 stats
                "sd1" = sd1,
                "se1" = se1,
                "M1low" = M1low,
                "M1high" = M1high,
                "M2" = m2, #group 2 stats
                "sd2" = sd2,
                "se2" = se2,
                "M2low" = M2low,
                "M2high" = M2high,
                "spooled" = spooled,
                "sepooled" = sepooled,
                "correction" = correction,
                "n1" = n1, #sample stats
                "n2" = n2,
                "df" = (n1 - 1 + n2 - 1),
                "t" = t, #sig stats,
                "p" = p)

  return(output)
}
