#' Cohen's d for Z-test from Population Mean and SD
#'
#' Computes Cohen's d for a Z-test using the sample mean, population mean, and population standard deviation.
#' The function also provides a normal-theory confidence interval for d, and returns relevant statistics including the z-statistic and its p-value.
#'
#' @details
#' The effect size is computed as:
#' \deqn{d = \frac{m_1 - \mu}{\sigma}}
#' where \eqn{m_1} is the sample mean, \eqn{\mu} is the population mean,
#' and \eqn{\sigma} is the population standard deviation.
#'
#' The z-statistic is:
#' \deqn{z = \frac{m_1 - \mu}{\sigma / \sqrt{n}}}
#' where \eqn{n} is the sample size.
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/zm.html}{Learn more on our example page.}
#'
#' @param mu The population mean.
#' @param m1 The sample study mean.
#' @param sig The population standard deviation.
#' @param sd1 The standard deviation from the study.
#' @param n The sample size.
#' @param a The significance level.
#' @return A list with the following components:
#' \describe{
#'   \item{d}{Effect size (Cohen's d).}
#'   \item{dlow}{Lower level confidence interval d value.}
#'   \item{dhigh}{Upper level confidence interval d value.}
#'   \item{M1}{Mean of sample.}
#'   \item{sd1}{Standard deviation of sample.}
#'   \item{se1}{Standard error of sample.}
#'   \item{M1low}{Lower level confidence interval of the mean.}
#'   \item{M1high}{Upper level confidence interval of the mean.}
#'   \item{Mu}{Population mean.}
#'   \item{Sigma}{Standard deviation of population.}
#'   \item{se2}{Standard error of population.}
#'   \item{z}{Z-statistic.}
#'   \item{p}{P-value.}
#'   \item{n}{Sample size.}
#'   \item{estimate}{The d statistic and confidence interval in APA style for markdown printing.}
#'   \item{statistic}{The Z-statistic in APA style for markdown printing.}
#' }
#'
#' @keywords effect size, z-test
#' @import stats
#' @export
#' @examples
#'
#' # The average quiz test taking time for a 10 item test is 22.5
#' # minutes, with a standard deviation of 10 minutes. My class of
#' # 25 students took 19 minutes on the test with a standard deviation of 5.
#'
#' d.z.mean(mu = 22.5, m1 = 19, sig = 10, sd1 = 5, n = 25, a = .05)


d.z.mean <- function (mu, m1, sig, sd1, n, a = .05) {

  if (missing(m1)){
    stop("Be sure to include m1 for the sample mean.")
  }

  if (missing(mu)){
    stop("Be sure to include mu for the population mean.")
  }

  if (missing(sig)){
    stop("Be sure to include sig for the population standard deviation.")
  }

  if (missing(sd1)){
    stop("Be sure to include sd1 for the sample standard deviation")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- (m1 - mu) / sig
  se1 <- sig / sqrt(n)
  se2 <- sd1 / sqrt(n)
  dlow <- d-qnorm(a/2, lower.tail = FALSE)*sig
  dhigh <- d+qnorm(a/2, lower.tail = FALSE)*sig
  z <- (m1 - mu) / se1
  p <- pnorm(abs(z), lower.tail = FALSE)*2
  M1low <- m1 - se2 * qnorm(a/2, lower.tail = FALSE)
  M1high <- m1 + se2 * qnorm(a/2, lower.tail = FALSE)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "M1" = m1, #level 1 stats
                "sd1" = sd1,
                "se1" = se2,
                "M1low" = M1low,
                "M1high" = M1high,
                "Mu" = mu,#population stats
                "Sigma" = sig,
                "se2" = se1,
                "z" = z,
                "p" = p,
                "n" = n, #sample stats
                "estimate" = paste("$d$ = ", apa(d,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,TRUE), ", ", apa(dhigh,2,TRUE), "]", sep = ""),
                "statistic" = paste("$Z$", " = ", apa(z,2,TRUE), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

