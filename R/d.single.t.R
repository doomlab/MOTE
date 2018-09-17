#' d for Single t from Means
#'
#' This function displays d and non-central confidence interval for single t from means.
#'
#' To calculate d, the population is subtracted from the sample mean,
#' which is then divided by the standard deviation.
#' \href{Learn more on our example page.}{https://www.aggieerin.com/shiny-server/tests/singlett.html}
#'
#' @param m sample mean
#' @param u population mean
#' @param sd sample standard deviation
#' @param n sample size
#' @param a significance level
#' @return
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{m}{sample mean}
#' \item{sd}{standard deviation of the sample}
#' \item{se}{standard error of the sample}
#' \item{Mlow}{lower level confidence interval of the sample mean}
#' \item{Mhigh}{upper level confidence interval of the sample mean}
#' \item{u}{population mean}
#' \item{n}{sample size}
#' \item{df}{degrees of freedom (n - 1)}
#' \item{t}{t-statistic}
#' \item{p}{p-value}
#'
#' @keywords effect size, single t, single-sample, mu, u, population mean, sample mean
#' @export
#' @examples
#'
#' #A school has a gifted/honors program that they claim is
#' significantly better than others in the country. The gifted/honors
#' students in this school scored an average of 1370 on the SAT,
#' with a standard deviation of 112.7, while the national average
#' for gifted programs is a SAT score of 1080.
#'
#' You can type in the numbers directly as shown below,
#' or refer to your dataset within the function.
#'
#'     d.single.t(m = 1370, u = 1080, sd = 112.7, n = 100, a = .05)
#'
#'     d.single.t(1370, 1080, 112.7, 100, .05)
#'

d.single.t = function (m, u, sd, n, a = .05) {
  # This function displays d and non-central confidence interval for single t from means.
  #
  # Args:
  #   m : sample mean
  #   u : population mean
  #   sd: sample standard deviation
  #   n : sample size
  #   a : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics

  library(MBESS)

  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ncpboth = conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = m - se*qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh = m + se*qt(a / 2, n - 1, lower.tail = FALSE)
  p = pt(abs(t), n - 1, lower.tail = F)*2

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "m" = m, #mean stats
                "sd" = sd,
                "se" = se,
                "Mlow" = Mlow,
                "Mhigh" = Mhigh,
                "u" = u,
                "n" = n, #sample stats
                "df" = (n - 1),
                "t" = t, #sig stats,
                "p" = p)

  return(output)
}
