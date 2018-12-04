#' d for Single t from Means
#'
#' This function displays d and non-central confidence interval for single t from means.
#'
#' To calculate d, the population is subtracted from the sample mean,
#' which is then divided by the standard deviation.
#'
#'      d = (m - u) / sd
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/singletm.html}{Learn more on our example page.}
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
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, single t, single-sample, mu, u, population mean, sample mean
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "singt_data" dataset included
#' #in the MOTE library.
#'
#' #A school has a gifted/honors program that they claim is
#' #significantly better than others in the country. The gifted/honors
#' #students in this school scored an average of 1370 on the SAT,
#' #with a standard deviation of 112.7, while the national average
#' #for gifted programs is a SAT score of 1080.
#'
#'     gift = t.test(singt_data, mu = 1080, alternative = "two.sided")
#'
#' #You can type in the numbers directly as shown below,
#' #or refer to your dataset within the function.
#'
#'     d.single.t(m = 1370, u = 1080, sd = 112.7, n = 14, a = .05)
#'
#'     d.single.t(1370, 1080, 112.7, 100, .05)
#'
#'     d.single.t(gift$estimate, gift$null.value,
#'             sd(singt_data$SATscore),
#'         length(singt_data$SATscore), .05)

d.single.t = function (m, u, sd, n, a = .05) {

  if (missing(m)){
    stop("Be sure to include m for the sample mean.")
  }

  if (missing(u)){
    stop("Be sure to include me for the population.")
  }

  if (missing(sd)){
    stop("Be sure to include sd for the sample mean.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  se <- sd / sqrt(n)
  d <- (m - u) / sd
  t <- (m - u) / se
  ncpboth = conf.limits.nct(t, (n - 1), conf.level = (1 - a), sup.int.warns = TRUE)
  dlow = ncpboth$Lower.Limit / sqrt(n)
  dhigh = ncpboth$Upper.Limit / sqrt(n)
  Mlow = m - se*qt(a / 2, n - 1, lower.tail = FALSE)
  Mhigh = m + se*qt(a / 2, n - 1, lower.tail = FALSE)
  p = pt(abs(t), n - 1, lower.tail = F)*2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

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
                "p" = p,
                "estimate" = paste("$d$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(dlow,2,T), ", ", apa(dhigh,2,T), "]", sep = ""),
                "statistic" = paste("$t$(", (n-1), ") = ", apa(t,2,T), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

#' @rdname d.single.t
#' @export
