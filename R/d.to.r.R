#' r and Coefficient of Determination (R2) from d
#'
#' Calculates r from d and then translates r to r2 to calculate
#' the non-central confidence interval for r2 using the F distribution.
#'
#' The correlation coefficient (r) is calculated by dividing Cohen's d
#' by the square root of the total sample size squared - divided
#' by the product of the sample sizes of group one and group two.
#'
#'      r = d / sqrt(d^2 + (n1 + n2)^2 / (n1*n2))
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/dtor.html}{Learn more on our example page.}
#'
#' @param d effect size statistic
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @return Provides the effect size (correlation coefficient) with associated
#' confidence intervals, the t-statistic, F-statistic, and other estimates
#' appropriate for d to r translation. Note this CI is not based on the
#' traditional r-to-z transformation but rather non-central F using the
#' ci.R function from MBESS.
#'
#' \item{r}{correlation coefficient}
#' \item{rlow}{lower level confidence interval r}
#' \item{rhigh}{upper level confidence interval r}
#' \item{R2}{coefficient of determination}
#' \item{R2low}{lower level confidence interval of R2}
#' \item{R2high}{upper level confidence interval of R2}
#' \item{se}{standard error}
#' \item{n}{sample size}
#' \item{dfm}{degrees of freedom of mean}
#' \item{dfe}{degrees of freedom error}
#' \item{t}{t-statistic}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the r statistic and confidence interval in
#' APA style for markdown printing}
#' \item{estimateR2}{the R^2 statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the t-statistic in APA style for markdown printing}
#'
#' @keywords effect size, correlation
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "indt_data" dataset, included
#' #in the MOTE library.
#'
#' #A forensic psychologist conducted a study to examine whether
#' #being hypnotized during recall affects how well a witness
#' #can remember facts about an event. Eight participants
#' #watched a short film of a mock robbery, after which
#' #each participant was questioned about what he or she had
#' #seen. The four participants in the experimental group
#' #were questioned while they were hypnotized. The four
#' #participants in the control group recieved the same
#' #questioning without hypnosis.
#'
#'     t.test(correctq ~ group, data = indt_data)
#'
#' #You can type in the numbers directly, or refer to the dataset,
#' #as shown below.
#'
#'     d.ind.t(m1 = 17.75, m2 = 23, sd1 = 3.30,
#'            sd2 = 2.16, n1 = 4, n2 = 4, a = .05)
#'
#'     d.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)
#'
#'     d.ind.t(mean(indt_data$correctq[indt_data$group == 1]),
#'             mean(indt_data$correctq[indt_data$group == 2]),
#'             sd(indt_data$correctq[indt_data$group == 1]),
#'             sd(indt_data$correctq[indt_data$group == 2]),
#'             length(indt_data$correctq[indt_data$group == 1]),
#'             length(indt_data$correctq[indt_data$group == 2]),
#'             .05)
#'
#' #Contrary to the hypothesized result, the group that underwent
#' #hypnosis were significantly less accurate while reporting
#' #facts than the control group with a large effect size, t(6) = -2.66,
#' #p = .038, d_s = 1.88.
#'
#'      d.to.r(d = -1.88, n1 = 4, n2 = 4, a = .05)


d.to.r <- function (d, n1, n2, a = .05) {

  if (missing(d)){
    stop("Be sure to include d effect size.")
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

  correct = (n1 + n2)^2 / (n1*n2)
  n = n1 + n2
  r <- d / sqrt(d^2 + correct)
  rsq <- (r) ^ 2
  se <- sqrt(4 * rsq * ((1 - rsq) ^ 2) * ((n - 3) ^ 2) / ((n ^ 2 - 1) * (3 + n)))
  t <- r / sqrt((1 - rsq) / (n - 2))
  Fvalue <- t ^ 2
  dfm <- 1
  dfe <- n - 2

  limits <- ci.R2(R2 = rsq, df.1 = dfm, df.2 = dfe, conf.level = (1-a))
  ciforr <- ci.R(R = abs(r), df.1 = dfm, df.2 = dfe, conf.level = (1 - a))
  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  #deal with negative r / d values
  if (r < 0) {
    rlow = 0 - ciforr$Lower.Conf.Limit.R
    rhigh = 0 - ciforr$Upper.Conf.Limit.R
  } else {
    rlow = ciforr$Lower.Conf.Limit.R
    rhigh = ciforr$Upper.Conf.Limit.R
    }

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("r" = r, #r stats
                "rlow" = rlow,
                "rhigh" = rhigh,
                "R2" = rsq, #R squared stats
                "R2low" = limits$Lower.Conf.Limit.R2,
                "R2high" = limits$Upper.Conf.Limit.R2,
                "se" = se,
                "n" = n, #sample stats
                "dfm" = 1, #sig stats
                "dfe" = (n - 2),
                "t" = t,
                "F" = Fvalue,
                "p" = p,
                "estimate" = paste("$r$ = ", apa(r,2,F), ", ", (1-a)*100, "\\% CI [",
                                   apa(rlow,2,F), ", ", apa(rhigh,2,F), "]", sep = ""),
                "estimateR2" = paste("$R^2$ = ", apa(rsq,2,F), ", ", (1-a)*100, "\\% CI [",
                                   apa(limits$Lower.Conf.Limit.R2,2,F), ", ",
                                   apa(limits$Upper.Conf.Limit.R2,2,F), "]", sep = ""),
                "statistic" = paste("$t$(", (n-2), ") = ", apa(t,2,T), ", $p$ ",
                                    reportp, sep = "")
  )

  return(output)
}

#' @rdname d.to.r
#' @export
