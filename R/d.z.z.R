#' d from z-statistic for Z-test
#'
#' This function displays d for Z-tests when all you have is the z-statistic.
#' The normal confidence interval is also provided if you have sigma.
#' If sigma is left blank, then you will not see a confidence interval.
#'
#' To calculate d, z is divided by the square root of N.
#'
#'      d = z / sqrt(N)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/zz.html}{Learn more on our example page.}
#'
#' @param z z statistic
#' @param sig population standard deviation
#' @param n sample size
#' @param a significance level
#' @return The effect size (Cohen's d) with associated confidence intervals and
#' relevant statistics.
#'
#' \item{d}{effect size}
#' \item{dlow}{lower level confidence interval d value}
#' \item{dhigh}{upper level confidence interval d value}
#' \item{sigma}{sample size}
#' \item{z}{sig stats}
#' \item{p}{p-value}
#' \item{n}{sample size}
#' \item{estimate}{the d statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the Z-statistic in APA style for markdown printing}
#'
#' @keywords effect size, z-test
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #A recent study suggested that students (N = 100) learning
#' #statistics improved their test scores with the use of
#' #visual aids (Z = 2.5). The population standard deviation is 4.
#'
#' #You can type in the numbers directly as shown below,
#' #or refer to your dataset within the function.
#'
#'     d.z.z(z = 2.5, sig = 4, n = 100, a = .05)
#'
#'     d.z.z(z = 2.5, n = 100, a = .05)
#'
#'     d.z.z(2.5, 4, 100, .05)


d.z.z <- function (z, sig = NA, n, a = .05) {

  if (missing(z)){
    stop("Be sure to include z from the z-statistic.")
  }

  if (missing(n)){
    stop("Be sure to include the sample size n for the sample.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  d <- z / sqrt(n)
  if (is.na(sig)){
    dlow <- NA
    dhigh <- NA
  } else {
    dlow <- d-qnorm(a/2, lower.tail = F)*sig
    dhigh <- d+qnorm(a/2, lower.tail = F)*sig
  }
  p <- pnorm(z, lower.tail = FALSE)*2

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "sigma" = sig, #population stats
                "z" = z, #sig stats
                "p" = p,
                "n" = n, #sample stats
                "estimate" = paste("$d$ = ", apa(d,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa((d-qnorm(a/2, lower.tail = F)*sig),2,T), ", ", apa((d+qnorm(a/2, lower.tail = F))*sig,2,T), "]", sep = ""),
                "statistic" = paste("$Z$", " = ", apa(z,2,T), ", $p$ ",
                                    reportp, sep = "")
                )

  return(output)
  }

#' @rdname d.z.z
#' @export
