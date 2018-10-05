#' d for z-scores from z
#'
#' This function displays d for z-scores when all you have is the z-statistic.
#' The normal confidence interval is also provided if you have sigma.
#' If sigma is left blank, then you will not see a confidence interval.
#'
#' To calculate d, z is divided by the square root of N.
#' \href{Learn more on our example page.}{https://www.aggieerin.com/shiny-server/tests/ztestz.html}
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
#'
#' @keywords effect size, z-test
#' @export
#' @examples
#'
#' A recent study suggested that students (N = 100) learning
#' statistics improved their test scores with the use of
#' visual aids (Z = 2.5). The population standard deviation is 4.
#'
#' You can type in the numbers directly as shown below,
#' or refer to your dataset within the function.
#'
#'     d.z.z(z = 2.5, sig = 4, n = 100, a = .05)
#'
#'     d.z.z(z = 2.5, n = 100, a = .05)
#'
#'     d.z.z(2.5, 4, 100, .05)


d.z.z <- function (z, sig = NA, n, a = .05) {
  # Displays d for z-test where you have one sample and the population
  # mean and standard deviation. The normal confidence intervals are also provided.
  #
  # Args:
  #   z   : z-test statistic
  #   sig : population standard deviation can be NA
  #   n   : sample size
  #   a   : significance level
  #
  # Returns:
  #   List of d, z statistics

  library(MBESS)

  d <- z / sqrt(n)
  if (is.na(sig)){
    dlow <- NA
    dhigh <- NA
  } else {
    dlow <- d-qnorm(a/2, lower.tail = F)*sig
    dhigh <- d+qnorm(a/2, lower.tail = F)*sig
  }
  p <- pnorm(z, lower.tail = FALSE)*2

  output = list("d" = d, #d stats
                "dlow" = dlow,
                "dhigh" = dhigh,
                "sigma" = sig, #population stats
                "z" = z, #sig stats
                "p" = p,
                "n" = n #sample stats
                )

  return(output)
  }
