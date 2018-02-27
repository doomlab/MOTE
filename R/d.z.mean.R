#' d.z.mean
#'
#' This function displays d for z-scores with the 
#' population mean and standard deviation.
#' The normoal confidence interval is also provided.
#'
#' @param mu population mean
#' @param m1 sample study mean
#' @param sig population standard deviation
#' @param sd1 standard deviation from the study
#' @param n sample size
#' @param a significance level
#' @keywords effect size, z-test
#' @export
#' @examples
#' d.z.mean(mu = 20, m1 = 17, sig = 4, sd1 = 5, n = 100, a = .05)


d.z.mean <- function (mu, m1, sig, sd1, n, a = .05) {
  # Displays d for z-test where you have one sample and the population
  # mean and standard deviation. The normal confidence intervals are also provided.
  #
  # Args:
  #   mu  : population mean
  #   m1  : sample study mean
  #   sig : population standard deviation
  #   sd1 : standard deviation from the study
  #   n   : sample size
  #   a   : significance level
  #
  # Returns:
  #   List of d, mean, and sample size statistics
  
  library(MBESS)
  
  d <- (mu - m1) / sig
  se1 <- sig / sqrt(n)
  se2 <- sd1 / sqrt(n)
  dlow <- d-qnorm(a/2, lower.tail = F)*sig
  dhigh <- d+qnorm(a/2, lower.tail = F)*sig
  z <- (mu - m1) / se1
  p <- pnorm(abs(z), lower.tail = FALSE)*2
  M1low <- m1 - se2 * qnorm(a/2, lower.tail = FALSE)
  M1high <- m1 + se2 * qnorm(a/2, lower.tail = FALSE)
  
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
                "n" = n #sample stats
                ) 
                
  return(output)
  }
