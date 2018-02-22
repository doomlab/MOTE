#' d.z.z
#'
#' This function displays d for z-scores when all you have is the z-statistics.
#' The normoal confidence interval is also provided if you have sigma.
#' If sigma is left blank, then you will not see a confidence interval.
#'
#' @param z z statistic
#' @param sig population standard deviation
#' @param n sample size
#' @param a significance level
#' @keywords effect size, z-test
#' @export
#' @examples
#' d.z.z(z = 1.25, n = 10, a = .05)


d.z.z <- function (z, sig = NA, n, a = .05) {
  # Displays d for z-test where you have one sample and the population
  # mean and standard deviation. The normal confidence intervals are also provided.
  #
  # Args:
  #   z  : z-test statistic
  #   n   : sample size
  #   a   : significance level
  #
  # Returns:
  #   List of d, z statistics
  
  library(MBESS)
  
  d <- z / sqrt(n)
  if (sig == NA){
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
                "Sigma" = sig, #population stats
                "z" = z, #sig stats
                "p" = p,
                "n" = n, #sample stats
                ) 
                
  return(output)
  }
