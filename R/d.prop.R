#' d.prop
#'
#' This function displays d and central confidence interval
#' calculated from differences in independent proportions. 
#'
#' @param p1 proportion of people group one
#' @param p2 proportion of people group two
#' @param n1 sample size group one
#' @param n2 sample size group two
#' @param a significance level
#' @keywords effect size, prop test, proportions
#' @export
#' @examples
#' d.prop(p1 = .4, p2 = .6, n1 = 100, n2 = 100, a = .05)


d.prop <- function (p1, p2, n1, n2, a = .05) {
  # This function displays d and central confidence interval
  # calculated from differences in independent proportions. 
  #
  # Args: 
  #   p1: proportion of people group one
  #   p2: proportion of people group two
  #   n1: sample size group one
  #   n2: sample size group two
  #   a : significance level
  #
  # Returns:
  #   List of d, proportions, and sample size statistics
  
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
                "p" = p) 
  
  return(output)
}
