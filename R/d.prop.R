#' d.prop
#'
#' This function Displays proportion differences, independent
#'
#' @param p1 proportion of people from group 1
#' @param p2 proportion of people from group 2
#' @param n1 sample size group 1
#' @param n2 sample size group 2
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' d.prop(p1 = .4, p2 = .6, n1 = 100, n2 = 100, a = .05, k = 2)


d.prop <- function (p1, p2, n1, n2, a = .05, k = 2) {
  # Displays proportion differences, independent
  #
  # Args: 
  #   p1: proportion of people from group 1
  #   p2: proportion of people from group 2
  #   n1: sample size group 1
  #   n2: sample size group 2
  #   a : significance level
  #   k : significant digits to use for formatting
  #
  # Returns:
  #   String describing z test statistic and 
  #   effect size with confidence intervals.
  
  ppooled <- (p1 * n1 + p2 * n2) / (n1 + n2)
  se <- sqrt(ppooled * (1 - ppooled) * ((1 / n1) + (1 / n2)))
  z <- (p1 - p2)/ se
  p <- pnorm(abs(z), lower.tail = F) * 2
  se1 <- sqrt((p1 * (1 - p1) / n1))
  se2 <- sqrt((p2 * (1 - p2) / n2))
  z1 <- p1 / se1
  z2 <- p2 / se2
  alow <- a / 2
  ahigh <- 1 - (a / 2)
  z1low <- z1 - qnorm(alow, lower.tail = F) * se1
  z1high <- z1 + qnorm(alow, lower.tail = F) * se1
  z2low <- z2 - qnorm(alow, lower.tail = F) * se2
  z2high <- z2 + qnorm(alow, lower.tail = F) * se2
  d <- z1 - z2
  dlow <- d - qnorm(alow, lower.tail = F) * se
  dhigh <- d + qnorm(alow, lower.tail = F) * se
  # Print the result
  cat("Z1 = ", 
      Apa(z1, k),
      ", SE = ", Apa(se1, k),
      ", ", (1 - a) * 100, "%CI[", 
      Apa(z1low, k),
      " - ",
      Apa(z1high, k),
      "]",
      "\nZ2 = ", 
      Apa(z2, k),
      ", SE = ", Apa(se2, k),
      ", ", (1 - a) * 100, "%CI[", 
      Apa(z2low, k),
      " - ",
      Apa(z2high, k),
      "]",
      "\nZ = ", 
      Apa(z, k),
      ", ", 
      p.value(p, k), 
      ", d = ",
      Apa(d, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(dlow, k), 
      " - ", 
      Apa(dhigh, k), "]", sep = "")
}
