#' Odds
#'
#' This function Displays odds ratios, sensitivity / specificity
#'
#' @param n11 input value
#' @param n12 input value
#' @param n21 input value
#' @param n22 input value
#' @param a significance level
#' @param k significant digits to use for formatting
#' @keywords effect size
#' @export
#' @examples
#' Odds(n11, n12, n21, n22, a = .05, k = 2)


Odds <- function (n11, n12, n21, n22, a = .05, k = 2) {
  # Displays odds ratios, sensitivity / specificity
  #
  # Args: 
  #   n11: 
  #   n12: 
  #   n21: 
  #   n22: 
  #   a  : significance level
  #   k  : significant digits to use for formatting
  #
  # Returns:
  #   String describing test statistic, standard 
  #   error, and confidence intervals.
  
  odds <- (n11 / n12) / (n21 / n22)
  se <- sqrt((1 / n11) + (1 / n12) + (1 / n21) + (1 / n22))
  olow <- exp(log(odds)) - qnorm(a / 2, lower.tail = F) * se
  ohigh <- exp(log(odds)) + qnorm(a / 2, lower.tail = F) * se
  # Print the result
  cat("Odds = ", 
      Apa(odds, k),
      ", SE = ", 
      Apa(se, k), 
      ", ", (1 - a) * 100, "%CI[", 
      Apa(olow, k), 
      " - ", 
      Apa(ohigh, k), "]", sep = "")
}
