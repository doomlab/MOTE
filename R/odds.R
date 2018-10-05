#' Odds Ratios
#'
#' This function displays odds ratios and their normal confidence intervals.
#' This statistic is calculated as (level 1.1/level 1.2) / (level 2.1/level 2.2),
#' which can be considered the odds of level 1.1 given level1 overall versus level2.1
#' given level2 overall.
#'
#' @param n11 sample size for level 1.1
#' @param n12 sample size for level 1.2
#' @param n21 sample size for level 2.1
#' @param n22 sample size for level 2.2
#' @param a significance level
#' @return Provides odds ratios with associated confidence intervals
#' and relevant statistics.
#'
#' \item{odds}{odds statistic}
#' \item{olow}{lower level confidence interval of odds statistic}
#' \item{ohigh}{upper level confidence interval of odds statistic}
#' \item{se}{standard error}
#'
#' @keywords effect size, odds ratios
#' @export
#' @examples
#' odds(n11 = 10, n12 = 15, n21 = 20, n22 = 5, a = .05)


odds <- function (n11, n12, n21, n22, a = .05) {
  # Displays odds ratios, sensitivity / specificity
  #
  # Args:
  #   n11 : sample size for level 1.1
  #   n12 : sample size for level 1.2
  #   n21 : sample size for level 2.1
  #   n22 : sample size for level 2.2
  #   a   : significance level
  #
  # Returns:
  #   List of odds and sample size statistics

  odds <- (n11 / n12) / (n21 / n22)
  se <- sqrt((1 / n11) + (1 / n12) + (1 / n21) + (1 / n22))
  olow <- exp(log(odds)) - qnorm(a / 2, lower.tail = F) * se
  ohigh <- exp(log(odds)) + qnorm(a / 2, lower.tail = F) * se

  output = list("odds" = odds, #odds stats
                "olow" = olow,
                "ohigh" = ohigh,
                "se" = se)

  return(output)

}
