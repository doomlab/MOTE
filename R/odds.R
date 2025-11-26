#' Odds Ratio from 2x2 Table
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
#' @return \describe{
#'   \item{odds}{odds ratio statistic}
#'   \item{olow}{lower level confidence interval of odds ratio}
#'   \item{ohigh}{upper level confidence interval of odds ratio}
#'   \item{se}{standard error}
#'   \item{estimate}{the odds ratio statistic and confidence interval in APA style for markdown printing}
#' }
#'
#' @keywords effect size, odds ratios
#' @import stats
#' @export
#' @examples
#'
#' # A health psychologist was interested in the rates of anxiety in
#' # first generation and regular college students. They polled campus
#' # and found the following data:
#'
#'   # |              | First Generation | Regular |
#'   # |--------------|------------------|---------|
#'   # | Low Anxiety  | 10               | 50      |
#'   # | High Anxiety | 20               | 15      |
#'
#' # What are the odds for the first generation students to have anxiety?
#'
#' odds(n11 = 10, n12 = 50, n21 = 20, n22 = 15, a = .05)


odds <- function (n11, n12, n21, n22, a = .05) {

  if (missing(n11)){
    stop("Be sure to include the sample size for row 1 and column 1.")
  }

  if (missing(n12)){
    stop("Be sure to include the sample size for row 1 and column 2.")
  }

  if (missing(n21)){
    stop("Be sure to include the sample size for row 2 and column 1.")
  }

  if (missing(n22)){
    stop("Be sure to include the sample size for row 2 and column 2.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

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
  olow <- exp(log(odds) - qnorm(a / 2, lower.tail = F) * se)
  ohigh <- exp(log(odds) + qnorm(a / 2, lower.tail = F) * se)

  output = list("odds" = odds, #odds stats
                "olow" = olow,
                "ohigh" = ohigh,
                "se" = se)

  return(output)

}

