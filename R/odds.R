#' Chi-Square Odds Ratios
#'
#' This function displays odds ratios and their normal confidence intervals.
#'
#' This statistic is the ratio between level 1.1 divided by level 1.2, and
#' level 2.1 divided by 2.2. In other words, these are the odds of level 1.1
#' given level 1 overall versus level 2.1 given level 2 overall.
#'
#' To calculate odds ratios, First, the sample size for level 1.1
#' is divided by the sample size for level 1.2. This value is divided
#' by the sample size for level 2.1, after dividing by the sample
#' size of level 2.2.
#'
#'      odds <- (n11 / n12) / (n21 / n22)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/chio.html}{Learn more on our example page.}
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
#' \item{estimate}{the oods statistic and confidence interval in
#' APA style for markdown printing}
#'
#' @keywords effect size, odds ratios
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #A health psychologist was interested in the rates of anxiety in
#' #first generation and regular college students. They polled campus
#' #and found the following data:
#'
#'   #|              | First Generation | Regular |
#'   #|--------------|------------------|---------|
#'   #| Low Anxiety  | 10               | 50      |
#'   #| High Anxiety | 20               | 15      |
#'
#' #What are the odds for the first generation students to have anxiety?
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

  odds <- (n11 / n12) / (n21 / n22)
  se <- sqrt((1 / n11) + (1 / n12) + (1 / n21) + (1 / n22))
  olow <- exp(log(odds)) - qnorm(a / 2, lower.tail = F) * se
  ohigh <- exp(log(odds)) + qnorm(a / 2, lower.tail = F) * se

  output = list("odds" = odds, #odds stats
                "olow" = olow,
                "ohigh" = ohigh,
                "se" = se,
                "estimate" = paste("$Odds$ = ", apa(odds,2,F), ", ", (1-a)*100, "\\% CI [",
                                   apa(olow,2,F), ", ", apa(ohigh,2,F), "]", sep = ""))

  return(output)

}

#' @rdname odds
#' @export
