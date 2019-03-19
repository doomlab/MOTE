#' Epsilon for ANOVA from F and Sum of Squares
#'
#' This function displays epsilon squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total calculation.
#'
#' To calculate epsilon, first, the mean square for the error is
#' substracted from the mean square for the model. The difference
#' is multiplied by the degrees of freedom for the model. The
#' product is divided by the sum of squares total.
#'
#'      epsilon^2 = (dfm * (msm - mse)) / (sst)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/epsilon.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total
#' @param a significance level
#' @return Provides the effect size (epsilon) with associated
#' confidence intervals from the F-statistic.
#'
#' \item{epsilon}{effect size}
#' \item{epsilonlow}{lower level confidence interval of epsilon}
#' \item{epsilonhigh}{upper level confidence interval of epsilon}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the epsilon statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, epsilon, ANOVA
#' @import MBESS
#' @import stats
#' @export
#' @examples
#'
#' #The following example is derived from the "bn1_data" dataset, included
#' #in the MOTE library.
#'
#' #A health psychologist recorded the number of close inter-personal
#' #attachments of 45-year-olds who were in excellent, fair, or poor
#' #health. People in the Excellent Health group had 4, 3, 2, and 3
#' #close attachments; people in the Fair Health group had 3, 5,
#' #and 8 close attachments; and people in the Poor Health group
#' #had 3, 1, 0, and 2 close attachments.
#'
#' anova_model = lm(formula = friends ~ group, data = bn1_data)
#' summary.aov(anova_model)
#'
#' epsilon.full.SS(dfm = 2, dfe = 8, msm = 12.621,
#'                 mse = 2.458, sst = (25.24+19.67), a = .05)


epsilon.full.SS <- function (dfm, dfe, msm, mse, sst, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(msm)){
    stop("Be sure to include the mean square value for your model (IV).")
  }

  if (missing(mse)){
    stop("Be sure to include the mean square value for the error.")
  }

  if (missing(sst)){
    stop("Be sure to include the sum of squares total for your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  epsilon <- (dfm * (msm - mse)) / (sst)
  Fvalue <- msm / mse

  limits <- ci.R2(R2 = epsilon, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("epsilon" = epsilon, #epsilon stats
                 "epsilonlow" = limits$Lower.Conf.Limit.R2,
                 "epsilonhigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\epsilon^2$ = ", apa(epsilon,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname epsilon.full.SS
#' @export
