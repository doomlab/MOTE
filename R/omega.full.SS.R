#' $\omega^2$ for One-Way and Multi-Way ANOVA from $F$
#'
#' This function displays $\omega^2$ from ANOVA analyses
#' and its non-central confidence interval based on the $F$ distribution.
#' This formula works for one way and multi way designs with careful
#' focus on which error term you are using for the calculation.
#'
#' Omega squared is calculated by deducting the mean square of the error
#' from the mean square of the model and multiplying by the degrees of freedom for the model.
#' This is divided by the sum of the sum of squares total and the mean square of the error.
#'
#'     $$\omega^2 = \frac{df_m (ms_m - ms_e)}{SS_T + ms_e}$$
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegass.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param sst sum of squares total
#' @param a significance level
#' @return \describe{
#'   \item{omega}{$\omega^2$ effect size}
#'   \item{omegalow}{lower level confidence interval of $\omega^2$}
#'   \item{omegahigh}{upper level confidence interval of $\omega^2$}
#'   \item{dfm}{degrees of freedom for the model/IV/between}
#'   \item{dfe}{degrees of freedom for the error/residual/within}
#'   \item{F}{$F$-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the $\omega^2$ statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the $F$-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, omega, ANOVA
#' @import stats
#' @export
#' @examples
#'
#' # The following example is derived from the "bn1_data"
#' # dataset, included in the MOTE library.
#'
#' # A health psychologist recorded the number of close inter-personal
#' # attachments of 45-year-olds who were in excellent, fair, or poor
#' # health. People in the Excellent Health group had 4, 3, 2, and 3
#' # close attachments; people in the Fair Health group had 3, 5,
#' # and 8 close attachments; and people in the Poor Health group
#' # had 3, 1, 0, and 2 close attachments.
#'
#' anova_model <- lm(formula = friends ~ group, data = bn1_data)
#' summary.aov(anova_model)
#'
#' omega.full.SS(dfm = 2, dfe = 8,
#'               msm = 12.621, mse = 2.548,
#'               sst = (25.54+19.67), a = .05)



omega.full.SS <- function (dfm, dfe, msm, mse, sst, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(msm)){
    stop("Be sure to include the mean squared model for your model (IV).")
  }

  if (missing(mse)){
    stop("Be sure to include the mean squared error for your model.")
  }

  if (missing(sst)){
    stop("Be sure to include the sum of squares total for your model.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega <- (dfm * (msm - mse)) / (sst + mse)

  Fvalue <- msm / mse

  limits <- ci.R2(R2 = omega, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = FALSE)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output <- list("omega" = omega, #omega stats
                 "omegalow" = limits$Lower.Conf.Limit.R2,
                 "omegahigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\omega^2$ = ", apa(omega,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,TRUE), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,TRUE), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,TRUE), ", $p$ ",
                                     reportp, sep = ""))

  return(output)
}
