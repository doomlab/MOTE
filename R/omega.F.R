#' Omega Squared for ANOVA from F
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial omega squared.
#'
#' Omega squared or partial omega squared is calculated by subtracting one
#' from the F-statistic and multiplying it by degrees of freedom of the model. This is
#' divided by the same value after adding the number of valid responses. This
#' value will be omega squared for one-way ANOVA designs, and will be
#' partial omega squared for multi-way ANOVA designs (i.e. with more than one IV).
#'
#'      omega^2 = (dfm * (Fvalue-1)) / ((dfm * (Fvalue-1)) + n)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegaf.html}{Learn more on our example page.}
#'
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param Fvalue F statistic
#' @param n full sample size
#' @param a significance level
#' @return The effect size (Cohen's d) with associated confidence intervals
#' and relevant statistics.
#'
#' \item{omega}{omega statistic}
#' \item{omegalow}{lower level confidence interval d value}
#' \item{omegahigh}{upper level confidence interval d value}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the omega squared statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, omega, ANOVA
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
#' omega.F(dfm = 2, dfe = 8,
#'       Fvalue = 5.134, n = 11, a = .05)


omega.F <- function (dfm, dfe, Fvalue, n, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the Fvalue from your ANOVA.")
  }

  if (missing(n)){
    stop("Be sure to include total sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega <- (dfm * (Fvalue-1)) / ((dfm * (Fvalue-1)) + n)

  limits <- ci.R2(R2 = omega, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("omega" = omega, #omega stats
                "omegalow" = limits$Lower.Conf.Limit.R2,
                "omegahigh" = limits$Upper.Conf.Limit.R2,
                "dfm" = dfm, #sig stats
                "dfe" = dfe,
                "F" = Fvalue,
                "p" = p,
                "estimate" = paste("$\\omega^2$ = ", apa(omega,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                   apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                    apa(Fvalue,2,T), ", $p$ ",
                                    reportp, sep = ""))

  return(output)

}

#' @rdname omega.F
#' @export

