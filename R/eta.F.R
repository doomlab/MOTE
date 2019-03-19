#' Eta and Coefficient of Determination (R2) for ANOVA from F
#'
#' This function displays eta squared from ANOVA analyses
#' and their non-central confidence interval based on the F distribution.
#' These values are calculated directly from F statistics and can be used
#' for between subjects and repeated measures designs.
#' Remember if you have two or more IVs, these values are partial eta squared.
#'
#' Eta is calculated by multiplying the degrees of freedom of
#' the model by the F-statistic. This is divided by the product
#' of degrees of freedom of the model, the F-statistic, and
#' the degrees of freedom for the error or residual.
#'
#'      eta^2 = (dfm * Fvalue) / (dfm * Fvalue + dfe)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etaf.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#' @return Provides eta with associated confidence intervals and relevant statistics.
#'
#' \item{eta}{effect size}
#' \item{etalow}{lower level confidence interval of eta}
#' \item{etahigh}{upper level confidence interval of eta}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the eta squared statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, eta, ANOVA
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
#' eta.F(dfm = 2, dfe = 8,
#'       Fvalue = 5.134, a = .05)


eta.F <- function (dfm, dfe, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta <- (dfm * Fvalue) / (dfm * Fvalue + dfe)

  limits <- ci.R2(R2 = eta, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("eta" = eta, #eta stats
                "etalow" = limits$Lower.Conf.Limit.R2,
                "etahigh" = limits$Upper.Conf.Limit.R2,
                "dfm" = dfm, #sig stats
                "dfe" = dfe,
                "F" = Fvalue,
                "p" = p,
                "estimate" = paste("$\\eta^2$ = ", apa(eta,2,T), ", ", (1-a)*100, "\\% CI [",
                                   apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                   apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                    apa(Fvalue,2,T), ", $p$ ",
                                    reportp, sep = ""))

  return(output)

}

#' @rdname eta.F
#' @export
