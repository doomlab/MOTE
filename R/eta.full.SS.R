#' \eqn{\eta^2} for ANOVA from \eqn{F} and Sum of Squares
#'
#' This function displays \eqn{\eta^2} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula works for one way and multi way designs with careful
#' focus on the sum of squares total.
#'
#' Eta squared is calculated by dividing the sum of squares for the model
#' by the sum of squares total.
#'
#' \deqn{\eta^2 = \frac{SS_M}{SS_T}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etass.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sst sum of squares total
#' @param Fvalue F statistic
#' @param a significance level
#' @return Provides the effect size (\eqn{\eta^2}) with associated confidence intervals and relevant statistics.
#'
#' \describe{
#'   \item{eta}{\eqn{\eta^2} effect size}
#'   \item{etalow}{lower level confidence interval of \eqn{\eta^2}}
#'   \item{etahigh}{upper level confidence interval of \eqn{\eta^2}}
#'   \item{dfm}{degrees of freedom for the model/IV/between}
#'   \item{dfe}{degrees of freedom for the error/residual/within}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{\eta^2} statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, eta, ANOVA
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
#' eta.full.SS(dfm = 2, dfe = 8, ssm = 25.24,
#'             sst = (25.24+19.67), Fvalue = 5.134, a = .05)


eta.full.SS <- function (dfm, dfe, ssm, sst, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sst)){
    stop("Be sure to include the sum of squares total from your ANOVA.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta <- ssm / sst

  limits <- ci.R2(R2 = eta, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = FALSE)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,FALSE), sep = "")}

  output <- list("eta" = eta, #eta stats
                 "etalow" = limits$Lower.Conf.Limit.R2,
                 "etahigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\eta^2$ = ", apa(eta,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,TRUE), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,TRUE), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,TRUE), ", $p$ ",
                                     reportp, sep = ""))

  return(output)
}

