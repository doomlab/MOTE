#' \eqn{\eta^2_p} for ANOVA from \eqn{F} and Sum of Squares
#'
#' This function displays \eqn{\eta^2_p} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula works for one way and multi way designs.
#'
#' \eqn{\eta^2_p} is calculated by dividing the sum of squares
#' of the model by the sum of the sum of squares of the model and
#' sum of squares of the error.
#'
#' \deqn{\eta^2_p = \frac{SS_M}{SS_M + SS_E}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/etapss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#'
#' @return Provides the effect size (\eqn{\eta^2_p}) with associated confidence intervals
#' and relevant statistics.
#'
#' \describe{
#' \item{eta}{\eqn{\eta^2_p} effect size}
#' \item{etalow}{lower level confidence interval of \eqn{\eta^2_p}}
#' \item{etahigh}{upper level confidence interval of \eqn{\eta^2_p}}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{\eqn{F}-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the \eqn{\eta^2_p} statistic and confidence interval in APA style for markdown printing}
#' \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, eta, ANOVA
#' @import stats
#' @examples
#'
#' # The following example is derived from the "bn2_data"
#' # dataset, included in the MOTE library.
#'
#' # Is there a difference in athletic spending budget for different sports?
#' # Does that spending interact with the change in coaching staff? This data includes
#' # (fake) athletic budgets for baseball, basketball, football, soccer, and volleyball teams
#' # with new and old coaches to determine if there are differences in
#' # spending across coaches and sports.
#'
#' # Example using reported ANOVA table values directly
#' eta.partial.SS(dfm = 4, dfe = 990,
#'                ssm = 338057.9, sse = 32833499,
#'                Fvalue = 2.548, a = .05)
#'
#' # Example computing Type III SS with code (requires the "car" package)
#' if (requireNamespace("car", quietly = TRUE)) {
#'
#'   # Fit the model using stats::lm
#'   mod <- stats::lm(money ~ coach * type, data = bn2_data)
#'
#'   # Type III table for the effects
#'   aov_type3 <- car::Anova(mod, type = 3)
#'
#'   # Extract degrees of freedom, sum of squares, and F for the interaction (coach:type)
#'   dfm_int <- aov_type3["coach:type", "Df"]
#'   ssm_int <- aov_type3["coach:type", "Sum Sq"]
#'   F_int   <- aov_type3["coach:type", "F value"]
#'
#'   # Residual degrees of freedom and sum of squares from the standard ANOVA table
#'   aov_type1 <- stats::anova(mod)
#'   dfe <- aov_type1["Residuals", "Df"]
#'   sse <- aov_type1["Residuals", "Sum Sq"]
#'
#'   # Calculate partial eta-squared for the interaction using Type III SS
#'   eta.partial.SS(dfm = dfm_int, dfe = dfe,
#'                  ssm = ssm_int, sse = sse,
#'                  Fvalue = F_int, a = .05)
#' }

eta.partial.SS <- function (dfm, dfe, ssm, sse, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sse)){
    stop("Be sure to include the sum of squares for the error.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the F-statistic from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  eta <- ssm / (ssm + sse)

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
                 "estimate" = paste("$\\eta^2_{p}$ = ", apa(eta,2,FALSE), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,TRUE), ", ", apa(limits$Upper.Conf.Limit.R2,2,TRUE), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,TRUE), ", $p$ ",
                                     reportp, sep = "")
  )

  return(output)

}
