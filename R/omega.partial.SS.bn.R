#' omega^2_p (Partial Omega Squared) for Between-Subjects ANOVA from F
#'
#' This function displays \eqn{\omega^2_p} from ANOVA analyses
#' and its non-central confidence interval based on the \eqn{F} distribution.
#' This formula is appropriate for multi-way between-subjects designs.
#'
#' Partial omega squared is calculated by subtracting the mean square for the error
#' from the mean square of the model, which is multiplied by degrees of freedom of
#' the model. This is divided by the product of the degrees of freedom
#' for the model are deducted from the sample size, multiplied by the
#' mean square of the error, plus the sum of squares for the model.
#'
#' \deqn{\omega^2_p = \frac{df_m (MS_M - MS_E)}{SS_M + (n - df_m) \times MS_E}}
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/omegapbnss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param n total sample size
#' @param a significance level
#'
#' @return \describe{
#'   \item{omega}{\eqn{\omega^2_p} effect size}
#'   \item{omegalow}{lower level confidence interval of \eqn{\omega^2_p}}
#'   \item{omegahigh}{upper level confidence interval of \eqn{\omega^2_p}}
#'   \item{dfm}{degrees of freedom for the model/IV/between}
#'   \item{dfe}{degrees of freedom for the error/residual/within}
#'   \item{F}{\eqn{F}-statistic}
#'   \item{p}{p-value}
#'   \item{estimate}{the \eqn{\omega^2_p} statistic and confidence interval in APA style for markdown printing}
#'   \item{statistic}{the \eqn{F}-statistic in APA style for markdown printing}
#' }
#'
#' @keywords effect size, omega, ANOVA
#' @import stats
#' @export
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
#' # You would calculate one omega value for each F-statistic.
#' # Here's an example for the interaction using reported ANOVA values.
#' omega.partial.SS.bn(dfm = 4, dfe = 990,
#'                     msm = 338057.9 / 4,
#'                     mse = 32833499 / 990,
#'                     ssm = 338057.9,
#'                     n = 1000, a = .05)
#'
#' # The same analysis can be fit with stats::lm and car::Anova(type = 3).
#' # This example shows how to obtain the ANOVA table and plug its values
#' # into omega.partial.SS.bn without relying on ezANOVA.
#' if (requireNamespace("car", quietly = TRUE)) {
#'
#'   mod <- stats::lm(money ~ coach * type, data = bn2_data)
#'
#'   # Type I table (for residual SS and df)
#'   aov_type1 <- stats::anova(mod)
#'
#'   # Type III SS table for the effects
#'   aov_type3 <- car::Anova(mod, type = 3)
#'
#'   # Extract dfs and sums of squares for the interaction coach:type
#'   dfm_int <- aov_type3["coach:type", "Df"]
#'   ssm_int <- aov_type3["coach:type", "Sum Sq"]
#'   msm_int <- ssm_int / dfm_int
#'
#'   dfe <- aov_type1["Residuals", "Df"]
#'   sse <- aov_type1["Residuals", "Sum Sq"]
#'   mse <- sse / dfe
#'
#'   omega.partial.SS.bn(dfm = dfm_int,
#'                       dfe = dfe,
#'                       msm = msm_int,
#'                       mse = mse,
#'                       ssm = ssm_int,
#'                       n = nrow(bn2_data),
#'                       a = .05)
#' }

omega.partial.SS.bn <- function (dfm, dfe, msm, mse, ssm, n, a = .05) {

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

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(n)){
    stop("Be sure to include total sample size.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega <- (dfm * (msm - mse)) / (ssm + (n-dfm)*mse)

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
                 "estimate" = paste("$\\omega^2_{p}$ = ", apa(omega,2,TRUE), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,TRUE), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,TRUE), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,TRUE), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}
