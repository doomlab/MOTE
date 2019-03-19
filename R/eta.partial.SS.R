#' Partial Eta Squared for ANOVA from F and Sum of Squares
#'
#' This function displays partial eta squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for one way and multi way designs.
#'
#' Partial eta squared is calculated by dividing the sum of squares
#' of the model by the sum of the sum of squares of the model and
#' sum of squares of the error.
#'
#'      partial eta^2 = ssm / (ssm + sse)
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
#' @return Provides partial eta squared with associated confidence intervals
#' and relevant statistics.
#'
#' \item{eta}{partial eta squared effect size}
#' \item{etalow}{lower level confidence interval of partial eta squared}
#' \item{etahigh}{upper level confidence interval of partial eta squared}
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
#' @import ez
#' @export
#' @examples
#'
#' #The following example is derived from the "bn2_data" dataset, included
#' #in the MOTE library.
#'
#' #Is there a difference in atheletic spending budget for different sports?
#' #Does that spending interact with the change in coaching staff? This data includes
#' #(fake) atheletic budgets for baseball, basketball, football, soccer, and volleyball teams
#' #with new and old coaches to determine if there are differences in
#' #spending across coaches and sports.
#'
#' library(ez)
#' bn2_data$partno = 1:nrow(bn2_data)
#' anova_model = ezANOVA(data = bn2_data,
#'                       dv = money,
#'                       wid = partno,
#'                       between = .(coach, type),
#'                       detailed = TRUE,
#'                       type = 3)
#'
#' #You would calculate one eta for each F-statistic.
#' #Here's an example for the interaction with typing in numbers.
#' eta.partial.SS(dfm = 4, dfe = 990,
#'                ssm = 338057.9, sse = 32833499,
#'                Fvalue = 2.548, a = .05)
#'
#' #Here's an example for the interaction with code.
#' eta.partial.SS(dfm = anova_model$ANOVA$DFn[4],
#'                dfe = anova_model$ANOVA$DFd[4],
#'                ssm = anova_model$ANOVA$SSn[4],
#'                sse = anova_model$ANOVA$SSd[4],
#'                Fvalue =  anova_model$ANOVA$F[4],
#'                a = .05)


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
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = "")
  )

  return(output)

}

#' @rdname eta.partial.SS
#' @export
