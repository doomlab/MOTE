#' Partial Generalized Eta-Squared for Mixed Design ANOVA from F
#'
#' This function displays partial generalized eta-squared (GES) from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for mixed designs.
#'
#' To calculate partial generalized eta squared, first, the sum of
#' squares of the model, sum of squares of the subject
#' variance, sum of squares for the subject variance,
#' and the sum of squares for the error/residual/within are added together.
#' The sum of squares of the model is divided by this value.
#'
#'      partial ges = ssm / (ssm + sss + sse)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gesmixss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sss sum of squares subject variance
#' @param sse sum of squares for the error/residual/within
#' @param Fvalue F statistic
#' @param a significance level
#'
#' @return Partial generalized eta-squared (GES) with associated confidence intervals
#' and relevant statistics.
#' \item{ges}{effect size}
#' \item{geslow}{lower level confidence interval for ges}
#' \item{geshigh}{upper level confidence interval for ges}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/residual/within}
#' \item{F}{F-statistic}
#' \item{p}{p-value}
#' \item{estimate}{the generalized eta squared statistic and confidence interval in
#' APA style for markdown printing}
#' \item{statistic}{the F-statistic in APA style for markdown printing}
#'
#' @keywords effect size, ges, ANOVA
#' @import MBESS
#' @import stats
#' @import ez
#' @import reshape
#' @export
#' @examples
#'
#' #The following example is derived from the "mix2_data" dataset, included
#' #in the MOTE library.
#'
#' #Given previous research, we know that backward strength in free
#' #association tends to increase the ratings participants give when
#' #you ask them how many people out of 100 would say a word in
#' #response to a target word (like Family Feud). This result is
#' #tied to people’s overestimation of how well they think they know
#' #something, which is bad for studying. So, we gave people instructions
#' #on how to ignore the BSG.  Did it help? Is there an interaction
#' #between BSG and instructions given?
#'
#' library(ez)
#' mix2_data$partno = 1:nrow(mix2_data)
#'
#' library(reshape)
#' long_mix = melt(mix2_data, id = c("partno", "group"))
#'
#' anova_model = ezANOVA(data = long_mix,
#'                       dv = value,
#'                       wid = partno,
#'                       between = group,
#'                       within = variable,
#'                       detailed = TRUE,
#'                       type = 3)
#'
#' #You would calculate one partial GES value for each F-statistic.
#' #Here's an example for the interaction with typing in numbers.
#' ges.partial.SS.mix(dfm = 1, dfe = 156,
#'                    ssm = 71.07608,
#'                    sss = 30936.498,
#'                    sse = 8657.094,
#'                    Fvalue = 1.280784, a = .05)
#'
#' #Here's an example for the interaction with code.
#' ges.partial.SS.mix(dfm = anova_model$ANOVA$DFn[4],
#'                dfe = anova_model$ANOVA$DFd[4],
#'                ssm = anova_model$ANOVA$SSn[4],
#'                sss = anova_model$ANOVA$SSd[1],
#'                sse = anova_model$ANOVA$SSd[4],
#'                Fvalue =  anova_model$ANOVA$F[4],
#'                a = .05)

ges.partial.SS.mix <- function (dfm, dfe, ssm, sss, sse, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(sss)){
    stop("Be sure to include the sum of squares for the subject variance.")
  }

  if (missing(sse)){
    stop("Be sure to include the sum of squares for your error for the model.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the Fvalue from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  ges <- ssm / (ssm + sss+ sse)

  limits <- ci.R2(R2 = ges, df.1 = dfm, df.2 = dfe, conf.level = (1-a))

  p <- pf(Fvalue, dfm, dfe, lower.tail = F)

  if (p < .001) {reportp = "< .001"} else {reportp = paste("= ", apa(p,3,F), sep = "")}

  output <- list("ges" = ges, #ges stats
                 "geslow" = limits$Lower.Conf.Limit.R2,
                 "geshigh" = limits$Upper.Conf.Limit.R2,
                 "dfm" = dfm, #sig stats
                 "dfe" = dfe,
                 "F" = Fvalue,
                 "p" = p,
                 "estimate" = paste("$\\eta^2_{G}$ = ", apa(ges,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname ges.partial.SS.mix
#' @export
