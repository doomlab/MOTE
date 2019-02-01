#' Partial Generalized Eta-Squared for ANOVA from F
#'
#' This function displays partial ges squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula works for multi-way repeated measures designs.
#'
#' To calculate partial generalized eta squared, first, the sum of
#' squares of the model, sum of squares of the subject
#' variance, sum of squares for the first and second independent variables,
#' and the sum of squares for the interaction are added together.
#' The sum of squares of the model is divided by this value.
#'
#'      partial ges <- ssm / (ssm + sss + sse1 + sse2 + sse3)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gesrmss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the model/IV/between
#' @param sss sum of squares subject variance
#' @param sse1 sum of squares for the error/residual/within for the first IV
#' @param sse2 sum of squares for the error/residual/within for the second IV
#' @param sse3 sum of squares for the error/residual/within for the interaction
#' @param Fvalue F statistic
#' @param a significance level
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
#' #The following example is derived from the "rm2_data" dataset, included
#' #in the MOTE library.
#'
#' #In this experiment people were given word pairs to rate based on
#' #their "relatedness". How many people out of a 100 would put LOST-FOUND
#' #together? Participants were given pairs of words and asked to rate them
#' #on how often they thought 100 people would give the second word if shown
#' #the first word.  The strength of the word pairs was manipulated through
#' #the actual rating (forward strength: FSG) and the strength of the reverse
#' #rating (backward strength: BSG). Is there an interaction between FSG and
#' #BSG when participants are estimating the relation between word pairs?
#'
#' library(ez)
#' library(reshape)
#' long_mix = melt(rm2_data, id = c("subject", "group"))
#' long_mix$FSG = c(rep("Low-FSG", nrow(rm2_data)),
#'                  rep("High-FSG", nrow(rm2_data)),
#'                  rep("Low-FSG", nrow(rm2_data)),
#'                  rep("High-FSG", nrow(rm2_data)))
#' long_mix$BSG = c(rep("Low-BSG", nrow(rm2_data)*2),
#'                  rep("High-BSG", nrow(rm2_data)*2))
#'
#' anova_model = ezANOVA(data = long_mix,
#'                       dv = value,
#'                       wid = subject,
#'                       within = .(FSG, BSG),
#'                       detailed = TRUE,
#'                       type = 3)
#'
#' #You would calculate one partial GES value for each F-statistic.
#' #Here's an example for the interaction with typing in numbers.
#' ges.partial.SS.rm(dfm = 1, dfe = 157,
#'                   ssm = 2442.948, sss = 76988.13,
#'                   sse1 = 5402.567, sse2 = 8318.75, sse3 = 6074.417,
#'                   Fvalue = 70.9927, a = .05)
#'
#' #Here's an example for the interaction with code.
#' ges.partial.SS.rm(dfm = anova_model$ANOVA$DFn[4],
#'                   dfe = anova_model$ANOVA$DFd[4],
#'                   ssm = anova_model$ANOVA$SSn[4],
#'                   sss = anova_model$ANOVA$SSd[1],
#'                   sse1 = anova_model$ANOVA$SSd[4],
#'                   sse2 = anova_model$ANOVA$SSd[2],
#'                   sse3 = anova_model$ANOVA$SSd[3],
#'                   Fvalue = anova_model$ANOVA$F[4],
#'                   a = .05)

ges.partial.SS.rm <- function (dfm, dfe, ssm, sss, sse1, sse2, sse3, Fvalue, a = .05) {

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

  if (missing(sse1)){
    stop("Be sure to include the sum of squares for your error for the first IV.")
  }

  if (missing(sse2)){
    stop("Be sure to include the sum of squares for your error for the second IV.")
  }

  if (missing(sse3)){
    stop("Be sure to include the sum of squares for your error for the interaction.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the Fvalue from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }


  ges <- ssm / (ssm + sss + sse1 + sse2 + sse3)

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

#' @rdname ges.partial.SS.rm
#' @export
