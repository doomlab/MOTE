#' Generalized Omega Squared for Multi-Way and Mixed ANOVA from F
#'
#' This function displays generalized omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way repeated measures
#' designs and mix level designs.
#'
#' Omega squared is calculated by subtracting the product of the
#' degrees of freedom of the model and the mean square of the
#' subject variance from the sum of squares for the model.
#'
#' This is divided by the value obtained after combining
#' the sum of squares total, sum of squares for the other
#' independent variable, and the mean square of the
#' subject variance multiplied by the number of levels
#' in the other model/IV/between.
#'
#'      generalized omega^2 = (ssm - (dfm * mss)) / (sst + ssm2 + j*mss)
#'
#' \href{https://www.aggieerin.com/shiny-server/tests/gosrmss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param ssm sum of squares for the MAIN model/IV/between
#' @param ssm2 sum of squares for the OTHER model/IV/between
#' @param sst sum of squares total across the whole ANOVA
#' @param mss mean square for the subject variance
#' @param j number of levels in the OTHER IV
#' @param Fvalue F statistic from the output for your IV
#' @param a significance level
#' @return Provides omega squared with associated confidence intervals
#' and relevant statistics.
#'
#' \item{omega}{omega squared}
#' \item{omegalow}{lower level confidence interval of omega}
#' \item{omegahigh}{upper level confidence interval of omega}
#' \item{dfm}{degrees of freedom for the model/IV/between}
#' \item{dfe}{degrees of freedom for the error/resisual/within}
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
#' #You would calculate one partial GOS value for each F-statistic.
#' #Here's an example for the main effect 1 with typing in numbers.
#'  omega.gen.SS.rm(dfm = 1, dfe = 156,
#'                  ssm = 6842.46829,
#'                  ssm2 = 14336.07886,
#'                  sst = sum(c(30936.498, 6842.46829,
#'                  14336.07886, 8657.094, 71.07608)),
#'                  mss = 30936.498 / 156,
#'                  j = 2, Fvalue = 34.503746, a = .05)
#'
#' #Here's an example for the main effect 1 with code.
#'  omega.gen.SS.rm(dfm = anova_model$ANOVA$DFn[2],
#'                  dfe = anova_model$ANOVA$DFd[2],
#'                  ssm = anova_model$ANOVA$SSn[2],
#'                  ssm2 = anova_model$ANOVA$SSn[3],
#'                  sst = sum(c(anova_model$ANOVA$SSn[-1], anova_model$ANOVA$SSd[c(1,3)])),
#'                  mss = anova_model$ANOVA$SSd[1]/anova_model$ANOVA$DFd[1],
#'                  j = anova_model$ANOVA$DFn[3]+1,
#'                  Fvalue = anova_model$ANOVA$F[2], a = .05)


omega.gen.SS.rm <- function (dfm, dfe, ssm, ssm2, sst, mss, j, Fvalue, a = .05) {

  if (missing(dfm)){
    stop("Be sure to include the degrees of freedom for the model (IV).")
  }

  if (missing(dfe)){
    stop("Be sure to include the degrees of freedom for the error.")
  }

  if (missing(ssm)){
    stop("Be sure to include the sum of squares for your model (IV).")
  }

  if (missing(ssm2)){
    stop("Be sure to include the sum of squares for the OTHER model (IV).")
  }

  if (missing(sst)){
    stop("Be sure to include the sum of squares total for your model.")
  }

  if (missing(mss)){
    stop("Be sure to include the mean square for your subjects from your model.")
  }

  if (missing(j)){
    stop("Be sure to include the number of levels in the OTHER IV.")
  }

  if (missing(Fvalue)){
    stop("Be sure to include the Fvalue from your ANOVA.")
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega <- (ssm - (dfm * mss)) / (sst + ssm2 + j*mss)

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
                 "estimate" = paste("$\\omega^2_{G}$ = ", apa(omega,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname omega.gen.SS.rm
#' @export

