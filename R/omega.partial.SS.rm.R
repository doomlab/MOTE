#' Partial Omega Squared for Repeated Measures ANOVA from F
#'
#' This function displays omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way repeated measures designs and mix level designs.
#'
#' Partial omega squared is calculated by subtracting the mean
#' square for the error from the mean square of the model, which is
#' multiplied by degrees of freedom of the model. This is divided
#' by the sum of the sum of squares for the model, sum of squares
#' for the error, sum of squares for the subject, and the
#' mean square of the subject.
#'
#'      omega_p^2 = (dfm x (msm - mse)) / (ssm + sse + sss + mss)
#'
#' The F-statistic is calculated by dividing the mean square
#' of the model by the mean square of the error.
#'
#'      F = msm / mse
#'
#'\href{https://www.aggieerin.com/shiny-server/tests/omegaprmss.html}{Learn more on our example page.}
#'
#' @param dfm degrees of freedom for the model/IV/between
#' @param dfe degrees of freedom for the error/residual/within
#' @param msm mean square for the model/IV/between
#' @param mse mean square for the error/residual/within
#' @param mss mean square for the subject variance
#' @param ssm sum of squares for the model/IV/between
#' @param sse sum of squares for the error/residual/within
#' @param sss sum of squares for the subject variance
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
#' #You would calculate one partial GOS value for each F-statistic.
#' #You can leave out the MS options if you include all the SS options.
#' #Here's an example for the interaction with typing in numbers.
#' omega.partial.SS.rm(dfm = 1, dfe = 157,
#'                     msm = 2442.948 / 1,
#'                     mse = 5402.567 / 157,
#'                     mss = 76988.130 / 157,
#'                     ssm = 2442.948, sss = 76988.13,
#'                     sse = 5402.567, a = .05)
#'
#' #Here's an example for the interaction with code.
#' omega.partial.SS.rm(dfm = anova_model$ANOVA$DFn[4],
#'                   dfe = anova_model$ANOVA$DFd[4],
#'                   msm = anova_model$ANOVA$SSn[4] / anova_model$ANOVA$DFn[4],
#'                   mse = anova_model$ANOVA$SSd[4] / anova_model$ANOVA$DFd[4],
#'                   mss = anova_model$ANOVA$SSd[1] / anova_model$ANOVA$DFd[1],
#'                   ssm = anova_model$ANOVA$SSn[4],
#'                   sse = anova_model$ANOVA$SSd[4],
#'                   sss = anova_model$ANOVA$SSd[1],
#'                   a = .05)

omega.partial.SS.rm <- function (dfm, dfe, msm, mse, mss, ssm, sse, sss, a = .05) {

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
    stop("Be sure to include the sum of squares for your error.")
  }

  if (missing(sss)){
    stop("Be sure to include the sum of squares for the subject variance.")
  }

  if (missing(msm)){
    msm = ssm / dfm
  }

  if (missing(mse)){
    mse = sse / dfe
  }

  if (missing(mss)){
    mss = sss / dfe
  }

  if (a < 0 || a > 1) {
    stop("Alpha should be between 0 and 1.")
  }

  omega <- (dfm * (msm - mse)) / (ssm + sse + sss + mss)
  Fvalue <- msm / mse

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
                 "estimate" = paste("$\\omega^2_{p}$ = ", apa(omega,2,T), ", ", (1-a)*100, "\\% CI [",
                                    apa(limits$Lower.Conf.Limit.R2,2,T), ", ",
                                    apa(limits$Upper.Conf.Limit.R2,2,T), "]", sep = ""),
                 "statistic" = paste("$F$(", dfm, ", ", dfe, ") = ",
                                     apa(Fvalue,2,T), ", $p$ ",
                                     reportp, sep = ""))

  return(output)

}

#' @rdname omega.partial.SS.rm
#' @export
