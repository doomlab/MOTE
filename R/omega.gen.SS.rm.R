#' Generalized Omega Squared for Multi-Way and Mixed ANOVA from F
#'
#' This function displays generalized omega squared from ANOVA analyses
#' and its non-central confidence interval based on the F distribution.
#' This formula is appropriate for multi-way repeated measures designs and mix level designs.
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
#' omega.gen.SS.rm(dfm = 2, dfe = 100,
#'                      ssm = 5339, ssm2 = 432, sst = 10947,
#'                      mss = 543, j = 3, Fvalue = 12, a = .05)

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

