% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/omega.partial.SS.bn.R
\name{omega.partial.SS.bn}
\alias{omega.partial.SS.bn}
\title{Partial Omega Squared for Between Subjects ANOVA from F}
\usage{
omega.partial.SS.bn(dfm, dfe, msm, mse, ssm, n, a = 0.05)
}
\arguments{
\item{dfm}{degrees of freedom for the model/IV/between}

\item{dfe}{degrees of freedom for the error/residual/within}

\item{msm}{mean square for the model/IV/between}

\item{mse}{mean square for the error/residual/within}

\item{ssm}{sum of squares for the model/IV/between}

\item{n}{total sample size}

\item{a}{significance level}
}
\value{
Provides omega squared with associated confidence intervals
and relevant statistics.

\item{omega}{omega squared}
\item{omegalow}{lower level confidence interval of omega}
\item{omegahigh}{upper level confidence interval of omega}
\item{dfm}{degrees of freedom for the model/IV/between}
\item{dfe}{degrees of freedom for the error/resisual/within}
\item{F}{F-statistic}
\item{p}{p-value}
\item{estimate}{the omega squared statistic and confidence interval in
APA style for markdown printing}
\item{statistic}{the F-statistic in APA style for markdown printing}
}
\description{
This function displays omega squared from ANOVA analyses
and its non-central confidence interval based on the F distribution.
This formula is appropriate for multi-way between subjects designs.
}
\details{
Partial omega squared is calculated by subtracting the mean square for the error
from the mean square of the model, which is multiplied by degrees of freedom of
the model. This is divided by the product of the degrees of freedom
for the model are deducted from the sample size, multiplied by the
mean square of the error, plus the sum of squares for the model.

     omega^2 <- (dfm * (msm - mse)) / (ssm + (n-dfm)*mse)

\href{https://www.aggieerin.com/shiny-server/tests/omegapbnss.html}{Learn more on our example page.}
}
\examples{

#The following example is derived from the "bn2_data" dataset, included
#in the MOTE library.

#Is there a difference in atheletic spending budget for different sports?
#Does that spending interact with the change in coaching staff? This data includes
#(fake) atheletic budgets for baseball, basketball, football, soccer, and volleyball teams
#with new and old coaches to determine if there are differences in
#spending across coaches and sports.

library(ez)
bn2_data$partno = 1:nrow(bn2_data)
anova_model = ezANOVA(data = bn2_data,
                      dv = money,
                      wid = partno,
                      between = .(coach, type),
                      detailed = TRUE,
                      type = 3)

#You would calculate one eta for each F-statistic.
#Here's an example for the interaction with typing in numbers.
omega.partial.SS.bn(dfm = 4, dfe = 990,
                    msm = 338057.9 / 4,
                    mse = 32833499 / 990,
                    ssm = 338057.9,
                    n = 1000, a = .05)

#Here's an example for the interaction with code.
omega.partial.SS.bn(dfm = anova_model$ANOVA$DFn[4],
                    dfe = anova_model$ANOVA$DFd[4],
                    msm = anova_model$ANOVA$SSn[4] / anova_model$ANOVA$DFn[4],
                    mse = anova_model$ANOVA$SSd[4] / anova_model$ANOVA$DFd[4],
                    ssm = anova_model$ANOVA$SSn[4],
                    n = nrow(bn2_data),
                    a = .05)
}
\keyword{ANOVA}
\keyword{effect}
\keyword{omega,}
\keyword{size,}
