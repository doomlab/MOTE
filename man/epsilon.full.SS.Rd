% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/epsilon.full.SS.R
\name{epsilon.full.SS}
\alias{epsilon.full.SS}
\title{Epsilon for ANOVA from F and Sum of Squares}
\usage{
epsilon.full.SS(dfm, dfe, msm, mse, sst, a = 0.05)
}
\arguments{
\item{dfm}{degrees of freedom for the model/IV/between}

\item{dfe}{degrees of freedom for the error/residual/within}

\item{msm}{mean square for the model/IV/between}

\item{mse}{mean square for the error/residual/within}

\item{sst}{sum of squares total}

\item{a}{significance level}
}
\value{
Provides the effect size (epsilon) with associated
confidence intervals from the F-statistic.

\item{epsilon}{effect size}
\item{epsilonlow}{lower level confidence interval of epsilon}
\item{epsilonhigh}{upper level confidence interval of epsilon}
\item{dfm}{degrees of freedom for the model/IV/between}
\item{dfe}{degrees of freedom for the error/residual/within}
\item{F}{F-statistic}
\item{p}{p-value}
\item{estimate}{the epsilon statistic and confidence interval in
APA style for markdown printing}
\item{statistic}{the F-statistic in APA style for markdown printing}
}
\description{
This function displays epsilon squared from ANOVA analyses
and its non-central confidence interval based on the F distribution.
This formula works for one way and multi way designs with careful
focus on the sum of squares total calculation.
}
\details{
To calculate epsilon, first, the mean square for the error is
substracted from the mean square for the model. The difference
is multiplied by the degrees of freedom for the model. The
product is divided by the sum of squares total.

     epsilon^2 = (dfm * (msm - mse)) / (sst)

\href{https://www.aggieerin.com/shiny-server/tests/epsilon.html}{Learn more on our example page.}
}
\examples{

#The following example is derived from the "bn1_data" dataset, included
#in the MOTE library.

#A health psychologist recorded the number of close inter-personal
#attachments of 45-year-olds who were in excellent, fair, or poor
#health. People in the Excellent Health group had 4, 3, 2, and 3
#close attachments; people in the Fair Health group had 3, 5,
#and 8 close attachments; and people in the Poor Health group
#had 3, 1, 0, and 2 close attachments.

anova_model = lm(formula = friends ~ group, data = bn1_data)
summary.aov(anova_model)

epsilon.full.SS(dfm = 2, dfe = 8, msm = 12.621,
                mse = 2.458, sst = (25.24+19.67), a = .05)
}
\keyword{ANOVA}
\keyword{effect}
\keyword{epsilon,}
\keyword{size,}
