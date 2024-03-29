% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eta.F.R
\name{eta.F}
\alias{eta.F}
\title{Eta and Coefficient of Determination (R2) for ANOVA from F}
\usage{
eta.F(dfm, dfe, Fvalue, a = 0.05)
}
\arguments{
\item{dfm}{degrees of freedom for the model/IV/between}

\item{dfe}{degrees of freedom for the error/residual/within}

\item{Fvalue}{F statistic}

\item{a}{significance level}
}
\value{
Provides eta with associated confidence intervals and relevant statistics.

\item{eta}{effect size}
\item{etalow}{lower level confidence interval of eta}
\item{etahigh}{upper level confidence interval of eta}
\item{dfm}{degrees of freedom for the model/IV/between}
\item{dfe}{degrees of freedom for the error/resisual/within}
\item{F}{F-statistic}
\item{p}{p-value}
\item{estimate}{the eta squared statistic and confidence interval in
APA style for markdown printing}
\item{statistic}{the F-statistic in APA style for markdown printing}
}
\description{
This function displays eta squared from ANOVA analyses
and their non-central confidence interval based on the F distribution.
These values are calculated directly from F statistics and can be used
for between subjects and repeated measures designs.
Remember if you have two or more IVs, these values are partial eta squared.
}
\details{
Eta is calculated by multiplying the degrees of freedom of
the model by the F-statistic. This is divided by the product
of degrees of freedom of the model, the F-statistic, and
the degrees of freedom for the error or residual.

     eta^2 = (dfm * Fvalue) / (dfm * Fvalue + dfe)

\href{https://www.aggieerin.com/shiny-server/tests/etaf.html}{Learn more on our example page.}
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

eta.F(dfm = 2, dfe = 8,
      Fvalue = 5.134, a = .05)
}
\keyword{ANOVA}
\keyword{effect}
\keyword{eta,}
\keyword{size,}
