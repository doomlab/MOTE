% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d.dep.t.diff.t.R
\name{d.dep.t.diff.t}
\alias{d.dep.t.diff.t}
\title{d from t for Repeated Measures with SD Difference Scores Denominator}
\usage{
d.dep.t.diff.t(t, n, a = 0.05)
}
\arguments{
\item{t}{t-test value}

\item{n}{sample size}

\item{a}{significance level}
}
\value{
\item{d}{effect size}
\item{dlow}{lower level confidence interval d value}
\item{dhigh}{upper level confidence interval d value}
\item{n}{sample size}
\item{df}{degrees of freedom (sample size - 1)}
\item{p}{p-value}
\item{estimate}{the d statistic and confidence interval in APA
style for markdown printing}
\item{statistic}{the t-statistic in APA style for markdown printing}
}
\description{
This function displays d for repeated measures data
and the non-central confidence interval using the
standard deviation of the differences as the denominator
estimating from the t-statistic.
}
\details{
To calculate d, the t-statistic is divided by the square root of the sample size.

     d_z = t / sqrt(n)

\href{https://www.aggieerin.com/shiny-server/tests/deptdifft.html}{Learn more on our example page.}
}
\examples{

#The following example is derived from the "dept_data" dataset included
#in the MOTE library.

#In a study to test the effects of science fiction movies on people’s belief
#in the supernatural, seven people completed a measure of belief in
#the supernatural before and after watching a popular science
#fiction movie. Higher scores indicated higher levels of belief.

    scifi = t.test(dept_data$before, dept_data$after, paired = TRUE)

#The t-test value was 1.43. You can type in the numbers directly,
#or refer to the dataset, as shown below.

    d.dep.t.diff.t(t = 1.43, n = 7, a = .05)

    d.dep.t.diff.t(1.43, 7, .05)

    d.dep.t.diff.t(scifi$statistic, length(dept_data$before), .05)

#The mean measure of belief on the pretest was 5.57, with a standard
#deviation of 1.99. The posttest scores appeared lower (M = 4.43, SD = 2.88)
#but the dependent t-test was not significant using alpha = .05,
#t(7) = 1.43, p = .203, d_z = 0.54. The effect size was a medium effect suggesting
#that the movie may have influenced belief in the supernatural.
}
\keyword{dependent}
\keyword{effect}
\keyword{measures,}
\keyword{paired}
\keyword{repeated}
\keyword{sample,}
\keyword{size,}
\keyword{t-test}
\keyword{t-test,}
