% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/d.ind.t.t.R
\name{d.ind.t.t}
\alias{d.ind.t.t}
\title{d from t for Between Subjects}
\usage{
d.ind.t.t(t, n1, n2, a = 0.05)
}
\arguments{
\item{t}{t-test value}

\item{n1}{sample size group one}

\item{n2}{sample size group two}

\item{a}{significance level}
}
\value{
Provides the effect size (Cohen's d) with associated confidence intervals,
degrees of freedom, t-statistic, and p-value.

\item{d}{effect size}
\item{dlow}{lower level confidence interval of d value}
\item{dhigh}{upper level confidence interval of d value}
\item{n1}{sample size}
\item{n2}{sample size}
\item{df}{degrees of freedom (n1 - 1 + n2 - 1)}
\item{t}{t-statistic}
\item{p}{p-value}
\item{estimate}{the d statistic and confidence interval in APA style for markdown printing}
\item{statistic}{the t-statistic in APA for the t-test}
}
\description{
This function displays d for between subjects data
and the non-central confidence interval estimating from the t-statistic.
}
\details{
To calculate d, the t-statistic is multiplied by two then divided by
the square root of the degrees of freedom.

     d_s = 2 * t / sqrt(n1 + n2 - 2)

\href{https://www.aggieerin.com/shiny-server/tests/indtt.html}{Learn more on our example page.}
}
\examples{

#The following example is derived from the "indt_data" dataset, included
#in the MOTE library.

#A forensic psychologist conducted a study to examine whether
#being hypnotized during recall affects how well a witness
#can remember facts about an event. Eight participants
#watched a short film of a mock robbery, after which
#each participant was questioned about what he or she had
#seen. The four participants in the experimental group
#were questioned while they were hypnotized. The four
#participants in the control group recieved the same
#questioning without hypnosis.

    hyp = t.test(correctq ~ group, data = indt_data)

#You can type in the numbers directly, or refer to the dataset,
#as shown below.

    d.ind.t.t(t = -2.6599, n1 = 4, n2 = 4, a = .05)

    d.ind.t.t(-2.6599, 4, 4, .05)

    d.ind.t.t(hyp$statistic,
              length(indt_data$group[indt_data$group == 1]),
              length(indt_data$group[indt_data$group == 2]),
              .05)

#Contrary to the hypothesized result, the group that underwent hypnosis were
#significantly less accurate while reporting facts than the control group
#with a large effect size, t(6) = -2.66, p = .038, d_s = 2.17.

}
\keyword{effect}
\keyword{independent}
\keyword{size,}
\keyword{t}
