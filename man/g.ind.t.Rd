% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/g.ind.t.R
\name{g.ind.t}
\alias{g.ind.t}
\title{d-g Corrected for Independent t}
\usage{
g.ind.t(m1, m2, sd1, sd2, n1, n2, a = 0.05)
}
\arguments{
\item{m1}{mean group one}

\item{m2}{mean group two}

\item{sd1}{standard deviation group one}

\item{sd2}{standard deviation group two}

\item{n1}{sample size group one}

\item{n2}{sample size group two}

\item{a}{significance level}
}
\value{
D-g corrected with associated confidence intervals,
the confidence intervals associated with the means of each group,
standard deviations of the means for each group, relevant statistics.

\item{d}{d-g corrected effect size}
\item{dlow}{lower level confidence interval d-g corrected}
\item{dhigh}{upper level confidence interval d-g corrected}
\item{M1}{mean group one}
\item{sd1}{standard deviation of group one}
\item{se1}{standard error of group one}
\item{M1low}{lower level confidence interval of mean one}
\item{M1high}{upper level confidence interval of mean one}
\item{M2}{mean two}
\item{sd2}{standard deviation of mean two}
\item{se1}{standard error of mean two}
\item{M2low}{lower level confidence interval of mean two}
\item{M2high}{upper level confidence interval of mean two}
\item{spooled}{pooled standard deviation}
\item{sepooled}{pooled standard error}
\item{correction}{g corrected}
\item{n1}{size of sample one}
\item{n2}{size of sample two}
\item{df}{degrees of freedom}
\item{t}{t-statistic}
\item{p}{p-value}
\item{estimate}{the d statistic and confidence interval in
APA style for markdown printing}
\item{statistic}{the t-statistic in APA style for markdown printing}
}
\description{
This function displays d-g corrected
and the non-central confidence interval for independent t.
}
\details{
The correction is calculated by dividing three by the sum of both
sample sizes after multiplying by four and subtracting nine.
This amount is deducted from one.

     correction = 1 - (3 / (4 * (n1 + n2) - 9))

D-g corrected is calculated by substracting mean two from mean one,
dividing by the pooled standard deviation which is multiplied
by the correction above.

     d_g corrected = ((m1 - m2) / spooled) * correction

\href{https://www.aggieerin.com/shiny-server/tests/indtg.html}{Learn more on our example page.}
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

    t.test(correctq ~ group, data = indt_data)

#You can type in the numbers directly, or refer to the dataset,
#as shown below.

    g.ind.t(m1 = 17.75, m2 = 23, sd1 = 3.30,
           sd2 = 2.16, n1 = 4, n2 = 4, a = .05)

    g.ind.t(17.75, 23, 3.30, 2.16, 4, 4, .05)

    g.ind.t(mean(indt_data$correctq[indt_data$group == 1]),
            mean(indt_data$correctq[indt_data$group == 2]),
            sd(indt_data$correctq[indt_data$group == 1]),
            sd(indt_data$correctq[indt_data$group == 2]),
            length(indt_data$correctq[indt_data$group == 1]),
            length(indt_data$correctq[indt_data$group == 2]),
            .05)

#Contrary to the hypothesized result, the group that underwent hypnosis were
#significantly less accurate while reporting facts than the control group
#with a large effect size, t(6) = -2.66, p = .038, d_g = 1.64.

}
\keyword{correction}
\keyword{effect}
\keyword{independent}
\keyword{size,}
\keyword{t,}
