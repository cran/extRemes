\name{gevrlgradient}
\alias{gevrlgradient}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ GEV return level function gradient. }
\description{
Calculates the gradient of the GEV return level function for computing
the GEV return level confidence intervals by way of the delta method
(e.g., Coles, 2001 section 3.3.3, pp. 56-57)
}
\usage{
gevrlgradient(z, p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ list object output from the \code{gev.fit} function from \pkg{ismev}.}
  \item{p}{ Vector of 1/p return periods (e.g., if block maxima are annual maxima, and the 20-year return level is desired,
		then use p=1/20). }
}
\value{
A '3 X np' matrix whose columns give the (three) gradient values for each return period.
}
\references{ Coles S, 2001 (2nd prt, 2003).  An Introduction to Statistical Modeling of Extreme Values. Springer-Verlag, London. 208pp. ISBN: 1852334592 }
\author{ Eric Gilleland }
\seealso{ \code{\link{return.level}}, From the \code{ismev} package: \code{gev.fit}, \code{gev.diag} }
\examples{
data(portpirie)
fit <- gev.fit(portpirie[,2])
gevrlgradient( fit, p=c(1/10, 1/20, 1/50, 1/100, 1/1000))
class( fit) <- "gev.fit"
return.level( fit, conf=0.05, rperiods=c(10, 20, 50, 100, 1000))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
