\name{gevrlgradient}
\alias{gevrlgradient}
\title{ GEV Return Level Function Gradient }
\description{
Calculates the gradient of the GEV return level function for computing the GEV return level confidence intervals by way of the delta method
}
\usage{
gevrlgradient(z, p)
}
\arguments{
  \item{z}{ list object output from the \code{gev.fit} function from \pkg{ismev}.}
  \item{p}{ Vector of 1/p return periods (e.g., if block maxima are annual maxima, and the 20-year return level is desired,
		then use p=1/20). }
}
\details{
Calculates the gradient of the return levels for the GEV df (see, e.g., Coles, 2001 section 3.3.3, pp. 56 - 57).
}
\value{
A 3 by np matrix whose columns give the (three) gradient values for each return period.
}
\references{
Coles, S. (2001) \emph{An Introduction to Statistical Modeling of Extreme Values}. London: Springer-Verlag, 208pp. ISBN: 1852334592.
}
\author{ Eric Gilleland }
\seealso{ \code{\link{return.level}}, From the \code{ismev} package: \code{gev.fit}, \code{gev.diag} }
\examples{
data(portpirie)
fit <- gev.fit(portpirie[,2])
gevrlgradient( fit, p=c(1/10, 1/20, 1/50, 1/100, 1/1000))
return.level( fit, conf=0.05, rperiods=c(10, 20, 50, 100, 1000))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
