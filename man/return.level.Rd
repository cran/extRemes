\name{return.level}
\alias{return.level}
\title{ Computes m-year return levels for GEV and GPD with confidence bounds. }
\description{
Computes confidence limits for m-year return levels for GEV and GPD with
the confidence levels using the ``delta" method.
}
\usage{
return.level(z, conf = 0.05, rperiods= c(10,100,210,510,810,980), make.plot = TRUE)
}
\arguments{
  \item{z}{ An object of class "gev.fit" or "gpd.fit" from the \code{gev.fit} or \code{gpd.fit} functions
	from the \code{ismev} library of Stuart Coles (ported to R by Alec Stephenson).  Note that z
	must be assigned one of the two classes or nothing will happen.}
  \item{conf}{ The confidence level(s) for (delta) confidence bounds. Confidence intervals are not 
		trustworthy for return periods beyond the range of the data.  Check skewness of
		profile likelihood for a given return level--symmetric profile likelihoods imply
		that the delta method is appropriate.}
  \item{rperiods}{ Return periods at which to compute confidence limits.}
  \item{make.plot}{ logical, if 'TRUE' plots the return levels with confidence bounds. }
}
\details{
The function also allows for the plotted values to be returned (invisibly).

Confidence levels are computed using the delta method as described in Coles (2001). 

For the GEV return levels, return periods must be greater than 1 year.
}
\value{
	Creates a plot if 'make.plot' is TRUE and returns a list invisibly with components.
  \item{return.level}{ The m-year return levels.}
  \item{return.period}{ The return periods, m.}
  \item{confidence.delta}{The confidence bounds computed by the delta method.}
  \item{conf.level}{The confidence level.}
}
\references{
Coles, Stuart.  "An Introduction to Statistical Modeling of Extreme Values",
 Springer 2001.  ISBN: 1852334592

Gilleland, Eric and Katz, Richard W. Tutorial for the 'Extremes Toolkit:
   Weather and Climate Applications of Extreme Value Statistics.'
   \url{http://www.assessment.ucar.edu/toolkit}, 2005.
}
\author{ Eric Gilleland }

\seealso{ \code{gevrlgradient}, \code{gev.effective.rl}, From the \code{ismev} package: \code{gev.fit}, \code{gpd.fit}, \code{gev.diag}, \code{gpd.diag}, \code{gev.rl}, \code{gpd.rl}}

\examples{
# Must have the 'ismev' package loaded.
require( ismev)
data( ftcanmax)
fit <- gev.fit( ftcanmax[,"Prec"])
class( fit) <- "gev.fit" # 'gev.fit' does not actually assign this class,
			 # but it must be done.
return.level( fit)
}
\keyword{ hplot }
