\name{return.level}
\alias{return.level}
\title{ Computes m-year return levels for GEV and GPD with confidence bounds. }
\description{
Computes confidence limits for m-year return levels for GEV and GPD using
profile likelihoods and a cubic spline ('splinefun').  It also computes
the confidence levels using the ``delta" method.
}
\usage{
return.level(z, conf = 0.05, rlevels= c(10,100,210,510,810,980), make.plot = TRUE)
}
\arguments{
  \item{z}{ An object of class "gev.fit" or "gpd.fit" from the \code{gev.fit} or \code{gpd.fit} functions
	from the \code{ismev} library of Stuart Coles (ported to R by Alec Stephenson).}
  \item{conf}{ The confidence level for confidence bounds. }
  \item{rlevels}{ Return levels at which to compute confidence limits with
	profile likelihood plots--the rest will be interpolated with a spline function
	using the R function \code{splinefun}}
  \item{make.plot}{ logical, if 'TRUE' plots the return levels with confidence bounds. }
}
\details{
This function is in part a modification of the functions 'gev.rl' and 'gpd.rl'
from the ismev library.  It allows for the plotted values to be returned
(invisibly) as well as other confidence levels besides 97.5\%.

Confidence levels are computed using both the profile likelihood method (for
return levels given by 'rlevels') with a cubic spline (to interpolate for
many values not given by 'rlevels') and by the delta method described in Coles
(2001). 
}
\value{
	Creates a plot if 'make.plot' is TRUE and returns a list invisibly with components.
  \item{return.level}{ The m-year return levels.}
  \item{return.period}{ The return periods, m.}
  \item{confidence}{ The confidence bounds estimated from the profile likelihood.}
  \item{confidence.delta}{The confidence bounds computed by the delta method.}
  \item{conf.level}{The confidence level.}
}
\references{ Coles, Stuart.  "An Introduction to Statistical Modeling of Extreme Values",
	Springer 2001.  ISBN: 1852334592}
\author{ Eric Gilleland }

\seealso{ From the \code{ismev} package: \code{gev.fit}, \code{gpd.fit}, \code{gev.diag}, \code{gpd.diag},
	\code{gev.rl}, \code{gpd.rl}}

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
