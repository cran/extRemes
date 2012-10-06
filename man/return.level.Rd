\name{return.level}
\alias{return.level}
\title{ Compute m-year Return Levels for GEV and GPD Distributions with Confidence Bounds }
\description{
Computes confidence limits for m-year return levels for GEV and GPD with the confidence levels using the delta method.
}
\usage{
return.level(z, conf = 0.05, rperiods= c(10,100,210,510,810,980), make.plot = TRUE)
}
\arguments{
  \item{z}{ An object of class \dQuote{gev.fit}, \dQuote{gum.fit}, or \dQuote{gpd.fit} from the \code{gev.fit}, \code{gum.fit}, or \code{gpd.fit} functions
	from \pkg{ismev}.}
  \item{conf}{ The confidence level(s) for (delta) confidence bounds. Confidence intervals are not trustworthy for return periods beyond the range of the data.  Check skewness of profile likelihood for a given return level--symmetric profile likelihoods imply that the delta method is appropriate.}
  \item{rperiods}{ Return periods at which to compute confidence limits.}
  \item{make.plot}{ logical, if TRUE plots the return levels with confidence bounds. }
}
\details{
The function also allows for the plotted values to be returned (invisibly).

Confidence levels are computed using the delta method as described in Coles (2001). 

For the GEV return levels, return periods must be greater than 1 year.
}
\value{
	Creates a plot if make.plot is TRUE and returns a list invisibly with components.
  \item{return.level}{ The m-year return levels.}
  \item{return.period}{ The return periods, m.}
  \item{confidence.delta}{The confidence bounds computed by the delta method.}
  \item{conf.level}{The confidence level.}
}
\references{
Coles, Stuart.  \emph{An Introduction to Statistical Modeling of Extreme Values}, Springer 2001.  ISBN: 1852334592

Gilleland, E. and Katz, R. W. (2011) New software to analyze how extremes change over time.  \emph{Eos}, \bold{92}, (20), 13--14.
}
\author{ Eric Gilleland }

\seealso{ \code{gevrlgradient}, \code{gev.effective.rl}, From the \code{ismev} package: \code{gev.fit}, \code{gpd.fit}, \code{gev.diag}, \code{gpd.diag}, \code{gev.rl}, \code{gpd.rl}}

\examples{
data( ftcanmax)
fit <- gev.fit( ftcanmax[,"Prec"])
return.level(fit)
}
\keyword{ hplot }
