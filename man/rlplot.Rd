\name{rlplot}
\alias{rlplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a return level plot for a fitted object of an extreme-value distribution. }
\description{
Plots several return levels against the return period for a fitted object from one of
the \pkg{ismev} functions: \code{gev.fit} and \code{gpd.fit}.
}
\usage{
rlplot(z, ci = 0.05, add.ci = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ A list object as returned by one of gev.fit or gpd.fit (with appropriate class attribute added). }
  \item{ci}{ The (1-ci)*100 confidence value.}
  \item{add.ci}{ logical if true will add confidence bounds to plot.}
}
\details{
Given a fitted list object from \code{gev.fit} or \code{gpd.fit}--attributed with the class
"gev.fit" or "gpd.fit", respectively--the return level plot is generated.  Confidence bounds,
if included, are found by the delta method, which may be wildly inappropriate because the
return level distribution is generally skewed.  Therefore, if a plot with better estimates of
the confidence bounds are desired, use \code{add.ci=FALSE}, and use the R function \code{lines}
to add different bounds (e.g., using values obtained from the \code{gev.parameterCI} or
\code{gpd.parameterCI} functions).

This function is simply a modification of Stuart Coles' functions \code{gpd.rl} and \code{gev.rl} (Coles, 2001).
}
\value{
No value returned.  A plot is created.
}
\references{
Coles, Stuart.  "An introduction to statistical modeling of extreme values",
 Springer-Verlag (London), 2001.

Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for
     the 'Extremes Toolkit: Weather and Climate Applications of Extreme
     Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.
}
\author{ Eric Gilleland }

\seealso{ \code{\link{gev.parameterCI}}, \code{\link{gpd.parameterCI}}, \code{gev.diag}, \code{gpd.diag}}
\examples{
data(ftcanmax)
fit <- gev.fit( ftcanmax[,"Prec"])
class( fit) <- "gev.fit"
rlplot( fit)
}
\keyword{ hplot }
