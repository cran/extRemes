\name{eiAnalyze}
\alias{eiAnalyze}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extremal index analysis}
\description{
Estimate the extremal index (and confidence limits) using intervals estimation.
}
\usage{
eiAnalyze(x, thresholds = quantile(x, seq(0.9, 0.995, by = 0.005)), conf = 0.95, iter = 500, plot=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ 'n X 1' vector of data observations. }
  \item{thresholds}{ Single number, or 'm X 1' vector of thresholds above which to estimate the extremal index.}
  \item{conf}{ The \code{conf}*100 percent confidece level.}
  \item{iter}{ Number of iterations desired for bootstrapping. }
  \item{plot}{ Logical whether or not to plot the extremal indices against the thresholds (w/ confidence bounds).
		Only used if the length of 'thresholds' is greater than 1.}
}
\details{
Uses several functions written by Chris Ferro for estimating the extremal index by intervals estimation
(Ferro and Segers (2003)).  Specifically, it calls the functions: \code{exi.intervals}, \code{decluster.intervals},
\code{boot.matrix} and \code{boot.sequence}.
}
\value{
	Returns a list with the following components.
  \item{ei }{'m X 1' vector of the estimate(s) of the extremal index.}
  \item{ci }{An 'm X 2' matrix giving the \code{conf}*100 percent confidence limits for \code{ei}.}
  \item{u }{The threshold(s) used.}
  \item{nc}{An 'n X 1' vector giving the number of clusters for each threshold choice.}
  \item{run.length}{An 'm X 1' vector giving the estimated run lengths.}
}
\references{ Ferro CAT and Segers J (2003) Inference for clusters of extreme
  values. Journal of the Royal Statistical Society B 65, 545-556. }
\author{ Chris Ferro and Eric Gilleland }

\note{Maintained by Eric Gilleland.}

\seealso{ \code{\link{exi.intervals}}, \code{\link{decluster.intervals}}, \code{\link{boot.matrix}}, \code{\link{boot.sequence}} }
\examples{
## The function is currently defined as
# function(x, thresholds=quantile(x, seq(0.900,0.995,by=0.005)), conf=0.95, iter=500)
}
\keyword{ misc }% at least one, from doc/KEYWORDS
