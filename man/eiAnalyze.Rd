\name{eiAnalyze}
\alias{eiAnalyze}
\title{Extremal Index Analysis}
\description{
Estimate the extremal index (and confidence limits) using intervals estimation.
}
\usage{
eiAnalyze(x, thresholds = quantile(x, seq(0.9, 0.995, by = 0.005)),
    conf = 0.95, iter = 500, plot=FALSE)
}
\arguments{
  \item{x}{ n by 1 vector of data observations. }
  \item{thresholds}{ Single number, or m by 1 vector of thresholds above which to estimate the extremal index.}
  \item{conf}{ The \code{conf}*100 percent confidece level.}
  \item{iter}{ Number of iterations desired for bootstrapping. }
  \item{plot}{ Logical whether or not to plot the extremal indices against the thresholds (w/ confidence bounds).  Only used if the length of \code{thresholds} is greater than 1.}
}
\details{
Uses several functions written by Chris Ferro for estimating the extremal index by intervals estimation
(Ferro and Segers (2003)).  Specifically, it calls the functions: \code{exi.intervals}, \code{decluster.intervals},
\code{boot.matrix} and \code{boot.sequence}.
}
\value{
	Returns a list with the following components.
  \item{ei }{m by 1 vector of the estimate(s) of the extremal index.}
  \item{ci }{An m by 2 matrix giving the \code{conf}*100 percent confidence limits for \code{ei}.}
  \item{u }{The threshold(s) used.}
  \item{nc}{An n by 1 vector giving the number of clusters for each threshold choice.}
  \item{run.length}{An m by 1 vector giving the estimated run lengths.}
}
\references{
Ferro, C. A. T. and Segers, J. (2003) Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.
}
\author{ Chris Ferro and Eric Gilleland }

\note{Maintained by Eric Gilleland.}

\seealso{ \code{\link{exi.intervals}}, \code{\link{decluster.intervals}}, \code{\link{boot.matrix}}, \code{\link{boot.sequence}} }
\examples{
data(ftcanmax)
eiAnalyze(ftcanmax$Prec, thresholds=0.1)
}
\keyword{ misc }
