\name{exi.intervals}
\alias{exi.intervals}
\title{ Extremal Index Estimator}
\description{
Evaluates the intervals estimator for the extremal index.
}
\usage{
exi.intervals(z, blocks = NULL)
}
\arguments{
  \item{z}{ Logical vector indicating which positions correspond to extreme values. }
  \item{blocks}{vector describing physical blocks across which exceedance times should not be calculated.  For example, if data are taken over only summer across years, then it might not make sense to consider the time between exceedances that cross from one summer to another.  This is passed to the \code{aggregate} function through the \code{by} argument in the form of \code{list(blocks[z])}, so any vector that would work in this matter is accepted.  If NULL, no blocking is done.}
}
\value{
Estimate of the extremal index.
}
\references{
Ferro, C. A. T. and Segers, J. (2003) Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.
}
\author{ Chris Ferro}
\note{ Original code has been modified, and is maintained, by Eric Gilleland.}

\section{Warning }{The estimator is not constrained to lie in [0,1] and a default value of 1 is returned if there are fewer than two extreme values.}

\seealso{ \code{\link{decluster.intervals}}}
\examples{
x <- rnorm(1000)
exi.intervals(x > quantile(x, 0.9))

data(FCwx)
x <- FCwx[,"MxT"]
y <- FCwx[,"Year"]
# Extract just the July and August time points.
id <- is.element(FCwx[,"Mn"],7:8)
x <- x[id]
y <- y[id]
# Does not account for crossing from one year to the next.
exi.intervals(x)

# Takes account of crossing from one year to the next.
exi.intervals(x>93.5, blocks=y)

}
\keyword{ misc }
