\name{extremalindex}
\alias{extremalindex}
\title{ Estimate the extremal index (theta) }
\description{
Estimates the extremal index based on the intervals estimator due to
Ferro and Segers (2003).
}
\usage{
extremalindex(xdat, u)
}
\arguments{
  \item{xdat}{ n X 1 numeric vector of the data. }
  \item{u}{ User defined threshold.  May be a single number or a numeric n X 1
	vector.}
}
\details{
The extremal index is a useful indicator of how much clustering of exceedances
of a threshold occurs in the limit of the distribution.  If theta = 1, then the
data is independent and if theta < 1, then there is some dependency (clustering)
in the limit.

There are many possible estimators of the extremal index.  The one used here is
the intervals estimator described in Ferro and Segers (2003).  It is unbiased
in the mean and can be used to estimate the number of clusters, which is also
done by this function.
}
\value{ A list with components:
  \item{theta }{The intervals estimate of the extremal index (theta).}
  \item{msg }{A message stating whether any interexceedance times were above 2
	and possibly other pertinent information.}
  \item{C}{The estimated number of clusters (handles ties).}
  \item{run.length}{The estimated run length for runs declustering.}
}
\references{ Ferro, C.A.T. and Segers, J. (2003): Inference for clusters of
	extreme values, J.R. Statist. Soc. B, 65 (2): 545-556. }
\author{ Eric Gilleland }
\seealso{ \code{dclust}}
\examples{
data( Tphap)
temp <- -Tphap[,"MinT"]
look <- extremalindex( temp, -70)
look
}
\keyword{univar}
