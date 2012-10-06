\name{decluster.runs}
\alias{decluster.runs}
\alias{decluster.intervals}
\title{ Declustering Extremes}
\description{
Performs runs/intervals declustering.
}
\usage{
decluster.runs(z, r, blocks = NULL)
decluster.intervals(z, ei, blocks = NULL)
}
\arguments{
  \item{z}{ Logical vector indicating which positions correspond to extreme values. }
  \item{r}{ Integer run length. }
  \item{ei}{ Estimate of the extremal index. }
  \item{blocks}{numeric of length \code{z} giving blocks across which inter-exceedance times should not be computed.  For example, if data cover only summer months across several years, it may not make sense to allow interexceedance times to carry over across years.}
}
\details{
Runs declustering:
Extremes separated by fewer than \code{r} non-extremes belong to the same cluster. Setting \code{r} < 1 causes each extreme to form a separate cluster.

Intervals declustering:
Extremes separated by fewer than \code{r} non-extremes belong to the same cluster, where \code{r} is the nc-th largest interexceedance time and nc, the number of clusters, is estimated from the extremal index, \code{ei}, and the times between extremes. Setting \code{ei} = 1 causes each extreme to form a separate cluster.
}
\value{
A list containing
  \item{scheme }{Name of declustering scheme.}
  \item{par }{Value of declustering parameter (i.e., run length).}
  \item{nc}{Number of clusters.}
  \item{size}{Vector of cluster sizes.}
  \item{s}{Vector of times of extremes.}
  \item{cluster}{Vector of numbers identifying clusters to which extremes belong.}
  \item{t}{Vector of times between extremes.}
  \item{inter}{Vector of intercluster time indicators (logical).}
  \item{intra}{Vector of intracluster time indicators (logical).}
  \item{blocks}{Same as the argument passed in.}
}
\references{
Smith, R. L. (1989) Extreme value analysis of environmental time series: an application to trend detection in ground-level ozone.  \emph{Statistical Science}, \bold{4}, 367--393.

 Ferro, C. A. T. and Segers, J. (2003) Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.
}
\author{ Chris Ferro }
\note{ Maintained by Eric Gilleland. }

\seealso{ \code{\link{exi.intervals}}}
\examples{
# Simulate a dependent series of random variables.
x <- runif(1000,-1,1)
x[2:1000] <- x[1:999]*0.6
# -- DON'T RUN
# pacf( x)

# use runs and intervals declustering using the 90th percentile as the threshold.
u <- quantile(x, 0.9)
z <- x > u
exi.intervals(z)
tmp1 <- decluster.runs(z, 1)
tmp2 <- decluster.intervals( z, exi.intervals(z))

data(FCwx)
x <- FCwx$MxT # Fort Collins, Colorado daily maximum temperature series (degrees Fahrenheit).
y <- FCwx$Year # corresponding years.
id <- is.element(FCwx$Mn,7:8) # July and August time points.
x <- x[id]
y <- y[id]
exi.intervals(x>93.5, blocks=y)
tmp1 <- decluster.runs(x>93.5, 1, blocks=y)
tmp2 <- decluster.intervals(x>93.5, 0.555, blocks=y)
}
\keyword{ misc }
