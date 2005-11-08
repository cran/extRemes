\name{decluster.runs}
\alias{decluster.runs}
\alias{decluster.intervals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Declustering Extremes}
\description{
Performs runs/intervals declustering.
}
\usage{
decluster.runs(z, r)
decluster.intervals(z, ei)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ Logical vector indicating which positions correspond to extreme values. }
  \item{r}{ Integer run length. }
  \item{ei}{ Estimate of the extremal index. }
}
\details{
Runs declustering:
Extremes separated by fewer than `r' non-extremes belong
to the same cluster. Setting `r' < 1 causes each extreme
to form a separate cluster.

Intervals declustering:
Extremes separated by fewer than `r' non-extremes belong
to the same cluster, where `r' is the `nc'-th largest
interexceedance time and `nc', the number of clusters,
is estimated from the extremal index, `ei', and the times
between extremes. Setting `ei' = 1 causes each extreme to
form a separate cluster.
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
}
\references{ Smith RL (1989) Extreme value analysis of environmental time
  series: an application to trend detection in ground-level ozone.
  Statistical Science 4, 367-393.

 Ferro CAT and Segers J (2003) Inference for clusters of extreme
  values. Journal of the Royal Statistical Society B 65, 545-556.}
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
}
\keyword{ misc }% at least one, from doc/KEYWORDS
