\name{boot.sequence}
\alias{boot.sequence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Bootstrap a sequence. }
\description{
Bootstrap a sequence of exceedances.
}
\usage{
boot.sequence(tmat, ymat, u)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tmat}{ Output from \code{boot.matrix} ('iet' component). }
  \item{ymat}{ Output from \code{boot.matrix} ('ce' component. }
  \item{u}{ Threshold above which the sequences of exceedances are to be bootstrapped.}
}
\details{
The bootstrapped sequence contains the same number of clusters
as identified in the original data. Clusters of exceedances
and inter-exceedance times within clusters are resampled as
single entities; inter-exceedance times between clusters are
resampled independently. The values of non-exceedances are set
equal to the threshold `u'.
}
\value{
	Vector of exceedances above 'u'.
}
\references{ Ferro CAT and Segers J. (2003). Inference for clusters of extreme values.  Journal of
   the Royal Statistical Society B 65:545--556.}
\author{ Chris Ferro }
\note{ Maintained by Eric Gilleland. }

\section{Warning }{The bootstrapped sequence can be longer than the original sequence.}

\seealso{ \code{\link{boot.matrix}}}
\examples{
# Simulate 1000 uniform random variables.
x <- runif(1000)

# Perform runs declustering with run length = 1 and 90th percentile as threshold.
u <- quantile(x, 0.9)
z <- x > u
dec <- decluster.runs(z, 1)

# Make sure the estimated run length is not zero before doing the rest.
if( dec[["par"]] != 0) {
   # Set up the matrices for bootstrapping.
   mat <- boot.matrix(dec, x)
   # Bootstrap with 500 iterations.
   eib <- numeric(500)
   for( i in 1:500) {
           set.seed(i)
           zb <- boot.sequence(mat[[1]],mat[[2]],u) > u
           eib[i] <- exi.intervals(zb)
           } # end of for 'i' loop.
   # Obtain bootstrapped 95th percentile confidence intervals.
   conf.int <- quantile( eib, c((1-0.95)/2,(1+0.95)/2))
} # end of if run length not zero stmt.
}
\keyword{ misc }% at least one, from doc/KEYWORDS
