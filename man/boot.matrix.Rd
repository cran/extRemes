\name{boot.matrix}
\alias{boot.matrix}
\title{ Boot Matrices. }
\description{
Set up matrices for bootstrapping sequences of extreme values.
}
\usage{
boot.matrix(x, y)
}
\arguments{
  \item{x}{ Output from \code{decluster.runs} or \code{decluster.intervals}. }
  \item{y}{ Vector of observations. }
}
\details{
This function merely formats the information needed by \code{boot.sequence} to improve efficiency.
}
\value{
	A list containing two (unamed) matrices, each with columns corresponding to clusters identified in 'x.'
  \item{comp1 }{inter-exceedance times}
  \item{comp2 }{Data values in 'y' corresponding to the exceedances in each cluster.}
}
\author{ Chris Ferro, C.A.T.Ferro \dQuote{at} exeter.ac.uk }
\note{ Maintained by Eric Gilleland. }

\seealso{ \code{\link{boot.sequence}}, \code{\link{decluster.intervals}}, \code{\link{decluster.runs}} }
\examples{
# Simulate 1000 uniform random variables.
x <- runif(1000)

# Perform runs declustering with run length = 1 and 90th percentile as threshold.
u <- quantile(x, 0.9)
z <- x > u
dec <- decluster.runs(z, 1)

# Make sure estimated run length is not zero before doing the rest.
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
} # end of if run length estimate not zero stmt.
}
\keyword{ manip }% at least one, from doc/KEYWORDS
