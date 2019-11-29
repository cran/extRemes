\name{bvpotbooter}
\alias{bvpotbooter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Bootstrap Functions for Bivariate POT
}
\description{
	Obtains bootstrap samples for the bivariate POT model as output from fbvpot.
}
\usage{
bvpotbooter(object, B, rsize, block.length = 1, shuffle = NULL, replace = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
	  A list object of class \dQuote{fbvpot}.
}
  \item{B}{
	  integer giving the number of bootstrap resamples to obtain.
}
  \item{rsize}{
	  integer giving the size of the bootstrap samples.  Defaults to the (original) sample size.  For heavy-tailed data, may want to use a smaller size (e.g., square root of the original sample size).
}
  \item{block.length}{
	  integer giving the length of blocks to use (for dependent data).  Default is unity, which yields IID samples.
}
  \item{shuffle}{
	  (optional) \code{rsize} by \code{B} matrix giving the indices for the bootstrap resamples.  If provided, \code{B} may be missing.
}
  \item{replace}{ logical, should the resamples be taken with replacement?
}
  \item{\dots}{ Not used.
}
}
\details{
	Performs bootstrap resampling for bivariate POT inference.  CI's are obtained by calling the ci function.  See the help file for booter from the distillery package for more details.
}
\value{

	A list object of class \dQuote{booted}.
  \item{call }{the function call}
  \item{data }{original data series}
  \item{statistic, B, block.length, replace, rsize}{same arguments as passed in}
  \item{results}{B column matrix giving the paramter estimates for each bootstrap resample.}
  \item{type}{character stating whether the resample method is iid or cbb.}
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fbvpot}}, \code{\link{booter}}, \code{\link{ci}}
}
\examples{
data( "SantaAna" )

Z <- SantaAna[,3:4]

mfit1 <- fevd( x = temp, data = Z, threshold = 36.75, type = "GP" )
mfit2 <- fevd( x = windspeeds, data = Z, threshold = 7.09875, type = "GP" )

fit2 <- fbvpot( x = Z, threshold = apply( Z, 2, quantile, probs = 0.95 ),
	                      tform = "tf", fit = list( mfit1, mfit2 ) )

# Note, using B = 50 below for R check speed, but probably need
# a higher number.
fit2booted <- bvpotbooter( fit2, B = 50, rsize = 62, block.length = 7 )
summary( fit2booted )
ci( fit2booted, type = "perc" )

}
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{ multivariate }% __ONLY ONE__ keyword per line
