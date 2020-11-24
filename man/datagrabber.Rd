\name{datagrabber.declustered}
\alias{datagrabber.declustered}
\alias{datagrabber.extremalindex}
\alias{datagrabber.fevd}
\title{
Get Original Data from an R Object
}
\description{
Get the original data set used to obtain the resulting R object for which a method function exists.
}
\usage{
\method{datagrabber}{declustered}(x, ...)

\method{datagrabber}{extremalindex}(x, ...)

\method{datagrabber}{fevd}(x, response = TRUE,
    cov.data = TRUE, ...)
}
\arguments{
  \item{x}{
An R object that has a method function for \code{datagrabber}.
}
   \item{response, cov.data}{logical; should the response data be returned?  Should the covariate data be returned?}
  \item{\dots}{
optional arguments to \code{get}.  This may eventually become deprecated as scoping gets mixed up, and is currently not actually used.
}
}
\details{

Accesses the original data set from a fitted \code{fevd} object or from declustered data (objects of class \dQuote{declustered}) or from \code{extremalindex}.

}
\value{
The original pertinent data in whatever form it takes.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{datagrabber}}, \code{\link{extremalindex}}, \code{\link{decluster}}, \code{\link{fevd}}, \code{\link{get}}
}
\examples{
y <- rnorm(100, mean=40, sd=20)
y <- apply(cbind(y[1:99], y[2:100]), 1, max)
bl <- rep(1:3, each=33)

ydc <- decluster(y, quantile(y, probs=c(0.95)), r=1, blocks=bl)

yorig <- datagrabber(ydc)
all(y - yorig == 0)


}
\keyword{ data }
\keyword{ manip }
