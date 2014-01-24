\name{extremalindex}
\alias{extremalindex}
\alias{ci.extremalindex}
\alias{print.extremalindex}
\title{
Extemal Index
}
\description{
Estimate the extremal index.
}
\usage{
extremalindex(x, threshold, method = c("intervals", "runs"), run.length = 1,
    na.action = na.fail, ...)

\method{ci}{extremalindex}(x, alpha = 0.05, R = 502, return.samples = FALSE, ...)

\method{print}{extremalindex}(x, ...)
}
\arguments{
  \item{x}{
A data vector.

\code{ci} and \code{print}: output from \code{extremalindex}.
}
  \item{threshold}{
numeric of length one or the length of \code{x} giving the value above which (non-inclusive) the extremal index should be calculated.
}
  \item{method}{
character string stating which method should be used to estimate the extremal index.
}
  \item{run.length}{
For runs declustering only, an integer giving the number of threshold deficits to be considered as starting a new cluster.
}
  \item{na.action}{
funciton to handle missing values.
}
  \item{alpha}{number between zero and one giving the (1 - alpha) * 100 percent confidence level.  For example, alpha = 0.05 corresponds to 95 percent confidence; alpha is the significance level (or probability of type I errors) for hypothesis tests based on the CIs.}
   \item{R}{Number of replicate samples to use in the bootstrap procedure.}
   \item{return.samples}{logical; if TRUE, the bootstrap replicate samples will be returned instead of CIs.  This is useful, for example, if one wishes to find CIs using a better method than the one used here (percentile method).}
  \item{\dots}{
optional arguments to \code{decluster}.  Not used by \code{ci} or \code{print}.
}
}
\details{
The extremal index is a useful indicator of how much clustering of exceedances of a threshold occurs in the limit of the distribution.  For independent data, theta = 1, (though the converse is does not hold) and if theta < 1, then there is some dependency (clustering) in the limit.

There are many possible estimators of the extremal index.  The ones used here are runs declustering (e.g., Coles, 2001 sec. 5.3.2) and the intervals estimator described in Ferro and Segers (2003).  It is unbiased in the mean and can be used to estimate the number of clusters, which is also done by this function.
}
\value{
A numeric vector of length three and class \dQuote{extremalindex} is returned giving the estimated extremal index, the number of clusters and the run length.  Also has attributes including:

  \item{cluster }{the resulting clusters.}
  \item{method }{Same as argument above.}
  \item{data.name}{character vector giving the name of the data used, and possibly the data frame or matrix and column name, if applicable. }
  \item{data.call}{character string giving the actual argument passed in for x.  May be the same as data.name.}
  \item{call}{the function call.}
  \item{na.action}{function used for handling missing values.  Same as argument above.}
  \item{threshold}{the threshold used.}
  
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Ferro, C. A. T. and Segers, J. (2003). Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{decluster}}, \code{\link{fevd}}
}
\examples{
data(Fort)

extremalindex(Fort$Prec, 0.395, method="runs", run.length=9, blocks=Fort$year)

\dontrun{
tmp <- extremalindex(Fort$Prec, 0.395, method="runs", run.length=9, blocks=Fort$year)
tmp
ci(tmp)

tmp <- extremalindex(Fort$Prec, 0.395, method="intervals", run.length=9, blocks=Fort$year)
tmp
ci(tmp)

}
}
\keyword{ univar }
