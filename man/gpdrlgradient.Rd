\name{gpdrlgradient}
\alias{gpdrlgradient}
\title{ Gradient for the GPD Return Level}
\description{
Calculate the gradient for the GPD return level for computing delta method confidence intervals.
}
\usage{
gpdrlgradient(z, m)
}
\arguments{
  \item{z}{ a list object output from the \pkg{ismev} function \code{gpd.fit} }
  \item{m}{ the N*npy return period (i.e., the actual return period for the N-year return level).  May be a vector. }
}
\details{
Does not allow covariates in the parameter estimates.  See Coles (2001) section 4.3.3, p. 82 for more information.
}
\value{
a 3 by np matrix whose columns give the (three) gradient values for each return period.
}
\references{
Coles, S. (2001) \emph{An Introduction to Statistical Modeling of Extreme Values}. London: Springer-Verlag, 208pp. ISBN: 1852334592.
}
\author{ Eric Gilleland }
\seealso{ \code{\link{gevrlgradient}}, From the \code{ismev} package: \code{gpd.fit}, \code{gpd.diag} }
\examples{
data(rain)
fit <- gpd.fit(rain, 10)
gpdrlgradient( fit, m=c(10*fit$npy, 20*fit$npy, 50*fit$npy, 100*fit$npy))
}
\keyword{ distribution }
