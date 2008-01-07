\name{gpdrlgradient}
\alias{gpdrlgradient}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Calculate the gradient for the GPD return level}
\description{
Calculate the gradient for the GPD return level for computing delta method confidence intervals.
}
\usage{
gpdrlgradient(z, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ a list object output from the \pkg{ismev} function \code{gpd.fit} }
  \item{m}{ the 'N*npy' return period (i.e., the actual return period for the 'N-yr' return level).  May be a vector. }
}
\details{
Does not allow covariates in the parameter estimates.  See Coles (2001) section 4.3.3, p. 82 for more information.
}
\value{
a '3 X np' matrix whose columns give the (three) gradient values for each return period.
}
\references{Coles S, 2001 (2nd prt, 2003).  An Introduction to Statistical Modeling of Extreme Values. Springer-Verlag, London. 208pp. ISBN: 1852334592}
\author{ Eric Gilleland }
\seealso{ \code{\link{gevrlgradient}}, From the \code{ismev} package: \code{gpd.fit}, \code{gpd.diag} }
\examples{
data(rain)
fit <- gpd.fit(rain, 10)
gpdrlgradient( fit, m=c(10*fit$npy, 20*fit$npy, 50*fit$npy, 100*fit$npy))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }
