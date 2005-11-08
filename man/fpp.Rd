\name{fpp}
\alias{fpp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Point Process }
\description{
Fit data to a point process model (allows for covariates in each of the parameters).
}
\usage{
fpp(xdat, threshold, npy = 365, ydat = NULL, mul = NULL, sigl = NULL, shl = NULL, mulink = identity, siglink = identity, shlink = identity, show = TRUE, method = "Nelder-Mead", maxit = 10000, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xdat}{ 'n X 1' vector of observed data. }
  \item{threshold}{  The threshold; a single number or a numeric 'n X 1' vector. }
  \item{npy}{ Number of points/observations per year. }
  \item{ydat}{ Optional 'n X p' matrix of covariate information. }
  \item{mul,sigl,shl}{ Numeric vectors of integers, giving the columns of
          'ydat' that contain covariates for generalized linear
          modelling of the location, scale and shape parameters
          repectively (or 'NULL' (the default) if the corresponding
          parameter is stationary).}
  \item{mulink,siglink,shlink}{ Inverse link functions for generalized linear
          modelling of the location, scale and shape parameters
          repectively. }
  \item{show}{ Logical; if 'TRUE' (the default), print details of the fit. }
  \item{method}{ The optimization method (see 'optim' for details).}
  \item{maxit}{ The maximum number of iterations. }
  \item{\dots}{ Other control parameters for the optimization. These are
          passed to components of the 'control' argument of 'optim'. }
}
\details{
For non-stationary fitting it is recommended that the covariates
     within the generalized linear models are (at least approximately)
     centered and scaled (i.e. the columns of 'ydat' should be
     approximately centered and scaled).

This function is a modification of the \pkg{ismev} function \code{pp.fit},
but uses the estimation $\sum_i \lambda_i$ instead of $\sum_i \lambda_i\cdot I_{x > u}$
(where $\lambda$ is the exceedance rate and $u$ the threshold) for the rate approximation
of the point process likelihood.
}
\value{
 A list containing the following components. A subset of these
     components are printed after the fit. If 'show' is 'TRUE', then
     assuming that successful convergence is indicated, the components
     'nexc', 'nllh', 'mle' and 'se' are always printed.

   trans: An logical indicator for a non-stationary fit.

   model: A list with components 'mul', 'sigl' and 'shl'.

    link: A character vector giving inverse link functions.

threshold: The threshold, or vector of thresholds.

     npy: The number of observations per year/block.

    nexc: The number of data points above the threshold.

    data: The data that lie above the threshold. For non-stationary
          models, the data is standardized.

    conv: The convergence code, taken from the list returned by
          'optim'. A zero indicates successful convergence.

    nllh: The negative logarithm of the likelihood evaluated at the
          maximum likelihood estimates.

    vals: A matrix with four columns containing the maximum likelihood
          estimates of the location, scale and shape parameters, and
          the threshold, at each data point.
 model: A list with components 'mul', 'sigl' and 'shl'.

    link: A character vector giving inverse link functions.

threshold: The threshold, or vector of thresholds.

     npy: The number of observations per year/block.

    nexc: The number of data points above the threshold.

    data: The data that lie above the threshold. For non-stationary
          models, the data is standardized.

    conv: The convergence code, taken from the list returned by
          'optim'. A zero indicates successful convergence.

    nllh: The negative logarithm of the likelihood evaluated at the
          maximum likelihood estimates.

    vals: A matrix with four columns containing the maximum likelihood
          estimates of the location, scale and shape parameters, and
          the threshold, at each data point.

     gpd: A matrix with three rows containing the maximum likelihood
          estimates of corresponding GPD location, scale and shape
          parameters at each data point.

     mle: A vector containing the maximum likelihood estimates.

     cov: The covariance matrix.

      se: A vector containing the standard errors.
}
\references{ 
Beirlant J, Goegebeur Y, Segers J and Teugels J. (2004). Statistics of Extremes, Wiley,
  Chichester, England.

Coles, Stuart (2001).  An Introduction to Statistical Modeling of
 Extreme Values.  Springer-Verlag, London. }
\author{ Eric Gilleland }
\note{ This is adapted from code originally written for S-Plus by Stuart Coles, and ported to R by Alec Stephenson. See
details section above.}

\section{Warning }{Different optimization methods may result in wildly different parameter estimates.}

\seealso{ \code{pp.fit}, \code{pp.diag}, \code{\link{optim}}, \code{pp.fitrange}, \code{mrl.plot}, \code{gpd.fit} }
\examples{
# load Fort Collins, CO precipitation dataset.
data(FtCoPrec)

# Perform a simple point process model fit.
x <- FtCoPrec[,"Prec"]
fit <- fpp( x, 0.395)
pp.diag( fit)

# Add seasonal covariates.
Time <- FtCoPrec[,"obs"]
angle <- (2*pi*Time)/365.25
s <- cbind( sin( angle), cos( angle))
fit <- fpp( xdat=x, threshold=0.395, npy=365.25, ydat=s, mul=1:2, sigl=1:2, siglink=exp)
pp.diag( fit)
}
\keyword{ misc }% at least one, from doc/KEYWORDS
