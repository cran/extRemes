\name{gev.parameterCI}
\alias{gev.parameterCI}
\alias{gpd.parameterCI}
\title{ Return Level and Shape Parameter Confidence Intervals for GEV (or GP) Distribution. }
\description{
Computes confidence intervals for return levels and/or shape parameters of GEV (or GP) using
the profile likelihood appraoch.
}
\usage{
gev.parameterCI(z, m, rl.xlow=NULL, rl.xup=NULL, xi.xlow=NULL, xi.xup=NULL, conf = 0.95,
    nint = 100, rl.only=FALSE, xi.only=FALSE, make.plot=FALSE)

gpd.parameterCI(z, m, conf = 0.95, nint = 100, rl.xup=NULL, rl.xlow=NULL,
    xi.xup=NULL, xi.xlow=NULL, rl.only=FALSE,
    xi.only=FALSE, make.plot=FALSE)
}
\arguments{
  \item{z}{ object from \code{gev.fit} or \code{gpd.fit} functions of the \code{ismev} package.}
  \item{m}{ m-year return level for which to calculate confidence interval. }
  \item{rl.xlow}{ initial guess of lower limit for return level (generally should be lower than actual limit).  If NULL, this function will make its own guess--it is recommended to plot the profile likelihood when finding these limits automatically (\code{make.plot}=TRUE).}
  \item{rl.xup}{ initial guess of upper limit for return level (generally should be higher than actual limit).  If NULL, this function will make its own guess--it is recommended to plot the profile likelihood when finding these limits automatically (\code{make.plot}=TRUE).}
  \item{xi.xlow}{ analogous to rl.xlow, but for shape parameter. }
  \item{xi.xup}{ analogous to rl.xup, but for shape parameter. }
  \item{conf}{ desired confidence. }
  \item{nint}{ number of values (ret. level and/or shape parameter) to compute in trying to find confidence bounds. }
  \item{rl.only}{ logical, if TRUE calculate only the return level confidence intervals. }
  \item{xi.only}{ logical, if TRUE calculate only the shape parameter confidence intervals. }
  \item{make.plot}{ logical, if TRUE plots profile likelihoods. }
}
\details{
This is a fairly rough routine, and is designed mainly to work internally with the extRemes GUI dialogs.
However, it can be used externally with care.  It makes use of a bisection search and spline fitting in order
to find where the profile likelihood crosses the horizontal line through c; the maximum log-likelihood less the
associated chi-square quantile.
}
\value{ A list with components:
  \item{upcross.level}{ the maximum log-likelihood value less the associated chi-square quantile.}
  \item{rl}{a list object containing the return level characteristics.  Including: the maximum likelihood
		estimate for the m-year return level (mle), the spline function used to estimate the profile
		likelihood (sfun), and the upper (up) and lower (dn) confidence limits.}
  \item{xi}{Shape parameter characteristics--analogous to rl, but no mle component.}
}
\references{
Coles, S. (2001) \emph{An Introduction to Statistical Modeling of Extreme Values}, London: Springer-Verlag, 208pp.

}
\author{ Eric Gilleland }

\seealso{ \code{gev.fit}, \code{gpd.fit}, \code{gev.diag}, \code{gpd.diag}, \code{gev.prof}, \code{gev.profxi},
	\code{gpd.prof}, \code{gpd.profxi}}
\examples{
data(ftcanmax)
fit <- gev.fit( ftcanmax[,"Prec"])
obj <- gev.parameterCI(fit, m=550, xi.xlow=-0.1, xi.xup=0.5, make.plot=TRUE)
}
\keyword{ manip }
\keyword{ hplot }
