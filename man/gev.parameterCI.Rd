\name{gev.parameterCI}
\alias{gev.parameterCI}
\alias{gpd.parameterCI}
\title{ Return level and shape parameter confidence intervals for GEV (or GP) distribution. }
\description{
Computes confidence intervals for return levels and/or shape parameters of GEV (or GP) using
the profile likelihood appraoch.
}
\usage{
gev.parameterCI(z, m, rl.xlow, rl.xup, xi.xlow = NULL, xi.xup = NULL, conf = 0.95, nint = 100, rl.only = FALSE, xi.only = FALSE, make.plot = FALSE)

gpd.parameterCI(z, m, rl.xlow, rl.xup, xi.xlow = NULL, xi.xup = NULL,
    npy = 365, conf = 0.95, nint = 100, rl.only = FALSE, xi.only = FALSE,
    make.plot = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{z}{ object from \code{gev.fit} or \code{gpd.fit} functions of the \code{ismev} package.}
  \item{m}{ m-year return level to calculate conf. interval for. }
  \item{rl.xlow}{ initial guess of lower limit for return level (generally should be lower than actual limit). }
  \item{rl.xup}{ initial guess of upper limit for return level (generally should be higher than actual limit). }
  \item{xi.xlow}{ analogous to rl.xlow, but for shape parameter. }
  \item{xi.xup}{ analogous to rl.xup, but for shape parameter. }
  \item{npy}{for GP distn. this is the number of obs. per year.}
  \item{conf}{ Desired confidence. }
  \item{nint}{ Number of values (ret. level and/or shape parameter) to compute confidence for.  }
  \item{rl.only}{ logical if TRUE calculate only the return level confidence intervals. }
  \item{xi.only}{ logical if TRUE calculate only the shape parameter confidence intervals. }
  \item{make.plot}{ logical if TRUE plots profile likelihoods. }
}
\details{
This is a fairly rough routine, and is designed mainly to work internally with the extRemes GUI dialogs.
However, it can be used externally with care.  It makes use of a bisection search and spline fitting in order
to find where the profile likelihood crosses the horizontal line through the associated chi-square quantile.
}
\value{
A list object with components:
  \item{upcross.level }{the horizontal lines associated with the chi-square quantile.}
  \item{rl}{a list object containing the return level characteristics.  Including: the maximum likelihood
		estimate for the m-year return level (mle), the spline function used to estimate the profile
		likelihood (sfun), and the upper (up) and lower (dn) confidence limits.}
  \item{xi}{Shape parameter characteristics--analogous to rl, but no mle component.}
}
\references{ Coles, Stuart (2001).  An Introduction to Statistical Modeling of
        Extreme Values.  Springer-Verlag, London.}
\author{ Eric Gilleland }

\seealso{ \code{\link[ismev]{gev.fit}}, \code{\link[ismev]{gpd.fit}},
	\code{\link[ismev]{gev.diag}}, \code{\link[ismev]{gpd.diag}},
	\code{\link[ismev]{gev.prof}}, \code{\link[ismev]{gev.profxi}},
	\code{\link[ismev]{gpd.prof}}, \code{\link[ismev]{gpd.profxi}}}
\examples{
# See extRemes tutorial for examples using the GUI interface.
}
\keyword{ manip }
\keyword{ hplot }
