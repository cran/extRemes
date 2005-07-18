\name{gen.gev}
\alias{gen.gev}
\alias{gen.gpd}
\title{ Simulate data from a generalized extreme value (GEV)}
\description{
Generates data from a GEV (GPD) using 'runif' (and 'rexp') function.  May also
incorporate a linear trend in location parameter of GEV.
}
\usage{
gen.gev(p, n, trend = NULL)
gen.gpd(n,sigma,xi,u)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{p}{ A $1\times 3$ vector indicating the mean, scale and shape of the
		GEV, respectively. }
  \item{n}{ The sample size to generate.}
  \item{trend}{ Slope of the location parameter trend (if desired). }
  \item{sigma}{ Scale parameter of GPD.}
  \item{xi}{ Shape parameter of GPD.}
  \item{u}{ Threshold for GPD.}
}
\details{
Value returned (with no trend) is derived from the follwing formula (GEV).
$\mu + \sigma \frac{X^{-\xi - 1}}{\xi}$, where $X$ is a uniform random
variable.

For GPD the formula is:
$\frac{\sigma}{\xi}\cdot((1-$runif$(n))^{-\xi-1}$ for $\xi\neq 0$ and
rexp$(n,$ rate=$\frac{1}{\sigma}$ for $\xi = 0$.

}
\value{
Returns a vector of simulated data.
}
\references{
Coles, Stuart.  "An introduction to statistical modeling of extreme values",
 Springer-Verlag (London), 2001.

Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for
     the 'Extremes Toolkit: Weather and Climate Applications of Extreme
     Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.
}
\author{ Functions written by Eric Gilleland and Greg Young.}

\seealso{ From \code{ismev} package: \code{gev.fit}, \code{gev.diag},
		\code{gpd.fit}, \code{gpd.diag}}

\examples{
# obtain a GEV with mean, 4, scale 1.5 and shape of -0.1
mu <- 4 # location parameter
sigma <- 1.5 # scale parameter
xi <- -0.1 # shape parameter

params <- c( mu, sigma, xi)

# generate a sample of size 25
gen1 <- gen.gev( p=params, n=25)

# Now generate one with a trend.
gen2 <- gen.gev( p=params, n=25, trend=0.1)

# Fit 'gen1' to a GEV distribution and plot the diagnostics.
gen1.fit <- gev.fit( gen1)
class( gen1.fit) <- "gev.fit"
plot( gen1.fit)

# Fit 'gen2' to a GEV distribution and plot the diagnostics.
gen2.fit1 <- gev.fit( gen2)
class( gen2.fit1) <- "gev.fit"
plot( gen2.fit1)

}
\keyword{ distribution }
