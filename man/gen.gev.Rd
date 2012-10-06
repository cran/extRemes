\name{gen.gev}
\alias{gen.gev}
\alias{gen.gpd}
\title{ Simulate Data from a Generalized Extreme Value (GEV) Distribution}
\description{
Generates data from a GEV (GPD) distribution function.  May also incorporate a linear trend in location parameter of GEV.
}
\usage{
gen.gev(p, n, trend = NULL)
gen.gpd(n,sigma,xi,u)
}
\arguments{
  \item{p}{ A length 3 vector indicating the mean, scale and shape of the GEV, respectively. }
  \item{n}{ The sample size to generate.}
  \item{trend}{ Slope of the location parameter trend (if desired). }
  \item{sigma}{ Scale parameter of GPD.}
  \item{xi}{ Shape parameter of GPD.}
  \item{u}{ Threshold for GPD.}
}
\details{
Value returned (with no trend) is derived from the follwing formula (GEV).

mu + sigma (X**(-xi - 1))*xi,

where X is a uniform random variable.

For GPD the formula is:

sigma/xi * ((1-runif(n))**(-xi-1) for xi != 0 and

rexp(n, rate=1/sigma) for xi = 0.

}
\value{
Returns a vector of simulated data.
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag.

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
