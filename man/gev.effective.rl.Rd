\name{gev.effective.rl}
\alias{gev.effective.rl}
\alias{gpd.effective.rl}
\title{
   Cacluate the Effective Return Levels for a Non-Stationary GEV (or GPD) Model.
}
\description{
   For a GEV model whose parameters change according to covariates, this function will calculate the \dQuote{effective} return levels for particular values of the covariates.
}
\usage{
gev.effective.rl(models, data, return.period = 100, plot = TRUE, ...)
gpd.effective.rl(models, data, return.period = 100, plot = TRUE, ...)
}
\arguments{
  \item{models}{
A list giving functions (or constants) for each of the parameter models for the GEV or GP df.  If the location parameter is a function of covariates, then there must be a component called \dQuote{location}, if the scale parameter is a function of covariates, then there must be a component called \dQuote{scale}, and similarly, if the shape parameter is a function of covariates, then there must be a component called \dQuote{shape}.

For the GP df, if the threshold is a function of the covariates, then models must have a component called \code{threshold}, and if the probability of exceeding the trheshold is a function of covariates, then there should be a component named \code{lambda}.  These components should be functions, and they should take \code{x} and \code{\dots} as arguments.  The argument \code{x} is a data frame, and the following argument \code{data} is used for \code{x} to evaluate the functions (i.e., this is the dataframe that has the covariate information).

If the location parameter is not a function of covariates (i.e., constant), then it may be ommitted from the \code{models} list object, as long as its value is passed by way of \code{\dots} as \code{mu}.  Similarly for the scale and shape parameters, whose values should be passed by way of \code{\dots} as \code{sigma} and \code{xi}, resp.  For the GP df, constant thresholds and rate aprameters may also be passed in this way as \code{u} and \code{lambda}, resp.  Alternatively, they may be included in 'models' as functions that return a numeric vector of the same length as the first dimension of the covariate data frame, repeating the constant value as to fill the vector.  Any parameter values utilized in the \code{models} functions must also be passed in by way of \code{\dots}
}
  \item{data}{
	An n by m data frame object whose names are used within the functions supplied by \code{models}.  These are the covariate values.
}
  \item{return.period}{
A single numeric value (i.e., of length=1), which (for the GEV df) must be greater than 1, giving the desired return levels in years.  For example, to obtain effective medians for each value of the covariate (or combination of covariates), this should be 2.  For the effective 100-year return levels, this should be 100, and so on.  For the GP df, this value must incorporate the number of points per year (i.e., this transformation is not done automatically by the function).  For example, if the model is based on daily observations throughout the entire year, and the 100-year effective return level is desired, then this value should be 100*365.25.  Similarly, if the model is based only on say 60 values per year, then the 100-year effective return period should be 100*60.
}
  \item{plot}{
logical, if TRUE a plot showing the effective return levels against 1 to n.  This plot may or may not be very useful.
}
  \item{\dots}{
Optional arguments to any and all of the functions contained in the \code{models} list apart from the actual data frame of covariates, which must be contained in the \code{data} argument.  May also contain \code{mu}, \code{sigma} and \code{xi} (as well as \code{u} and \code{lambda} for the GP df) if the location, scale, and shape parameters are not modeled as functions of the covariates; in which case they should be single numeric values (i.e., each of length 1). 
}
}
\details{
Suppose a non-stationary GEV df is fit to a data sample with the location parameter as a function of time.  Then, the model fit is GEV( mu(t), sigma, xi).  This function will take values of t, compute the effective GEV model for each value of t, and calculate the return level associated with \code{return.period} for each instance.  See Gilleland and Katz (2011) for an example.
}
\value{
A numeric vector giving the n effective return levels is returned invisibly.  Optionally, a plot of these values against 1 to n is created.
}
\references{
Gilleland, E. and Katz, R. W. (2011) New software to analyze how extremes change over time.  \emph{Eos}, \bold{92}, (2), 13--14.
}
\author{
Eric Gilleland
}

\seealso{ \code{gevrlgradient}, \code{return.level}, From the \code{ismev} package: \code{gev.fit}, \code{gpd.fit}, \code{gev.diag}, \code{gpd.diag}, \code{gev.rl}, \code{gpd.rl} }
\examples{
data( Tphap)
# Find the annual minimum of daily minimum temperatures at Phoenix Sky Harbor airport.
MinT <- aggregate( Tphap$MinT, by=list(Tphap$Year), min, na.rm=TRUE)$x

# Make two vectors of the years, one (Yr) for human readability, and one (yr) for including as a covariate
# in the lcoation parameter of a GEV df.
Yr   <- 1900 + unique( Tphap$Year)
yr <- 1:length(Yr)
ycov <- matrix( yr, ncol=1)

# Fit a GEV( mu(t) = mu0 + mu1*t, t=1,2,... (years), sigma, xi) to the negative annual min. temperatures.
fit <- gev.fit( -MinT, ydat=ycov, mul=1)

# Make a function representing the location parameter model.
hold <- function(x, ...) {
    a <- list(...)
    out <- a$mu0 + a$mu1*x$yr
    return( out)
} # end of 'hold' function.

# Make a list to pass to 'gev.effective.rl'.
hold <- list( location=hold)

# Find the effective medians for this GEV model.
look <- gev.effective.rl( models=hold, data=data.frame( yr=yr), return.period=2,
    mu0=fit$mle[1], mu1=fit$mle[2], sigma=fit$mle[3], xi=fit$mle[4])
# Note the medians are all negative because the model is for the negative annual minima.

# Make a nicer looking plot.
plot( Yr, MinT, type="l", lwd=2, col="darkblue", xlab="Year", ylab="Minimum temperature (deg. F)",
        main="Phoenix summer minimum temperature",
        cex.main=1.5, cex.lab=1.5, cex.axis=1.5,
        col.lab="darkblue", col.axis="darkblue", col.main="darkblue")

lines( Yr, -look, col="red", lty=2, lwd=2)

##
## Now an example using the GP df.
##
data(FtCoPrec)
prec <- FtCoPrec[,"Prec"]

ind <- prec > 0.395
t1 <- sin(2*pi*(1:length(prec))/365.25)
t2 <- cos(2*pi*(1:length(prec))/365.25)
ycov <- cbind( t1, t2)

## Fit annual cycle in Poisson rate parameter (orthogonal approach).
lamfit <- glm( ind~t1+t2, family=poisson())
lamcoeffs <- summary( lamfit)$coefficients[,1]

## Fit a GP df to the data with the 't1' and 't2' harmonics as covariates.
## Note that model is log sigma(t1,t2) = sig0 + sig1*sin( 2pi t/T) + sig2*cos( 2pi t/T).
GPfit <- gpd.fit( prec, 0.395, ydat=ycov, sigl=c(1,2), siglink=exp)

scale.fun <- function( x, ...) {
    a <- list(...)
    out <- exp( a$sig0 + a$sig1*x$t1 + a$sig2*x$t2)
    return( out)
} # end of 'scale.fun' internal function.

rate.fun <- function( x, ...) {
    a <- list(...)
    out <- exp( a$lam0 + a$lam1*x$t1 + a$lam2*x$t2)
    return( out)
} # end of 'rate.fun' internal function.

hold <- list( scale=scale.fun, lambda=rate.fun)
# Find effective 100-year return levels for each value of the harmonics used in the fit.
look <- gpd.effective.rl( models=hold, data=data.frame( t1=t1, t2=t2), return.period=100*365.25,
            plot=FALSE,
            sig0=GPfit$mle[1], sig1=GPfit$mle[2], sig2=GPfit$mle[3],
            xi=GPfit$mle[4], u=0.395, lam0=lamcoeffs[1], lam1=lamcoeffs[2],
            lam2=lamcoeffs[3])

# Look at 1945.
id <- FtCoPrec[,"year"] == 1945
n <- sum( id)
plot( 1:n, look[ id], pch="+", xlab="time (days)",
    main="Fort Collins precipitation (year 1945, inches)",
    ylab="100-year Effective Return Level", col="blue")
lines( 1:n, look[id], lty=3, col="blue")
}
\keyword{ hplot }
