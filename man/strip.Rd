\name{strip}
\alias{strip}
\alias{strip.fevd}
\title{
Strip Fitted EVD Object of Everything but the Parameter Estimates
}
\description{
Take any fevd object, regardless of estimation method, and return only a vector of the estimated parameters.
}
\usage{
strip(x, use.names = TRUE, ...)
\method{strip}{fevd}(x, use.names = TRUE, ...)
}
\arguments{
  \item{x}{
An object of class \dQuote{fevd}.
}
  \item{use.names}{
logical stating whether or not to keep the names attribute
}
  \item{\dots}{
For the Bayesian method, if an alternative function to taking the mean or posterior mode of the MCMC samples is used, then optional arguments may be passed.  Otherwise, not used.
}
}
\details{
This function is very similar to \code{distill}, but returns less information.  
}
\value{
numeric vector with the parameter estimates.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{distill.fevd}}
}
\examples{

z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

strip( fit )
strip( fit, use.names = FALSE )

# Compare with ...
distill( fit )
distill( fit, cov = FALSE )

\dontrun{
data( "Fort" )
fit <- fevd(Prec, Fort, threshold=0.395,
    scale.fun=~sin(2 * pi * (year - 1900)/365.25) + 
        cos(2 * pi * (year - 1900)/365.25),
    type="PP", method="Bayesian", iter=1999, use.phi=TRUE, verbose=TRUE)

fit

strip( fit )
strip( fit, burn.in = 700 )
strip( fit, FUN = "postmode" )

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
\keyword{ attribute }
