\name{postmode}
\alias{postmode}
\alias{postmode.fevd}
\title{
Posterior Mode from an MCMC Sample
}
\description{
Calculate the posterior mode from an MCMC sample for \dQuote{fevd} objects.
}
\usage{
postmode(x, burn.in = 499, verbose = FALSE, ...)

\method{postmode}{fevd}(x, burn.in = 499, verbose = FALSE, ...)

}
\arguments{
  \item{x}{
An object of class \dQuote{fevd} where component \code{method} = \dQuote{Bayesian}.}

  \item{burn.in}{
The furst burn.in samples from the posterior distribution will be removed before calculation.
}
  \item{verbose}{
logical, should progress information be printed to the screen.
}
  \item{\dots}{
Not used.
}
}
\details{
The log-likelihood and (log) prior is calculated for every sample from the chain, and added together, giving \code{h}.  The parameters from the sample that yield the maximum value for \code{h} are returned.  If more than one set of parameters yield a maximum, their average is returned.
}
\value{
A named numeric vector is returned giving the paramter values.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}, \code{\link{findpars}}
}
\examples{
data(ftcanmax)

fit <- fevd(Prec, ftcanmax, method="Bayesian", iter = 1000, verbose=TRUE)

postmode(fit)

}
\keyword{ methods }
\keyword{ optimize }
