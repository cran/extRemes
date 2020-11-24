\name{revtrans.evd}
\alias{revtrans.evd}
\title{
Reverse Transformation
}
\description{
Reverse transform standardized data to follow a non-standardized extreme value distribution (EVD).
}
\usage{
revtrans.evd(z, threshold = NULL, location = NULL, scale, shape = NULL,
    type = c("GEV", "GP", "PP", "Gumbel", "Weibull", "Frechet",
    "Exponential", "Beta", "Pareto"))
}
\arguments{
  \item{z}{
numeric vector of data of length n following a standardized EVD.
}
  \item{threshold}{
number or numeric vector of length n giving the threshold, if applicable.
}
  \item{location}{
number or numeric vector of length n giving the location parameter(s), if applicable.
}
  \item{scale}{
number or or numeric vector of length n giving the scale parameter(s).
}
  \item{shape}{
number or numeric vector of length n giving the shape parameter(s), if applicable.
}
  \item{type}{
character string naming to what EVD should the data be reverse-transformed.  Default is GEV df.
}
}
\details{
For standardized EVD data (e.g., via \code{trans}), this function performs the reverse transformation back to the original scale.
}
\value{
numeric vector of length n.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{trans}}, \code{\link{trans.fevd}}, \code{\link{fevd}}
}
\examples{
data(PORTw)

fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit

z <- trans(fit)

fevd(z)

p <- findpars(fit)

y <- revtrans.evd(z=z, location=p$location, scale=2.6809613,
    shape=-0.1812824)

fevd(y)

qqplot(y, PORTw$TMX1)


}
\keyword{ distribution }
\keyword{ manip }
