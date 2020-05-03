\name{is.fixedfevd}
\alias{is.fixedfevd}
\alias{check.constant}
\title{
Stationary Fitted Model Check
}
\description{
Test if a fitted \code{fevd} object is stationary or not.
}
\usage{
is.fixedfevd(x)

check.constant(x)
}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.

For \code{check.constant}, this may be a formula or vector.
}
}
\details{
This function is mostly intended as an internal function, but it may be useful generally.

\code{check.constant} determines if a formula is given simply by ~ 1.  It is used by \code{is.fixedfevd}.
}
\value{
logical of length one stating whether the fitted model is stationary (TRUE) or not (FALSE).
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

is.fixedfevd(fit)

}
\keyword{ logic }
\keyword{ misc }
