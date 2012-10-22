\name{make.qcov}
\alias{make.qcov}
\alias{is.qcov}
\title{
Covariate Matrix for Non-Stationary EVD Projections
}
\description{
Create a matrix for use with \code{pextRemes}.
}
\usage{
make.qcov(x, vals, nr = 1, ...)

is.qcov(x)
}
\arguments{
  \item{x}{
\code{make.qcov}: A list object of class \dQuote{fevd} as output from \code{fevd}.

\code{is.qcov}: Any R object.
}
  \item{vals}{
Either a named list whose names match the fitted model parameter names, or may be \dQuote{threshold}, a matrix or a numeric vector of length equal to the size of the resulting matrix.
}
  \item{nr}{
The number of rows desired in the resulting matrix.  Only if \code{vals} is a vector.  If \code{vals} argument is not a vector, the code will either fail or the argument will be ignored.
}
\item{\dots}{optional arguments to \code{matrix} (e.g., byrow=TRUE, depending on the order for \code{vals}, if a vector).  Only used if \code{vals} is a vector.}
}
\details{
Simply sets up a matrix of parameter coefficients to be used by \code{pextRemes}.  In particular, all parameters/thresholds that are constant (i.e., do not depend on covariate values) should have columns of all ones.  Paremters/threshold that vary in a non-stationary model may have whatever values are of interest.

\code{is.qcov} performs some very simple tests to determine if an object is a proper \code{qcov} matrix or not.  It is possible to have a matrix that is not a proper \code{qcov} matrix, but the returned value is TRUE.  It is also possible to have a valid \code{qcov} object that id not appropriate for a particular model.  Mostly this is an internal function.
}
\value{
An nr by np + 1 matrix is returned, where np is the number of parameters in the model.  The last column is always \dQuote{threshold} even if the model does not take a threshold (e.g., the GEV df), in which case the last column may be all NA, 0, or some other value depending on the vals argument.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{pextRemes}}, \code{\link{fevd}}, \code{\link{erlevd}}
}
\examples{
data(PORTw)
fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit

v <- cbind(rep(1,4), c(1, -1, 1, -1), rep(1,4), rep(1,4))
v <- make.qcov(fit, vals=v, nr=4)
v

# cf.
v <- make.qcov(fit, vals=list(mu1=c(1, -1, 1, -1)))
v

# Or
v <- make.qcov(fit, vals=c(rep(1,4), c(1, -1, 1, -1), rep(1,8), rep(0,4)), nr=4)
v

}
\keyword{ array }
\keyword{ misc }
