\name{parcov.fevd}
\alias{parcov.fevd}
\title{
EVD Parameter Covariance
}
\description{
Try to calculate the parameter covariance for an extreme value distribution (EVD) fitted using MLE.
}
\usage{
parcov.fevd(x)
}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.
}
}
\details{
Makes possibly two calls to \code{optimHess} in an effort to find the parameter covariance matrix for fitted EVDs where MLE is used.  The first attempt uses the actual gradient of the negative log-likelihood.  If this fails, or the Hessian matrix cannot be inverted, or there are any negative values along the diagonal in the inverted Hessian, then a second attempt is made using finite differences.  See Coles (2001) sec. 2.6.4 for more details.
}
\value{
An np by np matrix is returned where np is the number of parameters in the model.
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{summary.fevd}}, \code{\link{print.fevd}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

parcov.fevd(fit)
}
\keyword{ array }
\keyword{ distribution }
\keyword{ optimize }
