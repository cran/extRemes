\name{profliker}
\alias{profliker}
\title{
Profile Likelihood Function
}
\description{
Find the profile likelihood for a range of values for an extreme value df (EVD).
}
\usage{
profliker(object, type = c("return.level", "parameter"), xrange = NULL,
    return.period = 100, which.par = 1, nint = 20, plot = TRUE, gr = NULL,
    method = "BFGS", lower = -Inf, upper = Inf, control = list(), ...)
}
\arguments{
  \item{object}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.
}
  \item{type}{
character string stating whether the parameter of interest is a regular parameter or a return level.
}
  \item{xrange}{
numeric vector of length two giving the range of values of the parameter over which to calculate the profile likelihood.
}
  \item{return.period}{
If a return level is of interest, this number gives its associated return period.
}
  \item{which.par}{
If a parameter is of interest, this number tells for which component of the parameter vector to do the profile likelihood.
}
  \item{nint}{
The profile likelihood is calculated for a sequence of \code{nint} values covering \code{xrange}.
}
  \item{plot}{
logical; should a plot of the likelihood be made?  Note that this is controlled by the \code{verbose} argument in the ]code{ci} method function for MLE \code{fevd} objects when \dQuote{proflik} is chosen as the method for finding confidence intervals.  It is usually a good idea to plot the profile likelihood to see if the confidence intervals are really found or not.
}
  \item{gr, method, lower, upper, control}{
optional arguments to \code{optim}.
}
  \item{\dots}{
optional arguments to \code{plot}.
}
}
\details{
See the help file for \code{ci.fevd.mle} for more details on this approach.
}
\value{
A numeric vector is returned invisibly.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{ci.fevd.mle}}, \code{\link{fevd}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

profliker(fit, type="parameter", which.par=3)

profliker(fit, type="parameter",
    xrange=c(-0.35, -0.2), which.par=3)


}
\keyword{ hplot }
\keyword{ distribution }
