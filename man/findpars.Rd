\name{findpars}
\alias{findpars}
\alias{findpars.fevd}
\alias{findpars.fevd.bayesian}
\alias{findpars.fevd.lmoments}
\alias{findpars.fevd.mle}
\title{
Get EVD Parameters
}
\description{
Obtain the parameters from an fevd object.  This function differs greatly from distill.
}
\usage{
findpars(x, ...)

\method{findpars}{fevd}(x, ...)

\method{findpars}{fevd.bayesian}(x, burn.in = 499, FUN = "mean",
    use.blocks = FALSE, ..., qcov = NULL)

\method{findpars}{fevd.lmoments}(x, ...)

\method{findpars}{fevd.mle}(x, use.blocks = FALSE, ..., qcov = NULL)

}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.
}
  \item{burn.in}{number giving the burn in value.  The first 1:burn.in will not be used in obtaining parmaeter estiamtes. }
  \item{FUN}{character string naming a function, or a function, to use
    to find the parameter estimates from the MCMC sample.  Default is to
    take the posterior mean (after burn in).}
  \item{use.blocks}{logical: If \code{TRUE} and \code{x} was fit with
    \code{blocks} provided, returns parameters for each block}
  \item{\dots}{
Not used.
}
    \item{qcov}{numeric matrix with rows the same length as \code{q} and columns equal to the number of parameters (+ 1 for the threshold, if a POT model).  This gives any covariate values for a nonstationary model.  If NULL, and model is non-stationary, only the intercept terms for modeled parameters are used, and if a non-constant threshold, only the first threshold value is used.  Not used if model is stationary. }
}
\details{
This function finds the EVD parameters for each value of the covariates in a non-stationary model.  In the case of a stationary model, it will return vectors of length equal to the length of the data that simply repeat the parameter(s) value(s).

Note that this differs greatly from \code{distill}, which simply returns a vector of the length of the number of parameters in the model.  This function returns a named list containing the EVD parameter values possibly for each value of the covariates used to fit the model.  For example, if a GEV(location(t), scale, shape) is fit with location(t) = mu0 + mu1 * t, say, then the \dQuote{location} component of the returned list will have a vector of mu0 + mu1 * t for each value of t used in the model fit.
}
\value{
A list object is returned with components

    \item{location, scale, shape}{vector of parameter values (or NULL if the parameter is not in the model).  For stationary models, or for parameters that are fixed in the otherwise non-stationary model, the vectors will repeat the parameter value.  The length of the vectors equals the length of the data used to fit the models.}
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}, \code{\link{distill}}, \code{\link{parcov.fevd}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

findpars(fit)

\dontrun{
data(PORTw)
fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit

findpars(fit)

}
}
\keyword{ manip }
\keyword{ list }
\keyword{ methods}
