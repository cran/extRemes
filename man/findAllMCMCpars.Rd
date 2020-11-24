\name{findAllMCMCpars}
\alias{findAllMCMCpars}
\title{
Manipulate MCMC Output from fevd Objects
}
\description{
Manipulates the MCMC sample from an \dQuote{fevd} object to be in a unified format that can be used in other function calls.
}
\usage{
findAllMCMCpars(x, burn.in = 499, qcov = NULL, ...)
}
\arguments{
  \item{x}{
Object of class \dQuote{fevd} with component \code{method} = \dQuote{Bayesian}.
}
  \item{burn.in}{
Burn in period.
}
  \item{qcov}{
Matrix giving specific covariate values.  See 'make.qcov' for more details.  If not suplied, original covariates are used.
}
  \item{\dots}{
Not used.
}
}
\details{
This function was first constructed for use by \code{postmode}, but might be useful in other areas as well.  It evaluates any parameters that vary according to covariates at the values supplied by \code{qcov} or else at the covariate values used to obtain the original fit (default).  If a model does not contain one or more parameters (e.g., the GP does not have a location component), then a column with these values (set to zero) are returned.  That is, a matrix with columns corresponding to location, scale, shape and threshold are returned regardless of the model fit so that subsequent calls to functions like \code{fevd} can be made more easily.

This function is intended more as an internal function, but may still be useful to end users.

This function is very similar to \code{findpars}, but is only for MCMC samples and returns the entire MCMC sample of parameters.  Also, returns a matrix instead of a list.
}
\value{
A matrix is returned whose rows correspond to the MCMC samples (less burn in), and whose columns are \dQuote{location} (if no location parameter is in the model, this column is still given with all values identical to zero), \dQuote{scale}, \dQuote{shape} and \dQuote{threshold}.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{findpars}}, \code{\link{postmode}}
}
\keyword{ manip }
