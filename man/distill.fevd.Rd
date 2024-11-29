\name{distill.fevd}
\alias{distill.fevd}
\alias{distill.fevd.bayesian}
\alias{distill.fevd.lmoments}
\alias{distill.fevd.mle}

\title{

Distill Parameter Information

}
\description{
Distill parameter information (and possibly other pertinent inforamtion) from fevd objects.
}
\usage{
\method{distill}{fevd}(x, ...)

\method{distill}{fevd.bayesian}(x, cov = TRUE, FUN = "mean", burn.in = 499, ...)

\method{distill}{fevd.lmoments}(x, ...)

\method{distill}{fevd.mle}(x, cov = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{

list object returned by \code{fevd}.

}
  \item{\dots}{

Not used.

}

  \item{cov}{logical; should the parameter covariance be returned with the parameters (if TRUE, they are returned as a vector concatenated to the end of the returned value).}

  \item{FUN}{character string naming a function to use to estimate the parameters from the MCMC sample.  The function is applied to each column of the \code{results} component of the returned \code{fevd} object.}

\item{burn.in}{The first \code{burn.in} values are thrown out before calculating anything from the MCMC sample.}

}
\details{

Obtaining just the basic information from the fits:

\code{distill}: The \code{distill} method function works on \code{fevd} output to obtain only pertinent information and output it in a very user-friendly format (i.e., a single vector).  Mostly, this simply means returning the parameter estimates, but for some methods, more information (e.g., the optimized negative log-likelihood value and parameter covariances) can also be returned.  In the case of the parameter covariances (returned if \code{cov} = TRUE), if np is the number of parameters in the model, the covariance matrix can be obtained by peeling off the last np^2 values of the vector, call it v, and using v <- matrix(v, np, np).

As with \code{ci}, only \code{distill} need be called by the user.  The appropriate choice of the other functions is automatically determined from the \code{fevd} fitted object.

}
\value{

numeric vector giving the parameter values, and if estimation method is MLE/GMLE, then the negative log-likelihood.  If the estimation method is MLE/GMLE or Bayesian, then the parameter covariance values (collapsed with \code{c}) are concatenated to the end as well.

}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{ci.fevd}}, \code{\link[distillery:distill]{distillery::distill}}
}
\examples{

data(Fort)

fit <- fevd(Prec, Fort, threshold=0.395, type="PP", units="inches", verbose=TRUE)
fit

distill(fit)

}
\keyword{ manip }
