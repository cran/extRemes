\name{trans}
\alias{trans}
\alias{trans.fevd}
\title{
Transform Data
}
\description{
Method function to transform a data set.  In the case of \code{fevd} objects, the transformation is to a standardized Gumbel or exponential scale.
}
\usage{
trans(object, ...)

\method{trans}{fevd}(object, ..., burn.in = 499, return.all = FALSE)
}
\arguments{
  \item{object}{
An R object with a \code{trans} method.  In the case of \dQuote{fevd} objects, output from \code{fevd}.
}
   \item{burn.in}{number giving the burn in value.  The first 1:burn.in will not be used in obtaining parmaeter estiamtes. }
    \item{return.all}{logical, only for POT models, but primarily for use with the Point Process model.  Should only the threshold exceedances be returned?}
  \item{\dots}{
Not used.
}
}
\details{
Many important situations occur in extreme value analysis (EVA) where it is useful or necessary to transform data to a standardized scale.  For example, when investigating multivariate or conditional EVA much of the theory revolves around first transfroming the data to a unit scale.  Further, for non-stationary models, it can be useful to transform the data to a df that does not depend on the covariates.

The present function transforms data taken from \dQuote{fevd} class objects and transforms them to either a standard Gumbel (GEV, Gumbel case) or standard exponential (GP, PP, exponential case) df.  In the first case, if the data are Gumbel distributed (really, if a gumbel fit was performed) the transformation is:

    z = (x - location(yi))/scale(yi),

where yi represent possible covariate terms and z is distributed according to a Gumbel(0, 1) df.  If the data are GEV distributed, then the transformation is:

    z = - log(1 + (shape(yi)/scale(yi) * (x - location(yi)))^(-1/shape(yi))),

and again z is distributed Gumbel(0, 1).

In the case of exponentially distributed data, the transformation is:

    z = (x - threshold(yi))/scale(yi)

and z is distributed according to an exponential(1) df.

For GP distributed data, the transformation is:

    z = -log((1 + (shape(yi)/scale(yi) * (x - threshold(yi))))^(-1/shape(yi))

where again z follows an exponential(1) df.

For PP models, the transformation is:

    z = (1 + shape(yi)/scale(yi) * (x - threshold(yi)))^(-1/shape(yi))

and z is distributed exponential(1).

See Coles (2001) sec. 2.3.2 for more details.
}
\value{
numeric vector of transformed data.
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{revtrans.evd}}, \code{\link{fevd}}
}
\examples{
data(PORTw)

fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit

z <- trans(fit)

fevd(z)

}
\keyword{ distribution }
\keyword{ methods }
\keyword{ manip }
