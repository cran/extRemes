\name{levd}
\alias{levd}
\title{
Extreme Value Likelihood
}
\description{
Find the EVD parameter likelihood given data.
}
\usage{
levd(x, threshold, location, scale, shape,
    type = c("GEV", "GP", "PP", "Gumbel", "Weibull", "Frechet",
    "Exponential", "Beta", "Pareto"), log = TRUE, negative = TRUE,
    span, npy = 365.25, infval = Inf, weights = 1, blocks = NULL)
}
\arguments{
  \item{x}{
A numeric vector of data of length n.
}
  \item{threshold}{
number or numeric vector of length n giving the desired threshold, if applicable.
}
  \item{location}{
number or numeric vector of length n giving the location parameter value(s), if applicable.
}
  \item{scale}{
number or numeric vector of length n giving the scale parameter value(s).
}
  \item{shape}{
number or numeric vector of length n giving the shape parameter value(s), if applicable.
}
  \item{type}{
character string naming the particular EVD for which to compute the likelihood.
}
  \item{log, negative}{
logicals; should the negative log-likelihood (default) be returned (both TRUE) or the likelihood (both FALSE)?  It is possible to return other possibilities such as the negative likelihood (log = FALSE, negative = TRUE) or the log-likelihood (log = TRUE, negative = FALSE).
}
  \item{span}{
number stating how many periods (usually years) the data cover (applicable only for PP models).  Currently not used.
}
  \item{npy}{
number of points per period (period is usually years).
}
  \item{infval}{
Value to return if the likelihood is infinite.  If negative is FALSE, the negative of this value will be returned.  The default is to return \code{Inf}, but noe that for optimization routines, this would result in an error and stop the process.  Therefore, it can be advantageous to use a very large value instead.
}
    \item{weights}{numeric of length 1 or n giving weights to be applied in the likelihood calculation (e.g., if some data points are to be weighted more/less heavily than others).}
    \item{blocks}{An optional list containing information required to evaluate the
likelihood of point process models in a computationally-efficient
manner by using only the exceedances and not the observations below
the threshold(s). See details.}
}
\details{
This function is called by a wrapper function within \code{fevd} and other functions.  It is generally an internal function, but may be useful for some users.

The negative log-likelihood for the generalized extreme value (GEV) df, possibly with parameters that are functions of covariates, yi) is given by:

    sum(log(scale(yi))) + sum(z^(-1/shape(yi)) + sum(log(z) * (1/shape(yi) + 1)),

where z = (x - location(yi))/scale(yi), x are the data.  For the Frechet and Weibull cases, the shape parameter is forced to have the correct sign, so it does not matter if the user chooses positive or negative shape.  In the case of shape = 0, defined by continuity (Gumbel case), the negative log-likelihood simplifies to:

    sum(log(scale(yi))) + sum(z) + sum(exp(-z)),

where z is as above.

The negative log-likelihood for the GP df is given by:

    sum(log(scale(yi))) + sum( log(z) * (1/shape(yi) + 1)),

where z = 1 + shape(yi) * t, where t = (x[x > threshold] - threshold(yi))/scale(yi).  Similar to the GEV df, the Beta and Pareto cases are forced to have the correct sign for the shape parameter.  In the case of shape = 0, defined by continuity (Exponential case), the negative log-likelihood simplifies to:

    sum(log(scale(yi))) + z,

where z is as above in the GP negative log-likelihood.

See Coles (2001) for more details.

Using Blocks to Reduce Computation in PP Fitting:

When \code{blocks} is supplied, the user should provide only the
exceedances and not all of the data values. The list should contain a
component called \code{nBlocks} indicating the number of observations
within a block, where blocks are defined in a manner analogous to that
used in GEV models. The list should also contain components named
\code{threshold}, \code{location}, \code{scale}, \code{shape}, and \code{weights} corresponding
to the arguments of the same name supplied to \code{levd}, but with values
on a per block basis. If some of the observations within any block are
missing (assuming missing at random or missing completely at random),
the list should contain a \code{proportionMissing} component that is a
vector with one value per block indicating the proportion of
observations missing for the block. Scalar values are allowed when a
component is stationary. Warning: to properly analyze nonstationary
models, the components must be constant within each block.
}
\value{
A single number giving the likelihood value (or negative log-likelihood or log-likelihood or negative likelihood depending on the value of the log and negative arguments).
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}, \code{\link{profliker}}
}
\examples{
data(ftcanmax)

levd(ftcanmax$Prec, location=134.66520, scale=53.28089, shape=0.17363)


}
\keyword{ distribution }
\keyword{ models }
