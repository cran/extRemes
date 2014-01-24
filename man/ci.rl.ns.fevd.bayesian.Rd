\name{ci.rl.ns.fevd.bayesian}
\alias{ci.rl.ns.fevd.bayesian}
\alias{ci.rl.ns.fevd.mle}
\title{
Confidence/Credible Intrvals for Effective Return Levels
}
\description{
Calculates credible intervals based on the upper and lower alpha/2 quantiles of the MCMC sample for effective return levels from a non-stationary EVD fit using Bayesian estimation, or find normal approximation confidence intervals if estimation method is MLE.
}
\usage{
\method{ci}{rl.ns.fevd.bayesian}(x, alpha = 0.05, return.period = 100, FUN = "mean",
                 burn.in = 499, ..., qcov = NULL, qcov.base = NULL,
                 verbose = FALSE)

\method{ci}{rl.ns.fevd.mle}(x, alpha = 0.05, return.period = 100, method =
                 c("normal"), verbose = FALSE, qcov = NULL, qcov.base =
                 NULL, ...)
}
\arguments{
  \item{x}{
An object of class \dQuote{fevd}.
}
  \item{alpha}{
Confidence level (numeric).
}
  \item{return.period}{
numeric giving the desired return period.  Must have length one!
}
  \item{FUN}{
character string naming the function to use to calculate the estimated return levels from the posterior sample (default takes the posterior mean).
}
  \item{burn.in}{
The first \code{burn.in} iterations will be removed from the posterior sample before calculating anything.
}
   \item{method}{Currently only \dQuote{normal} method is implemented.}
    \item{verbose}{logical, should progress information be printed to the screen?  Currently not used by the MLE method.}
  \item{\dots}{
Not used.
}
  \item{qcov, qcov.base}{
Matrix giving specific covariate values.  \code{qcov.base} is used if difference betwen effective return levels for two (or more) sets of covariates is desired, where it is rl(qcov) - rl(qcov.base).  See \code{make.qcov} for more details.  If not supplied, effective return levels are calculated for all of the original covariate values used for the fit.  If \code{qcov.base} is not NULL but \code{qcov} is NULL, then \code{qcov} takes on the values of \code{qcov.base} and \code{qcov.base} is set to NULL, and a warning message is produced.
}
}
\details{
Return levels are calculated for all coavariates supplied by \code{qcov} (and, if desired, \code{qcov.base}) for all values of the posterior sample (less the \code{burn.in}), or for all values of the original covariates used for the fit (if \code{qcov} and \code{qcov.base} are NULL).  The estimates aree taken from the sample according to \code{FUN} and credible intervals are returned according to \code{alpha}.
}
\value{
A three-column matrix is returned with the estimated effective return levels in the middle and lower and upper to the left and right.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{make.qcov}}, \code{\link{fevd}}, \code{\link{ci.fevd}}, \code{\link{return.level}}
}
\examples{
data(Fort)
fit <- fevd(Prec, threshold = 2, data = Fort,
    location.fun = ~cos(2 * pi * day /365.25),
    type = "PP", verbose = TRUE)

v <- make.qcov(fit, vals=list(mu1 = c(cos(2 * pi * 1 /365.25),
    cos(2 * pi * 120 /365.25), cos(2 * pi * 360 /365.25))))

ci(fit, return.period = 100, qcov = v)

\dontrun{
fit <- fevd(Prec, threshold = 2, data = Fort,
    location.fun = ~cos(2 * day /365.25),
    type = "PP", method = "Bayesian", verbose = TRUE)

ci(fit, return.period = 100, qcov = v)
}
}
\keyword{ htest }
