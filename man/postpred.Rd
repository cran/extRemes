\name{postpred}
\alias{postpred}
\alias{postpred.default}
\alias{postpred.fevd}
\alias{hist.postpred}
\alias{print.postpred}
\alias{summary.postpred}
\title{
Posterior Prediction of Return Level
}
\description{
Calculate the posterior prediction for the m-year return level.
}
\usage{
postpred(x, return.period = 100, ...)

\method{postpred}{default}(x, return.period = 100, ..., burn.in = 499, FUN = "mean", threshold = 0, 
    type = "GEV", lambda = 1, interval, period.basis = "year")

\method{postpred}{fevd}(x, return.period = 100, ..., burn.in = 499, FUN = "mean", interval)

\method{hist}{postpred}(x, ...)

\method{print}{postpred}(x, ..., alpha = 0.05)

\method{summary}{postpred}(object, ..., alpha = 0.05)
}

\arguments{

  \item{x, object}{
\code{postpred}: object of class \dQuote{fevd} where the method used to create it is \dQuote{Bayesian}.  Or, can be a matrix with appropriate column names.

\code{print}, \code{summary}: object of class \dQuote{postpred}.
}
  \item{return.period}{
single numeric giving the return period desired.
}
  \item{burn.in}{the burn-in period}
  \item{FUN}{function used to estimate the parameters/return values from teh MCMC sample.}
  \item{threshold}{numeric giving the threshold.}
  \item{type, lambda, period.basis}{type of EVD (character), rate parameter (numeric) and period basis (character).}
  \item{interval}{vector of length 2 giving the interval over which to search for the optimium (argument to \code{optimize}).}
  \item{alpha}{numeric giving the (1 - alpha) * 100\% prediction interval.}
  \item{\dots}{
\code{postpred}: optional arguments to \code{optimize}.

\code{hist}: optional arguments to \code{hist}.

Not used by \code{print} or \code{summary}.
   }

}

\details{

The posterior prediction for the return level is found by setting 1 - 1 / m equal to the mean over the MCMC sample of Pr[Z <= z | theta] (i.e., the probability of observing a value <= z based on the chosen EVD and for the parameter values at each iteration of the MCMC chain possibly after burn in), and solving numerically using \code{optimize}.  See Coles (2001, sec. 9.1.2) for more details.

The function does not currently handle models with covariates in teh parameter estimates.
}
\value{
list of class \dQuote{postpred} with components
  \item{minimum, objective }{values returned by optimize function, where minimum is the minimum value obtained, and objective is the desired return level estimate.}
  \item{return.period, return.basis }{the return level value and return period character as obtained from fevd object and/or passed in through the self-same argument name(s).}
  \item{sample}{The estimated return levels from the MCMC sample that result in the mean value returned as the estimate.}
}
\references{
Coles, S.  (2001). \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag.

}
\author{
Eric Gilleland
}

\seealso{
\code{\link{optimize}}, \code{\link{fevd}}
}
\examples{

\dontrun{
data(ftcanmax)

fitB <- fevd(Prec, ftcanmax, method="Bayesian",
    proposalParams = list(sd = c(10, 0.1, 0.1)),
    verbose=TRUE)
fitB
plot(fitB, "trace")

pfit <- postpred(fitB)

pfit

hist(pfit, col = "darkblue")

pfit <- postpred(fitB, m = 1000)
}
}
\keyword{ distribution }
