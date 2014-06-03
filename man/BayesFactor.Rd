\name{BayesFactor}
\alias{BayesFactor}
\title{
Estimate Bayes Factor
}
\description{
Estimate Bayes factor between two models for two \dQuote{fevd} objects.
}
\usage{
BayesFactor(m1, m2, burn.in = 499, FUN = "postmode", 
    method = c("laplace", "harmonic"), verbose = FALSE)
}
\arguments{
  \item{m1, m2}{
objects of class \dQuote{fevd} giving the two models to be compared.
}
  \item{burn.in}{
numeric how many of the first several iterations from the MCMC sample to throw away before estimating the Bayes factor.
}
  \item{FUN}{
function to be used to determine the estimated parameter values from the MCMC sample.  With the exception of the default (posterior mode), the function should operate on a matrix and return a vector of length equal to the number of parameters.  If \dQuote{mean} is given, then \code{colMeans} is actually used.
}
  \item{method}{
Estimation method to be used.
}
  \item{verbose}{
logical, should progress information be printed to the screen (no longer necessary).
}
}
\details{
Better options for estimating the Bayes factor from an MCMC sample are planned for the future.  The current options are perhaps the two most common, but do suffer from major drawbacks.  See Kass and Raftery (1995) for a review.
}
\value{
A list object of class \dQuote{htest} is returned with components:

  \item{statistic}{The estimated Bayes factor.}
  \item{method}{character string naming which estimation method was used.}
  \item{data.name}{character vector naming the models being compared.}
}
\references{
Kass, R. E. and Raftery, A. E. (1995) Bayes factors.  \emph{J American Statistical Association}, \bold{90} (430), 773--795.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}
}
\examples{
data(PORTw)
fB <- fevd(TMX1, PORTw, method = "Bayesian", iter = 500)
fB2 <- fevd(TMX1, PORTw, location.fun = ~AOindex,
    method = "Bayesian", iter = 500)

BayesFactor(fB, fB2, burn.in = 100, method = "harmonic")

}
\keyword{ htest }
