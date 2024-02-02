\name{lr.test}
\alias{lr.test}
\title{
Likelihood-Ratio Test
}
\description{
Conduct the likelihood-ratio test for two nested extreme value distribution models.
}
\usage{
lr.test(x, y, alpha = 0.05, df = 1, ...)
}
\arguments{
  \item{x,y}{
Each can be either an object of class \dQuote{fevd} (provided the fit method is MLE or GMLE) or a single numeric giving the negative log-likelihod value for each model.  \code{x} should be the model with fewer parameters, but if both \code{x} and \code{y} are \dQuote{fevd} objects, then the order does not matter (it will be determined from which model has more parameters).
}
  \item{alpha}{
single numeric between 0 and 1 giving the significance level for the test.
}
  \item{df}{
single numeric giving the degrees of freedom.  If both \code{x} and \code{y} are \dQuote{fevd} objects, then the degrees of freedom will be calculated, and this argument ignored.  Otherwise, if either or both of \code{x} and \code{y} are single numerics, then it must be provided or the test may be invalid.
}
  \item{\dots}{
Not used.
}
}
\details{
When it is desired to incorporate covariates into an extreme value analysis, one method is to incorporate them into the parameters of the extreme value distributions themselves in a regression-like manner (cf. Coles, 2001 ch 6; Reiss and Thomas, 2007 ch 15).  In order to justify whether or not inclusion of the covariates into the model is significant or not is to apply the likelihood-ratio test (of course, the test is more general than that, cf. Coles (2001) p 35).

The test is only valid for comparing nested models.  That is, the parameters of one model must be a subset of the parameters of the second model.

Suppose the base model, m0, is nested within the model m1.  Let \code{x} be the negative log-likelihood for m0 and \code{y} for m1.  Then the likelihood-ratio statistic (or deviance statistic) is given by (Coles, 2001, p 35; Reiss and Thomas, 2007, p 118):

    D = -2*(\code{y} - \code{x}).

Letting c.alpha be the (1 - alpha) quantile of the chi-square distribution with degrees of freedom equal to the difference in the number of model parameters, the null hypothesis that D = 0 is rejected if D > c.alpha (i.e., in favor of model m1).
}
\value{
A list object of class \dQuote{htest} is returned with components:
  \item{statistic }{The test statistic value (referred to as D above).}
  \item{parameter }{numeric vector giving the chi-square critical value (c.alpha described above), the significance leve (alpha) and the degrees of freedom.}
  \item{alternative}{character string stating \dQuote{greater} indicating that the alternative decision is determined if the statistic is greater than c.alpha.}
  \item{p.value}{numeric giving the p-value for the test.  If the p-value is smaller than alpha, then the decision is to reject the null hypothesis in favor of the model with more parameters.}
  \item{method}{character string saying \dQuote{Likelihood-ratio Test}.}
  \item{data.name}{character vector of length two giving the names of the datasets used for the test (if \dQuote{fevd} objects are passed) or the negative log-likelihood values if numbers are passed, or the names of x and y.  Although the names may differ, the models should have been fit to the same data set.}
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkhauser, 530pp., 3rd edition. 
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{fevd}}, \code{\link{taildep.test}}
}
\examples{
data(PORTw)
fit0 <- fevd(PORTw$TMX1, type="Gumbel") 
fit1 <- fevd(PORTw$TMX1)
fit2 <- fevd(TMX1, PORTw, scale.fun=~STDTMAX)
lr.test(fit0, fit1)
lr.test(fit1, fit2)

}
\keyword{ htest }
