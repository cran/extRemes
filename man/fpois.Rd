\name{fpois}
\alias{fpois}
\alias{fpois.default}
\alias{fpois.data.frame}
\alias{fpois.matrix}
\alias{fpois.list}
\title{

Fit Homogeneous Poisson to Data and Test Equality of Mean and Variance

}
\description{
Fit a homogeneous Poisson to data and test whether or not the mean and variance are equal.
}
\usage{
fpois(x, na.action = na.fail, ...)

\method{fpois}{default}(x, na.action = na.fail, ...)

\method{fpois}{data.frame}(x, na.action = na.fail, ..., which.col = 1)

\method{fpois}{matrix}(x, na.action = na.fail, ..., which.col = 1)

\method{fpois}{list}(x, na.action = na.fail, ..., which.component = 1)


}
\arguments{
  \item{x}{

    numeric, matrix, data frame or list object containing the data to which the Poisson is to be fit.

}
  \item{na.action}{

   function to be called to handle missing values.

}
  \item{\dots}{

Not used.

}

  \item{which.col,which.component}{column or component (list) number containing the data to which the Poisson is to be fit.}

}
\details{

The probability function for the (homogeneous) Poisson distribution is given by:

    Pr( N = k ) = exp(-lambda) * lambda^k / k!

for k = 0, 1, 2, ...

The rate parameter, lambda, is both the mean and the variance of the Poisson distribution.
To test the adequacy of the Poisson fit, therefore, it makes sense to test whether or not
the mean equals the variance.  R. A. Fisher showed that under the assumption that X_1, ...,
X_n follow a Poisson distribution, the statistic given by:

    D = (n - 1) * var(X_1) / mean(X_1)

follows a Chi-square distribution with n - 1 degrees of freedom.  Therefore, the p-value
for the one-sided alternative (greater) is obtained by finding the probability of being greater
than D based on a Chi-square distribution with n - 1 degrees of freedom.

}
\value{

A list of class \dQuote{htest} is returned with components:

  \item{statistic }{The value of the dispersion D}
  \item{parameter }{named numeric vector giving the estimated mean, variance, and degrees of freedom.}
  \item{alternative}{character string with the value \dQuote{greater} indicating the one-sided alternative hypothesis.}
  \item{p.value}{the p-value for the test.}
  \item{method}{character string stating the name of the test.}
  \item{data.name}{character naming the data used by the test (if a vector is applied).}
}
\author{
Eric Gilleland
}
\seealso{
\code{\link{glm}}
}
\examples{

data(Rsum)
fpois(Rsum$Ct)

\dontrun{

# Because 'Rsum' is a data frame object,
# the above can also be carried out as:

fpois(Rsum, which.col = 3)

# Or:

fpois(Rsum, which.col = "Ct")

##
## For a non-homogeneous fit, use glm.
##
## For example, to fit the non-homogeneous Poisson model
## Enso as a covariate:
##

fit <- glm(Ct~EN, data = Rsum, family = poisson())
summary(fit)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ htest }
\keyword{ univar }
