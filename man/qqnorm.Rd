\name{qqnorm}
\alias{qqnorm}
\title{
Function to produce a normal qq-plot with 95 percent simultaneous confidence bands.
}
\description{
Calculates a normal qq-plot for a vector of data along with 95 percent simultaneous confidence bands.
}
\usage{
qqnorm(y, pch = 20, xlab = "Standard Normal Quantiles", ylab = "Sample Quantiles", ...)
}
\arguments{
  \item{y}{ numeric vector of data.}
  \item{pch}{ plot symbol to use.}
  \item{xlab}{ Character string giving abscissa label.}
  \item{ylab}{ Character string giving ordinate axis label.}
  \item{\dots}{ optional arguments to the plot function.}
}
\details{
  Confidence intervals are calculated using +/- k, where

k = 0.895 / (sqrt(n) * (1- 0.01 / sqrt(n) + 0.85/n))

Gives a 95 percent asymptotic band based on the Kolmogorov-Smirnov statistic (Doksum and Sievers, 1976).

}
\value{
No value is returned, but a plot is created.
}
\references{
Doksum, K.A. and G.L. Sievers, 1976.  Plotting with confidence: graphical comparisons of two populations.  Biometrika, 63 (3), 421--434.
}
\author{
Peter Guttorp
}

\seealso{ \code{\link{qnorm}}, \code{\link{qqplot}}, \code{\link{shiftplot}} }
\examples{
z <- rexp(100)
qqnorm( z)

y <- rnorm( 100)
qqnorm( y)

data( ftcanmax)
qqnorm( ftcanmax[,"Prec"])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ hplot }
