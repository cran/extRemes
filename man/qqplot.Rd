\name{qqplot}
\alias{qqplot}
\title{
Make a qq-plot between two vectors of data, along with 95 percent confidence bands.
}
\description{
QQ-plot between two data vectors with 95 percent confidence bands based on the Kolmogorov-Smirnov statistic (Doksum and Sievers, 1976).
}
\usage{
qqplot(x, y, pch = 20, xlab = "x Quantiles", ylab = "y Quantiles", main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{ numeric vector of length 'm' giving one data set.}
  \item{y}{ numeric vector of length 'n' giving the other data set.}
  \item{pch}{ Plot character.}
  \item{xlab}{ Character string giving the label for the abscissa axis.}
  \item{ylab}{ Character string giving the label for the ordinate axis.}
  \item{main}{ Character string giving a plot title.}
  \item{\dots}{ Other optional arguments to the plot function.}
}
\details{
  Plots the sorted (missing-values removed) 'x' values against the sorted, and interpolated (via the approxfun function from package stats), 'y' values.  Confidence bands are about the sorted and interpolated 'y' values using +/- K/sqrt(M), where

K = 1.36

and

M = m*n / (m+n).
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
\seealso{ \code{\link{approxfun}}, \code{\link{qqnorm}}, \code{\link{shiftplot}} }
\examples{
z <- rnorm(100)
y <- rexp(100)
qqplot( z, y)
qqplot( y, z)

data( ftcanmax)
qqplot( ftcanmax[,"Prec"], z)
qqplot( ftcanmax[,"Prec"], y)
}
\keyword{ hplot }
