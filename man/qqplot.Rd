\name{qqplot}
\alias{qqplot}
\alias{plot.qqplot}
\alias{summary.qqplot}
\title{
qq-plot Between Two Vectors of Data with 95 Percent Confidence Bands
}
\description{
QQ-plot between two data vectors with 95 percent confidence bands based on the Kolmogorov-Smirnov statistic (Doksum and Sievers, 1976).
}
\usage{
qqplot(x, y, pch = 20, xlab = "x Quantiles", ylab = "y Quantiles", regress = TRUE,
    make.plot = TRUE, ...)

\method{plot}{qqplot}(x, ...)

\method{summary}{qqplot}(object, ...)
}
\arguments{
  \item{x}{ \code{qqplot}: numeric vector of length 'm' giving one data set.

\code{plot} method function: list object of class \dQuote{qqplot} returned by \code{qqplot}.}
  \item{object}{list object of class \dQuote{qqplot} returned by \code{qqplot}.}
  \item{y}{ numeric vector of length 'n' giving the other data set.}
  \item{pch}{ Plot character.}
  \item{xlab}{ Character string giving the label for the abscissa axis.}
  \item{ylab}{ Character string giving the label for the ordinate axis.}
  \item{regress}{logical, should a regression line be fit to the quantiles?}
  \item{make.plot}{logical, should the plot be created (TRUE) or not (FALSE)?}
  \item{\dots}{ Other optional arguments to the plot function.  Not used by \code{summary} method function.}
}
\details{
  Plots the sorted (missing-values removed) 'x' values against the sorted, and interpolated (via the approxfun function from package stats), 'y' values.  Confidence bands are about the sorted and interpolated 'y' values using +/- K/sqrt(M), where

K = 1.36

and

M = m*n / (m+n).

The \code{plot} method function does exactly the same thing as \code{qqplot} except that it does not need to do any calculations.

The \code{summary} method function merely displays the original call to the function unless a regression line was fit between the quantiles, in which case summary information is displayed for the regression (i.e., the \code{summary} method function for \code{lm} is run on the \dQuote{lm} object).
}
\value{
An object of class \dQuote{qqplot} is invisibly returned by each function (in the case of the method functions, the object entered is simply returned invisibly).  This is a list object with components:
\item{call}{calling string}
\item{names}{list object with components x and y giving the object names for the objects passed into x and y, resp.}
\item{regression}{If regress was TRUE, then this is the fitted regression object as returned by lm.  Otherwise, this component is not included.}
\item{qdata}{data frame with components: x and y giving the quantiles for x and y, resp., and lower and upper giving the lower and upper 95 percent confidence bands, resp.}
}
\references{
Doksum, K.A. and G.L. Sievers, 1976.  Plotting with confidence: graphical comparisons of two populations.  Biometrika, 63 (3), 421--434.
}
\author{
Peter Guttorp, peter \dQuote{at} stat.washington.edu
}
\seealso{ \code{\link{approxfun}}, \code{\link{qqnorm}}, \code{\link{shiftplot}} }
\examples{
z <- rnorm(100)
y <- rexp(100)
qqplot( z, y)
qqplot( y, z)

data( ftcanmax)
qqplot( ftcanmax[,"Prec"], z)
obj <- qqplot( ftcanmax[,"Prec"], y, make.plot=FALSE)
plot(obj)
summary(obj)

}
\keyword{ hplot }
