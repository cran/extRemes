\name{shiftplot}
\alias{shiftplot}
\title{ Shift Plot Between Two Sets of Data}
\description{
 A shift plot is a plot of the quantiles of a data set y minus those of another data set x against those of x.  Includes 95 percent simultaneous confidence bands.
}
\usage{
shiftplot(x, y, pch = 20, xlab = "x Quantiles", ylab = "y Quantiles", main = NULL, ...)
}
\arguments{
  \item{x}{ numeric vector of length m.}
  \item{y}{ numeric vector of length n.}
  \item{pch}{ Plotting character.}
  \item{xlab}{ Character string giving abscissa axis label.}
  \item{ylab}{ Character string giving ordinate axis label.}
  \item{main}{ Character string giving plot title.}
  \item{\dots}{ Other optional arguments to plot function.}
}
\details{
The shift plot is a graph of y_q - x_q vs. x_q, where y_q and x_q denote the quantiles of x and y, resp.  95 percent simultaneous confidence bands are calculated per Doksum and Sievers (1976).  The primary usage of this plot is where x is a control group and y is an experimental method; or something similar.  For example, x might represent observations, and y might represent climate model output; or some such.
}
\value{
No value is returned, but a plot is created.
}
\references{
Doksum, K. A. and Sievers, G. L. (1976)  Plotting with confidence: graphical comparisons of two populations.  \emph{Biometrika}, \bold{63}, (3), 421--434.
}
\author{
Peter Guttorp
}
\seealso{ \code{\link{qqplot}}, \code{\link{qqnorm}}, \code{\link{approxfun}} }
\examples{
z <- rnorm( 100)
y <- rexp(30)
shiftplot( z, y)

data( ftcanmax)
shiftplot( y, ftcanmax[,"Prec"])
}
\keyword{ hplot }
