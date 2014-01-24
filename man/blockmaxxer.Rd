\name{blockmaxxer}
\alias{blockmaxxer}
\alias{blockmaxxer.data.frame}
\alias{blockmaxxer.fevd}
\alias{blockmaxxer.matrix}
\alias{blockmaxxer.vector}

\title{
Find Block Maxima
}
\description{
Find the block maximum of a data set.
}
\usage{
blockmaxxer(x, ...)

\method{blockmaxxer}{data.frame}(x, ..., which = 1, blocks = NULL,
    blen = NULL, span = NULL)

\method{blockmaxxer}{fevd}(x, ...)

\method{blockmaxxer}{matrix}(x, ..., which = 1, blocks = NULL,
    blen = NULL, span = NULL)

\method{blockmaxxer}{vector}(x, ..., blocks = NULL, blen = NULL,
    span = NULL)
}
\arguments{
  \item{x}{
An object of class \dQuote{fevd} where the fit is for a PP model, a numeric vector, matrix or data frame.
}
  \item{\dots}{
optional arguments to \code{max}.
}
  \item{which}{
number or name of a column indicating for which column to take the block maxima.  Note, this does not take componentwise maxima (as in the multivariate setting).  Instead, it takes the maxima for a single column and returns a vector, data frame or matrix of the block maxima for that column along with the entire row where that maxima occurred.
}
  \item{blocks}{
numeric (integer or factor) vector indicating the blocks over which to take the maxima.  Must be non-NULL if \code{blen} and \code{span} are NULL.
}
  \item{blen}{
(optional) may be used instead of the \code{blocks} argument, and \code{span} must be non-NULL.  This determines the length of the blocks to be created.  Note, the last block may be smaller or larger than \code{blen}.  Ignored if \code{blocks} is not NULL.
}
  \item{span}{
(optional) must be specified if \code{blen} is non-NULL and \code{blocks} is NULL.  This is the number of blocks over which to take the maxima, and the returned value will be either a vector of length equal to \code{span} or a matrix or data frame with \code{span} rows.
}
}
\value{
vector of length equal to the number of blocks (vector method) or a matrix or data frame with number of rows equal to the number of blocks (matrix and data frame methods).

The \code{fevd} method is for finding the block maxima of the data passed to a PP model fit and the blocks are determined by the \code{npy} and \code{span} components of the fitted object.  If the \code{fevd} object is not a PP model, the function will error out.  This is useful for utilizing the PP model in the GEV with approximate annual maxima.  Any covariate values that occur contiguous with the maxima are returned as well.

The \code{aggregate} function is used with \code{max} in order to take the maxima from each block.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{max}}, \code{\link{aggregate}}
}
\examples{
data(Fort)

bmFort <- blockmaxxer(Fort, blocks = Fort$year, which="Prec")

plot(Fort$year, Fort$Prec, xlab = "Year", ylab = "Precipitation (inches)",
    cex = 1.25, cex.lab = 1.25,
    col = "darkblue", bg = "lightblue", pch = 21)

points(bmFort$year, bmFort$Prec, col="darkred", cex=1.5)

}
\keyword{manip}
