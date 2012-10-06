\name{as.extRemesDataObject}
\alias{as.extRemesDataObject}
\title{ Convert a Data Frame, Matrix or Vector to an extRemes Data Object }
\description{
Converts a data frame, matrix or vector to an object of class \dQuote{extRemesDataObject} so that extRemes GUI dialogs can handle the data.
}
\usage{
as.extRemesDataObject(x)
}
\arguments{
  \item{x}{ A data frame, matrix or vector. }
}
\details{
If an object is a vector of length n, an n by 2 matrix is created where the first column is simply the line number.  If a vector (e.g., rnorm(10)) is passed as the argument \code{x}, then the second column will be labelled as \code{x}.  If \code{x} is a matrix or data frame, then the column names of \code{x} will be used.  Otherwise, column names will be \code{x1},\code{x2}, etc.
}
\value{
A list object is returned with class \dQuote{extRemesDataObject} and only one component.
  \item{data }{The data passed via the x argument.}
}
\author{ Eric Gilleland }
\seealso{ \code{\link{extremes.gui}}}
\examples{
z <- rnorm( 100)
y <- as.extRemesDataObject(z)
# The object 'y' can now be used with the extRemes GUI dialogs.
# To open the main dialog window, use 'extremes.gui()'.
}
\keyword{misc}
