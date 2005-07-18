\name{as.extRemesDataObject}
\alias{as.extRemesDataObject}
\title{ Convert a data frame, matrix or vector to an object of class "extRemesDataObject" }
\description{
Converts a data frame, matrix or vector to an object of class "extRemesDataObject" so that extRemes GUI dialogs
can handle the data.
}
\usage{
as.extRemesDataObject(x)
}
\arguments{
  \item{x}{ A data frame, matrix or vector. }
}
\details{
If an object is a vector of length n, an 'n X 2' matrix is created where the first column is simply the line number.
If a vector (e.g., rnorm(10)) is passed as the argument 'x', then the second column will be labelled as 'x'.  If
'x' is a matrix or data frame, then the column names of 'x' will be used.  Otherwise, column names will be 'x1,x2,...'
}
\value{
A list object is returned with class "extRemesDataObject" and only one component.
  \item{data }{The data passed via the 'x' argument.}
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
