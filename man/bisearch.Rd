\name{bisearch}
\alias{bisearch}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Simple bisection search. }
\description{
Bisection search algorithm to find where a function crosses a particular value.
}
\usage{
bisearch(x1, x2, f, tol = 1e-07, niter = 25, upcross.level = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{ Low-end starting value. }
  \item{x2}{ High-end starting value. }
  \item{f}{ A function for which to find the crossing value. }
  \item{tol}{ The tolerance that determines whether to break (early) from the algorithm. }
  \item{niter}{ Number of iterations to try before quitting. }
  \item{upcross.level}{ The level where it is desired to find where \code{f} crosses.}
}
\details{
This function is shamelessly stolen from the \pkg{fields} package (cf. \code{bisection.search}),
but is slightly simpler (i.e., less useful) in that it doesn't allow for extra arguments to \code{f}.
It is used in the \code{gev.parameterCI} and \code{gpd.parameterCI} functions to try to find analytically
where the profile likelihood crosses the chi-square based critical value for obtaining confidence
bounds on parameter estimates.
}
\value{
  \item{x }{The estimated crossing.}
  \item{fm }{The difference between the function value at the found crossing and \code{upcross.level}.}
  \item{iter}{Number of iterations used to find the crossing.}
}
\references{Fishbane, P.M., Gasiorowicz, S., and S.T. Thornton
Physics for Scientists and Engineers,
2d ed. (Extended), Upper Saddle River, New Jersey: Prentice Hall, 1996.}
\author{ Unknown }
\seealso{ \code{\link{gev.parameterCI}},\code{\link{gpd.parameterCI}}}
\examples{
# Try to solve x^2 = 0.2
# Initial guess is that it occurs between x=0 and x=1.
bisearch( 0, 1, function(x) x^2, tol=1e-20, upcross.level=0.2)
}
\keyword{math}
