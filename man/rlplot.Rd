\name{rlplot}
\alias{rlplot}
\title{ Return Level Plot for a Fitted Object of an Extreme-Value Distribution }
\description{
Plots several return levels against the return period for a fitted object from one of
the \pkg{ismev} functions: \code{gev.fit} and \code{gpd.fit}.
}
\usage{
rlplot(z, ci = 0.05, add.ci = FALSE)
}
\arguments{
  \item{z}{ A list object as returned by one of gev.fit or gpd.fit (with appropriate class attribute added). }
  \item{ci}{ The (1-ci)*100 confidence value.}
  \item{add.ci}{ logical if true will add confidence bounds to plot.}
}
\details{
Given a fitted list object from \code{gev.fit} or \code{gpd.fit}--attributed with the class \dQuote{gev.fit} or \dQuote{gpd.fit}, respectively--the return level plot is generated.  Confidence bounds, if included, are found by the delta method, which is generally appropriate for shorter return periods, but not for longer return periods because the return level distribution is generally skewed.  Therefore, if a plot with better estimates of the confidence bounds are desired, use \code{add.ci=FALSE}, and use the R function \code{lines} to add different bounds (e.g., using values obtained from the \code{gev.parameterCI} or \code{gpd.parameterCI} functions).

This function is simply a modification of \pkg{ismev} functions \code{gpd.rl} and \code{gev.rl} (Coles, 2001).
}
\value{
A plot is created.  If assigned to an object, a list will be returned with the following items.
   \item{period}{The return periods used for calculating the return levels.}
   \item{level}{The estimated return level for each return period.}
   \item{lower}{If add.ci is TRUE, then this is a vector of lower 1-ci confidence bounds.  Otherwise the value is NULL.}
   \item{upper}{If add.ci is TRUE, then this is a vector of upper 1-ci confidence bounds.  Otherwise the value is NULL.}
}
\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}.  London: Springer-Verlag, 208pp.

}
\author{ Eric Gilleland }

\seealso{ \code{\link{gev.parameterCI}}, \code{\link{gpd.parameterCI}}, \code{gev.diag}, \code{gpd.diag}}
\examples{
data(ftcanmax)
fit <- gev.fit( ftcanmax[,"Prec"])
class( fit) <- "gev.fit"
rlplot( fit)
}
\keyword{ hplot }
