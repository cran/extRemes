\name{mrlplot}
\alias{mrlplot}
\title{
Mean Residual Life Plot
}
\description{
An empirical mean residual life plot, including confidence intervals, is produced. The mean residual life plot aids the selection of a threshold for the GPD or point process models.
}
\usage{
mrlplot(x, nint = 100, alpha = 0.05, na.action = na.fail, xlab = "Threshold", ...)
}
\arguments{
  \item{x}{
numeric vector of data.
}
  \item{nint}{
Number of thresholds to use.
}
  \item{alpha}{
number giving the 1 - \code{alpha} confidence levels to use.
}
  \item{na.action}{
function to be called to handle missing values.
}
  \item{xlab}{
character string giving the abscissa label to use.
} 
  \item{\dots}{optional arguments to \code{plot}.}
}

\details{
The mean excesses are found for each value of a range of thresholds that cover the range of the data (less one at the high end).  CIs are also shown based on the normal df for the mean excesses.  The goal is to find the lowest threshold such that the graph is linear with increasing thresholds, within uncertainty.

See Coles (2001) sec. 4.3.1 for more information.
}
\value{
A matrix with the mean excess values and their confidence bounds is returned invisibly.
}
\references{
Coles, S.  (2001). \emph{An introduction to statistical modeling of extreme values}, London, United Kingdom: Springer-Verlag, 208 pp.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{threshrange.plot}}
}
\examples{
data(Fort)
mrlplot(Fort$Prec)
}
\keyword{ hplot }
\keyword{ htest }
