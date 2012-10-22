\name{threshrange.plot}
\alias{threshrange.plot}
\title{
Threshold Selection Through Fitting Models to a Range of Thresholds
}
\description{
Find an appropriate threshold for GP or PP models by fitting them to a sequence of thresholds in order to find the lowest threshold that yields roughly the same parameter estiamtes as any higher threshold.
}
\usage{
threshrange.plot(x, r, type = c("GP", "PP", "Exponential"), nint = 10, alpha = 0.05,
    na.action = na.fail, set.panels = TRUE, verbose = FALSE, ...)
}

\arguments{
  \item{x}{
numeric vector of data.
}
  \item{r}{
numeric vector of length two giving the range of thresholds.
}
  \item{type}{
character string stating which model to fit.
}
  \item{nint}{
number of thresholds to use.
}
  \item{alpha}{
number between zero and one stating which 1 - \code{alpha} confidence level to use for the confidence limits.
}
  \item{na.action}{
function to be called to handle missing values.
}
  \item{set.panels}{
logical; should the function set the panels on the device (TRUE) or not (FALSE).
}
  \item{verbose}{
logical; should progress information be printed to the screen?
}
  \item{\dots}{
optional arguments to \code{fevd}.
}
}
\details{
Several GP or PP (or exponential) models are fit to \code{x} according to a range of \code{nint} thresholds given by \code{r}.  The estimated parameters are plotted against these thresholds along with their associated (1 - \code{alpha}) * 100 percent CIs.

Choice of threshold is a compromise between low variance (lower thresholds yield more data with which to fit the models) and bias (higher thresholds yield estimates that are less biased because model assumptions require very high thresholds, and it can happen that lower data values may be more abundant causing the model to be biased toward the wrong values) in the parameter estimates.  An appropriate threshold should yield the same parameter estimates (within uncertainty) as would be fit for any model fit to higher thresholds.  Therefore, the idea is to find the lowest possible threshold whereby a higher threshold would give the same results within uncertainty bounds.

See Coles (2001) sec. 4.3.4 and 4.4 for more information.

Note that the default uses maximum likelihood estimation.  While it is possible to use other methods, it is not recommended because of efficiency problems.
}
\value{
A matrix of parameter values and CI bounds for each threshold value is returned invisibly.  A plot is created.
}
\references{
Coles, S.  (2001). \emph{An introduction to statistical modeling of extreme values}, London, United Kingdom: Springer-Verlag, 208 pp.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{mrlplot}}
}
\examples{
data(Fort)
threshrange.plot(Fort$Prec, r = c(1, 2.25), nint=5)

\dontrun{
threshrange.plot(Fort$Prec, r=c(0.01,1), nint=30, verbose=TRUE)

threshrange.plot(Fort$Prec, r=c(0.2,0.8), type="PP", nint=15,
    verbose=TRUE)

threshrange.plot(Fort$Prec, r=c(0.2,0.8), type="PP", nint=15,
    optim.args=list(method="Nelder-Mead"), verbose=TRUE)

}
}
\keyword{ hplot }
\keyword{ htest }
\keyword{ models}
