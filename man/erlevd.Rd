\name{erlevd}
\alias{erlevd}
\title{
Effective Return Levels
}
\description{
Find the so-called effective return levels for non-stationary extreme value distributions (EVDs).
}
\usage{
erlevd(x, period = 100)
}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd}.
}
  \item{period}{
number stating for what return period the effective return levels should be calculated.
}
}
\details{
Return levels are the same as the quantiles for the GEV df.  For the GP
df, they are very similar to the quantiles, but with the event frequency
taken into consideration.  Effective return levels are the return levels
obtained for given parameter/threshold values of a non-stationary model.
For example, suppose the df for data are modeled as a GEV(location(t) =
mu0 + mu1 * t, scale, shape), where \sQuote{t} is time.  Then for any
specific given time, \sQuote{t}, return levels can be found.  This is
done for each value of the covariate(s) used to fit the model to the
data.  See, for example, Gilleland and Katz (2011) for more details. 

This function is called by the \code{plot} method function for \dQuote{fevd} objects when the models are non-stationary.
}
\value{
A vector of length equal to the length of the data used to obtain the
fit. When \code{x} is from a PP fit with blocks, a vector of length
equal to the number of blocks.
}
\references{
Gilleland, E. and Katz, R. W. (2011). New software to analyze how extremes change over time. \emph{Eos}, 11 January, \bold{92}, (2), 13--14.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{fevd}}, \code{\link{rlevd}}, \code{\link{rextRemes}}, \code{\link{pextRemes}}, \code{\link{plot.fevd}}
}
\examples{

data(PORTw)

fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit
tmp <- erlevd(fit, period=20)

\dontrun{
# Currently, the ci function does not work for effective
# return levels.  There were coding issues encountered.
# But, could try:
#
z <- rextRemes(fit, n=500)
dim(z)
# 500 randomly drawn samples from the
# fitted model.  Each row is a sample
# of data from the fitted model of the
# same length as the data.  Each column
# is a separate sample.

sam <- numeric(0)
for( i in 1:500) {
    cat(i, " ")
    dat <- data.frame(z=z[,i], AOindex=PORTw$AOindex)
    res <- fevd(z, dat, location.fun=~AOindex)
    sam <- cbind(sam, c(erlevd(res)))
}
cat("\n")

dim(sam)

a <- 0.05
res <- apply(sam, 1, quantile, probs=c(a/2, 1 - a/2))
nm <- rownames(res)

res <- cbind(res[1,], tmp, res[2,])
colnames(res) <- c(nm[1], "Estimated 20-year eff. ret. level", nm[2])
res

}
}
\keyword{ distribution }
\keyword{ models }
