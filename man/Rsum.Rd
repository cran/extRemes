\name{Rsum}
\alias{Rsum}
\docType{data}
\title{ Hurricane Frequency Dataset.}
\description{
This dataset gives the number of hurricanes per year (from 1925 to 1995) as well as the ENSO state and total monetary damage.
}
\usage{data(Rsum)}
\format{
  A data frame with 71 observations on the following 4 variables.
  \describe{
    \item{Year}{a numeric vector giving the year.}
    \item{EN}{a numeric vector giving the ENSO state (-1 for La Ninha, 1 for El Ninho and 0 otherwise).}
    \item{Ct}{a numeric vector giving the number of hurricanes for the corresponding year.}
    \item{TDam}{a numeric vector giving the total monetary damage (millions of U.S. dollars).}
  }
}
\details{
More information on these data can be found in Pielke and Landsea (1998) or Katz (2002).
}
\source{
\url{http://sciencepolicy.colorado.edu/pielke/hp_roger/hurr_norm/data.html}
}
\references{
Katz, R. W. (2002) Stochastic modeling of hurricane damage. \emph{Journal of Applied Meteorology}, \bold{41}, 754--762.

Pielke, R. A. and Landsea, C. W. (1998) Normalized hurricane damages in the United States: 1925-95. \emph{Weather and Forecasting}, \bold{13}, (3), 621--631.
}
\examples{
data(Rsum)
str(Rsum)
plot(Rsum)

# Reproduce Fig. 1 of Katz (2002).
plot(	Rsum[,"Year"], Rsum[,"TDam"]/1000, type="h", xlab="Year",
	ylab="Total damage (billion U.S. dollars)",
	ylim=c(0,80), lwd=2)

# Reproduce Fig. 2 of Katz (2002).
plot(Rsum[,"Year"],Rsum[,"Ct"],type="h", xlab="Year", ylab="Number of Hurricanes", ylim=c(0,5), lwd=2)
}
\keyword{datasets}
