\name{Rsum}
\alias{Rsum}
\docType{data}
\title{ Hurricane frequency dataset.}
\description{
This dataset gives the number of hurricanes per year (from 1925 to 1995) as well as the ENSO state and total
monetary damage.
}
\usage{data(Rsum)}
\format{
  A data frame with 71 observations on the following 4 variables.
  \describe{
    \item{Year}{a numeric vector giving the year.}
    \item{EN}{a numeric vector giving the ENSO state (-1 for La Ni\~{n}a, 1 for El Ni\~{n}o and 0 otherwise).}
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
Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for the 'Extremes Toolkit: Weather and Climate
 Applications of Extreme Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.

Katz, Richard W., Stochastic modeling of hurricane damage. Journal of Applied Meteorology, 41:754--762, 2002.

Pielke, Roger A. and Landsea, CW., Normalized hurricane damages in the United States: 1925-95. Weather
        and Forecasting, 13 (3):621--631, 1998.
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

# See Gilleland et al. (2005) for more examples using these data with extRemes.
}
\keyword{datasets}
