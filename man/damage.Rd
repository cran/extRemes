\name{damage}
\alias{damage}
\docType{data}
\title{ Hurricane Damage Data}
\description{
Estimated economic damage (billions USD) caused by hurricanes.
}
\usage{data(damage)}
\format{
  A data frame with 144 observations on the following 3 variables.
  \describe{
    \item{obs}{a numeric vector that simply gives the line numbers.}
    \item{Year}{a numeric vector giving the years in which the specific hurricane occurred.}
    \item{Dam}{a numeric vector giving the total estimated economic damage in billions of U.S. dollars.}
  }
}
\details{
More information on these data can be found in Pielke and Landsea (1998) or Katz (2002).  Also see
Gilleland et al. (2005) for examples using \pkg{extRemes}.
}
\source{
\url{http://sciencepolicy.colorado.edu/pielke/hp_roger/hurr_norm/data.html}
}
\references{
Gilleland, Eric and Katz, Richard W. Tutorial for the 'Extremes Toolkit:
   Weather and Climate Applications of Extreme Value Statistics.'
   \url{http://www.assessment.ucar.edu/toolkit}, 2005.

Katz, Richard W., Stochastic modeling of hurricane damage. Journal of Applied Meteorology, 41:754--762, 2002.

Pielke, Roger A. and Landsea, CW., Normalized hurricane damages in the United States: 1925-95. Weather
        and Forecasting, 13 (3):621--631, 1998.
}
\examples{
data(damage)
plot( damage[,1], damage[,3], xlab="", ylab="Economic Damage", type="l", lwd=2)

# Fig. 3 of Katz (2002).
plot( damage[,"Year"], log( damage[,"Dam"]), xlab="Year", ylab="ln(Damage)", ylim=c(-10,5))

# Fig. 4 of Katz (2002).
qqnorm( log( damage[,"Dam"]), ylim=c(-10,5))
}
\keyword{datasets}
