\name{ftcanmax}
\alias{ftcanmax}
\docType{data}
\title{ Fort Collins, Colorado annual maximum precipitation.}
\description{
Annual maximum precipitation (inches) for one rain gauge in Fort Collins, Colorado from 1900 through 1999.
See Katz et al. (2002) Sec. 2.3.1 for more information and analyses.
}
\usage{data(ftcanmax)}
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{Year}{a numeric vector giving the Year.}
    \item{Prec}{a numeric vector giving the annual maximum precipitation amount in inches.}
  }
}
\source{
Colorado Climate Center, Colorado State University (\url{http://ulysses.atmos.colostate.edu}).
The annual maximum precipitation data is taken from daily precipitation data.
}
\references{
Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for
     the 'Extremes Toolkit: Weather and Climate Applications of Extreme
     Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.

Katz, Richard W., Parlange, Marc B. and Naveau, Philippe. Statistics of extremes in hydrology. Advances in
	Water Resources, 25:1287--1304, 2002.
}
\examples{
data(ftcanmax)
str(ftcanmax)
plot(ftcanmax, type="l", lwd=2)

# See Gilleland et al. (2005) for more examples using these data with extRemes.
}
\keyword{datasets}
