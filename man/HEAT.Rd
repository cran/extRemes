\name{HEAT}
\alias{HEAT}
\docType{data}
\title{ Summer maximum and minimum temperature in Phoenix, Arizona.}
\description{
Summer maximum and minimum temperature (degrees Fahrenheit) for July through August 1948 through 1990 at
Sky Harbor airport in Phoenix, Arizona.
}
\usage{data(HEAT)}
\format{
  A data frame with 43 observations on the following 3 variables.
  \describe{
    \item{Year}{a numeric vector giving the number of years since 1900.}
    \item{Tmax}{a numeric vector giving the Summer maximum temperatures in degrees Fahrenheit.}
    \item{Tmin}{a numeric vector giving the Summer minimum temperatures in degrees Fahrenheit.}
  }
}
\details{
Data is Summer maximum and minimum temperature for the months of July through August from 1948 through 1990.
}
\source{
U.S. National Weather Service Forecast office at the Phoenix Sky Harbor Airport.
}
\references{
Balling, R.C., Jr., Skindlov, J.A. and Phillips, D.H., The impact of increasing summer mean temperatures on
	extreme maximum and minimum temperatures in Phoenix, Arizona. Journal of Climate, 3:1491--1494, 1990.

Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for the 'Extremes Toolkit: Weather and Climate
 Applications of Extreme Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.

Tarleton, Lesley F. and Katz, Richard W., Statistical explanation for trends in extreme summer temperatures
	at Phoenix, A.Z., Journal of Climate, 8 (6):1704--1708, 1995.
}
\examples{
data(HEAT)
str(HEAT)
plot(HEAT)
}
\keyword{datasets}
