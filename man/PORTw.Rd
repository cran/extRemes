\name{PORTw}
\alias{PORTw}
\alias{SEPTsp}
\docType{data}
\title{ Daily Maximum and Minimum Temperature}
\description{
Daily maximum and minimum Winter temperature (degrees centigrade) with a covariate for the
North Atlantic Oscillation index from 1927 through 1995.  Data is for Winter for Port Jervis,
New York (PORTw) and Spring for Sept-Iles, Quebec (SEPTsp).
}
\usage{data(PORTw)}
\format{
  A data frame with 68 observations on the following 16 variables.
  \describe{
    \item{Year}{a numeric vector giving the year.}
    \item{MTMAX}{a numeric vector giving the mean maximum temperatures (degrees centigrade) over a one month period.}
    \item{MTMIN}{a numeric vector giving the mean minimum temperatures (degrees centigrade). over a one month period.}
    \item{STDTMAX}{a numeric vector giving the standard deviations of maximum temperatures (degrees centigrade) over a
			one month period.}
    \item{STDMIN}{a numeric vector giving the standard deviations of minimum temperatures (degrees centigrade)
			over a one month period.}
    \item{TMX1}{a numeric vector giving the maximum temperature (degrees centigrade) over a one month period.}
    \item{TMN0}{a numeric vector giving the minimum temperature (degrees centigrade) over a one month period.}
    \item{MDTR}{a numeric vector giving the mean diurnal temperature range (degrees centigrade).}
    \item{AOindex}{a numeric vector giving the Atlantic Oscillation index (see Thompson and Wallace (1998)).}
  }
}
\details{
See Wettstein and Mearns (2002) for a much more detailed explanation of the above variables.
}
\source{
National Oceanic and Atmospheric Administration/National Climate Data Center (NOAA/NCDC).
}
\references{
Thompson, D. W. J. and Wallace, J. M. (1998) The Arctic Oscillation signature in the wintertime geopotential height and temperature fields. \emph{Geophys. Res. Lett.}, \bold{25}, 1297--1300.

Wettstein, J. J. and Mearns, L. O. (2002) The influence of the North Atlantic-Arctic Oscillation on mean, variance and extremes of temperature in the northeastern United States and Canada. \emph{Journal of Climate}, \bold{15}, 3586--3600.
}
\examples{
data(PORTw)
str(PORTw)
par( mfrow=c(2,1))
plot(PORTw[,"TMX1"], type="l", lwd=2, xlab="", xaxt="n", ylab="Maximum Temperature (C)")
plot(PORTw[,"TMN0"], type="l", lwd=2, xlab="", xaxt="n", ylab="Minimum Temperature (C)")
}
\keyword{datasets}
