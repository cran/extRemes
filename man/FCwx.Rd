\name{FCwx}
\alias{FCwx}
\docType{data}
\title{
Fort Collins, Colorado Weather Data
}
\description{
Weather data from Fort Collins, Colorado, U.S.A. from 1900 to 1999.
}
\usage{data(FCwx)}
\format{
  The format is:
 chr "FCwx"
}
\details{
Data frame with components:

Year: integer years from 1900 to 1999,

Mn: integer months from 1 to 12,

Dy: integer days of the month (i.e., from 1 to 28, 29, 30 or 31 depending on the month/year),

MxT: integer valued daily maximum temperature (degrees Fahrenheit),

MnT: integer valued daily minimum temperature (degrees Fahrenheit),

Prec: numeric giving the daily accumulated precipitation (inches),

Snow: numeric daily accumulated snow amount,

SnCv: numeric daily snow cover amount
}
\source{
Originally from the Colorado Climate Center at Colorado State University.  The Colorado state climatologist office no longer provides this data without charge. The data can be obtained from the NOAA/NCDC web site, but there are slight differences (i.e., some missing values for temperature).
}
\references{
Katz, R. W., Parlange, M. B. and Naveau, P.  (2002) Statistics of extremes in hydrology. \emph{Advances in Water Resources}, \bold{25}, 1287--1304.
}
\examples{
data(FCwx)
str(FCwx)
plot(FCwx$Mn, FCwx$Prec)
plot(1:36524, FCwx$MxT, type="l")
}
\keyword{datasets}
