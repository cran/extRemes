\name{ftcanmax}
\alias{ftcanmax}
\docType{data}
\title{ Annual Maximum Precipitation: Fort Collins, Colorado}
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
Originally from the Colorado Climate Center at Colorado State University.  The Colorado state climatologist office no longer provides this data without charge. The data can be obtained from the NOAA/NCDC web site, but there are slight differences (i.e., some missing values for temperature).  The annual maximum precipitation data is taken directly from the daily precipitation data available in this package under the name \dQuote{Fort}.
}
\references{
Katz, R. W., Parlange, M. B. and Naveau, P. (2002) Statistics of extremes in hydrology. \emph{Advances in Water Resources}, \bold{25}, 1287--1304.
}
\examples{
data(ftcanmax)
str(ftcanmax)
plot(ftcanmax, type="l", lwd=2)

}
\keyword{datasets}
