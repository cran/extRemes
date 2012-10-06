\name{Tphap}
\alias{Tphap}
\docType{data}
\title{ Daily Maximum and Minimum Temperature in Phoenix, Arizona.}
\description{
Daily maximum and minimum temperature (degrees Fahrenheit) for July through August 1948 through 1990 at
Sky Harbor airport in Phoenix, Arizona.
}
\usage{data(Tphap)}
\format{
  A data frame with 43 observations on the following 3 variables.
  \describe{
    \item{Year}{a numeric vector giving the number of years since 1900.}
    \item{Month}{a numeric vector giving the month.}
    \item{Day}{a numeric vector giving the day of the month.}
    \item{MaxT}{a numeric vector giving the daily maximum temperatures in degrees Fahrenheit.}
    \item{MinT}{a numeric vector giving the daily minimum temperatures in degrees Fahrenheit.}
  }
}
\details{
Data are daily maximum and minimum temperature for the summer months of July through August from 1948 through 1990.
}
\source{
U.S. National Weather Service Forecast office at the Phoenix Sky Harbor Airport.
}
\references{
Balling, R. C., Jr., Skindlov, J. A. and Phillips, D. H. (1990) The impact of increasing summer mean temperatures on extreme maximum and minimum temperatures in Phoenix, Arizona. \emph{Journal of Climate}, \bold{3}, 1491--1494.

Tarleton, L. F. and Katz, R. W. (1995) Statistical explanation for trends in extreme summer temperatures at Phoenix, A.Z. \emph{Journal of Climate}, \bold{8}, (6), 1704--1708.
}
\examples{
data(Tphap)
str(Tphap)

par( mfrow=c(2,1))
hist( Tphap[,"MaxT"], main="", xlab="Max Temp", xlim=c(60,120), freq=FALSE, breaks="FD", col="red")
hist( Tphap[,"MinT"], main="", xlab="Min Temp", xlim=c(60,120), freq=FALSE, breaks="FD", col="blue")
}
\keyword{datasets}
