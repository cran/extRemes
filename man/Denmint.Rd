\name{Denmint}
\alias{Denmint}
\docType{data}
\title{ Denver Minimum Temperature}
\description{
Daily minimum temperature (degrees centigrade) for Denver, Colorado from 1949 through 1999.
}
\usage{data(Denmint)}
\format{
  A data frame with 18564 observations on the following 5 variables.
  \describe{
    \item{Time}{a numeric vector indicating the line number (time from first entry to the last).}
    \item{Year}{a numeric vector giving the year.}
    \item{Mon}{a numeric vector giving the month of each year.}
    \item{Day}{a numeric vector giving the day of the month.}
    \item{Min}{a numeric vector giving the minimum temperature in degrees Fahrenheit.}
  }
}
\source{
Colorado Climate Center, Colorado State University (\url{http://ulysses.atmos.colostate.edu}).
}
\examples{
data(Denmint)
plot( Denmint[,3], Denmint[,5], xlab="", xaxt="n", ylab="Minimum Temperature (deg. F)")
axis(1,at=1:12,labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
}
\keyword{datasets}
