\name{Denversp}
\alias{Denversp}
\docType{data}
\title{ Denver July hourly precipitation amount.}
\description{
Hourly precipitation (mm) for Denver, Colorado in the month of July from 1949 to 1990.
}
\usage{data(Denversp)}
\format{
  A data frame with 31247 observations on the following 4 variables.
  \describe{
    \item{Year}{a numeric vector giving the number of years from 1900.}
    \item{Day}{a numeric vector giving the day of the month.}
    \item{Hour}{a numeric vector giving the hour of the day (1 to 24).}
    \item{Prec}{a numeric vector giving the precipitation amount (mm).}
  }
}
\details{
These observations are part of an hourly precipitation dataset for the United States that has been critically
assessed by Collander et al. (1993).  The Denver hourly precipitation dataset is examined further by Katz and
Parlange (1995).  Summer precipitation in this region near the eastern edge of the Rocky Mountains is predominantly
of local convective origin (Katz and Parlange (1005)).
}
\source{
Katz, R. W. and Parlange, M. B. (1995) Generalizations of chain-dependent processes: Application to hourly precipitation, \emph{Water Resources Research} \bold{31}, (5), 1331--1341.
}
\references{
Collander, R. S., Tollerud, E. I., Li, L., and Viront-Lazar, A. (1993) Hourly precipitation data and station histories: A research assessment, in \emph{Preprints, Eighth Symposium on Meteorological Observations and Instrumentation, American Meteorological Society}, Boston, 153--158.
}
\examples{
data(Denversp)
plot( Denversp[,1], Denversp[,4], xlab="", ylab="Hourly precipitation (mm)", xaxt="n")
axis(1,at=c(50,60,70,80,90),labels=c("1950","1960","1970","1980","1990"))
}
\keyword{datasets}
