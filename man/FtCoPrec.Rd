\name{FtCoPrec}
\alias{FtCoPrec}
\docType{data}
\title{ Daily precipitation amounts in Fort Collins, Colorado.}
\description{
Daily precipitation amounts (inches) from a single rain gauge in Fort Collins, Colorado.  See
Katz et al. (2002) Sec. 2.3.1 for more information and analyses.
}
\usage{data(FtCoPrec)}
\format{
A matrix with dimension 36524 by 5.  Columns are: "obs", "month", "day", "year" and "Prec"; where
"Prec" is the daily precipitation amount (inches).
}
\source{
Colorado Climate Center, Colorado State University (\url{http://ulysses.atmos.colostate.edu}).
}
\references{
Gilleland, Eric, Katz, Richard W. and Young, Greg. Tutorial for
     the 'Extremes Toolkit: Weather and Climate Applications of Extreme
     Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.

Katz, Richard W., Parlange, Marc B. and Naveau, Philippe. Statistics of extremes in hydrology. Advances in
        Water Resources, 25:1287--1304, 2002.
}
\examples{
data(FtCoPrec)
str(FtCoPrec)
plot(FtCoPrec[,"month"], FtCoPrec[,"Prec"], xlab="month", ylab="daily precipitation (inches)")

# See Gilleland et al. (2005) for more examples using these dta with extRemes.
}
\keyword{datasets}
