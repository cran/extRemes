\name{Flood}
\alias{Flood}
\docType{data}
\title{ United States Total Economic Damage Resulting from Floods}
\description{
United States total economic damage (in billions of U.S. dollars) caused by floods by hydrologic year from
1932-1997.  See Pielke and Downton (2000) for more information.
}
\usage{data(Flood)}
\format{
  A data frame with 66 observations on the following 5 variables.
  \describe{
    \item{OBS}{a numeric vector giving the line number.}
    \item{HYEAR}{a numeric vector giving the hydrologic year.}
    \item{USDMG}{a numeric vector giving total economic damage (in billions of U.S. dollars) caused by floods.}
    \item{DMGPC}{a numeric vector giving damage per capita.}
    \item{LOSSPW}{a numeric vector giving damage per unit wealth.}
  }
}
\details{
From Pielke and Downton (2000):

The National Weather Service (NWS) maintains a national flood damage record from 1903 to the present, and state level
data from 1983 to the present.  The reported losses are for "significant flood events" and include only direct economic
damage that results from flooding caused by ranfall and/or snowmelt.  The annual losses are based on "hydrologic years"
from October through September.  Flood damage per capita is computed by dividing the inflation-adjusted losses for each
hydrological year by the estimated population on 1 July of that year (www.census.gov).  Flood damage per million dollars
of national wealth uses the net stock of fixed reproducible tangible wealth in millions of current dollars (see Pielke
and Downton (2000) for more details; see also Katz et al. (2002) for analysis).
}
\source{
National Weather Service
}
\references{ 
Katz, R. W., Parlange, M. B. and Naveau, P. (2002) Statistics of extremes in hydrology, \emph{Advances in Water Resources}, \bold{25}, 1287--1304.

Pielke, R. A. Jr. and Downton, M. W. (2000) Precipitation and damaging floods: trends in the United States, 1932-97, \emph{Journal of Climate}, \bold{13}, (20), 3625--3637.
}
\examples{
data(Flood)
plot( Flood[,2], Flood[,3], type="l", lwd=2, xlab="hydrologic year",
    ylab="Total economic damage (billions of U.S. dollars)")
}
\keyword{datasets}
