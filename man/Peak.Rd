\name{Peak}
\alias{Peak}
\docType{data}
\title{ Salt River Peak Stream Flow}
\description{
Peak stream flow data from 1924 through 1999 for the Salt River near Roosevelt, Arizona.
}
\usage{data(Peak)}
\format{
  A data frame with 75 observations on the following 2 variables.
  \describe{
    \item{Year}{a numeric vector giving the year.}
    \item{Flow}{a numeric vector giving the peak stream flow (cfs).}
    \item{Winter}{a numeric vector giving the Winter seasonal mean Darwin pressure (mb--1000).}
    \item{Spring}{a numeric vector giving the Spring seasonal mean Darwin pressure (mb--1000).}
    \item{Summer}{a numeric vector giving the Summer seasonal mean Darwin pressure (mb--1000).}
    \item{Fall}{a numeric vector giving the Fall seasonal mean Darwin pressure (mb--1000) (see Katz et al. (2002)
			Sec. 5.2.2).}
  }
}
\details{
Peak stream flow in cfs (1 cfs=0.028317 $m^3/s$) data for water years (October through September)
from 1924 through 1999 for the Salt River near Roosevelt, Arizona.  Data for 1986 are missing.
Also includes seasonal mean Darwin pressures (mb--1000).

Several analyses have been performed on streamflow at this location (see, e.g., Anderson and
Meerschaert (1998), Dettinger and Diaz (2000); and, for extreme stream flow, Katz et al. (2002) Sec. 5.2.2).
}
\source{
U.S. Geological Survey for Salt River peak flows.
NOAA Climate Prediction Center (\url{http://www.cpc.ncep.noaa.gov/data/indices}) for seasonal mean
Darwin pressures.
}
\references{
Anderson, P. L. and Meerschaert, M. M.  (1998) Modeling river flows with heavy tails. \emph{Water Resour Res}, \bold{34}, (9), 2271--2280.

Dettinger, M. D. and Diaz, H. F. (2000) Global characteristics of stream flow seasonality and variability. \emph{Journal of Hydrometeorology}, \bold{1}, 289--310.

Katz, R. W., Parlange, M. B. and Naveau, P. (2002) Statistics of extremes in hydrology. \emph{Advances in Water Resources}, \bold{25}, 1287--1304.
}
\examples{
data(Peak)
str(Peak)
# Fig. 9 of Katz et al. (2002) Sec. 5.2.2.
plot(Peak[,"Year"], Peak[,"Flow"]/1000, type="l", yaxt="n",
    xlab="Water year (Oct-Sept)", ylab="Annual peak flow (thousand cfs)")
axis(2,at=c(0,40,80,120),labels=c("0","40","80","120"))
}
\keyword{datasets}
