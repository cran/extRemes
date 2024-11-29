\name{Potomac}
\alias{Potomac}
\docType{data}
\title{ Potomac River Peak Stream Flow Data.}
\description{
Potomac River peak stream flow (cfs) data for water years (Oct-Sep) 1895 through 2000
at Point Rocks, Maryland.
}
\usage{data(Potomac)}
\format{
  A data frame with 106 observations on the following 2 variables.
  \describe{
    \item{Year}{a numeric vector giving the water year (Oct-Sep).}
    \item{Flow}{a numeric vector the peak stream flow (cfs; 1 cfs = 0.028317 cubic meters per second).}
  }
}
\details{
Potomac River peak stream flow (cfs) data for water years (Oct-Sep) 1895 through 2000 at Point Rocks, Maryland.

These data (up to 1986) have been analyzed by Smith (1987) and this entire dataset by Katz et al. (2002) Sec. 2.3.2.
}
\source{
U.S. Geological Survey.
}
\references{
Katz, R. W., Parlange, M. B. and Naveau, P. (2002) Statistics of extremes in hydrology. \emph{Advances in Water Resources}, \bold{25}, 1287--1304. 

Smith, J. A. (1987) Regional flood frequency analysis using extreme order statistics of the annual peak record. \emph{Water Resour Res}, \bold{23}, 1657--1666.
}
\examples{
data(Potomac)
str(Potomac)
# Fig. 3 of Katz et al. (2002) Sec. 2.3.2.
plot(Potomac[,"Year"], Potomac[,"Flow"]/1000, yaxt="n", ylim=c(0,500), type="l", lwd=1.5,
    xlab="Water Year (Oct-Sept)", ylab="Annual peak flow (thousand cfs)")
axis(2,at=seq(0,500,100),labels=as.character(seq(0,500,100)))
}
\keyword{datasets}
