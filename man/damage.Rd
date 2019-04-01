\name{damage}
\alias{damage}
\docType{data}
\title{ Hurricane Damage Data}
\description{
Estimated economic damage (billions USD) caused by hurricanes.
}
\usage{data(damage)}
\format{
  A data frame with 144 observations on the following 3 variables.
  \describe{
    \item{obs}{a numeric vector that simply gives the line numbers.}
    \item{Year}{a numeric vector giving the years in which the specific hurricane occurred.}
    \item{Dam}{a numeric vector giving the total estimated economic damage in billions of U.S. dollars.}
  }
}
\details{
More information on these data can be found in Pielke and Landsea (1998) or Katz (2002).
}

\references{
Katz, R. W. (2002) Stochastic modeling of hurricane damage. \emph{Journal of Applied Meteorology}, \bold{41}, 754--762.

Pielke, R. A. Jr. and Landsea, C. W. (1998) Normalized hurricane damages in the United States: 1925-95. \emph{Weather and Forecasting}, \bold{13}, (3), 621--631.
}
\examples{
data(damage)
plot( damage[,1], damage[,3], xlab="", ylab="Economic Damage", type="l", lwd=2)

# Fig. 3 of Katz (2002).
plot( damage[,"Year"], log( damage[,"Dam"]), xlab="Year", ylab="ln(Damage)", ylim=c(-10,5))

# Fig. 4 of Katz (2002).
qqnorm( log( damage[,"Dam"]), ylim=c(-10,5))
}
\keyword{datasets}
