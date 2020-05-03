\name{Fort}
\alias{Fort}
\docType{data}
\title{ Daily precipitation amounts in Fort Collins, Colorado.}
\description{
Daily precipitation amounts (inches) from a single rain gauge in Fort Collins, Colorado.  See
Katz et al. (2002) Sec. 2.3.1 for more information and analyses.
}
\usage{data(Fort)}
\format{
A data frame with dimension 36524 by 5.  Columns are: "obs", "tobs", "month", "day", "year" and "Prec"; where
"Prec" is the daily precipitation amount (inches).
}
\source{
Originally from the Colorado Climate Center at Colorado State University.  The Colorado state climatologist office no longer provides this data without charge. The data can be obtained from the NOAA/NCDC web site, but there are slight differences (i.e., some missing values for temperature).
}
\references{
Katz, R. W., Parlange, M. B. and Naveau, P. (2002) Statistics of extremes in hydrology. \emph{Advances in Water Resources}, \bold{25}, 1287--1304.
}
\examples{
data(Fort)
str(Fort)
plot(Fort[,"month"], Fort[,"Prec"], xlab="month", ylab="daily precipitation (inches)")
}
\keyword{datasets}
