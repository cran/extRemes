\name{Ozone4H}
\alias{Ozone4H}
\docType{data}
\title{ Ground-level ozone order statistics.}
\description{
Ground-level ozone order statistics from 1997 at 513 monitoring stations in the eastern United States.
}
\usage{data(Ozone4H)}
\format{
  A data frame with 513 observations on the following 5 variables.
  \describe{
    \item{station}{a numeric vector identifying the station (or line) number.}
    \item{r1}{a numeric vector giving the maximum ozone reading (ppb) for 1997.}
    \item{r2}{a numeric vector giving the second-highest ozone reading (ppb) for 1997.}
    \item{r3}{a numeric vector giving the third-highest ozone reading (ppb) for 1997.}
    \item{r4}{a numeric vector giving the fourth-highest ozone reading (ppb) for 1997.}
  }
}
\details{
Ground level ozone readings in parts per billion (ppb) are recorded hourly at ozone monitoring stations
throughout the country during the "ozone season" (roughly April to October).  These data are taken from
a dataset giving daily maximum 8-hour average ozone for 5 ozone seasons (including 1997).  The new U.S.
Environmental Protection Agency (EPA) National Ambient Air Quality Standard (NAAQS) for ground-level ozone
is based on a three-year average of fourth-highest daily 8-hour maximum ozone readings (see
\url{http://www.epa.gov/air/criteria.html}).

For more analysis on the original data regarding the U.S. EPA NAAQS for ground-level ozone, see Fuentes (2003),
Gilleland and Nychka (2005) and Gilleland et al. (2005b).  For an example of using these data with \pkg{extRemes},
see Gilleland et al. (2005a).  These data are in the form required by the \code{rlarg.fit} function of Stuart
Coles available in the R package \pkg{ismev}; see Coles (2001) for more on the r-th largest order statistic model
and the function \code{rlarg.fit}.
}
\source{
Data was originally provided by the U.S. EPA (\url{http://www.epa.gov/epahome/ozone.htm}), and can be obtained
(daily maximum 8-hour average ozone for all five seasons from 1995 to 1999) from the Geophysical Statistics
Project (GSP) at the National Center for Atmospheric Research (NCAR) at

\url{http://www.cgd.ucar.edu/stats/data.shtml}

along with the orignal longitude/latitude coordinates for the stations.
}
\references{
Coles, Stuart. An Introduction to Statistical Modeling of Extreme Values. Springer-Verlag, London, 2001.

Fuentes, Montserrat.  Statistical assessment of geographic areas of compliance with air
  quality.  Journal of Geophysical Research, 108(D24), 2003.

Gilleland, Eric and Nychka, Douglas. Statistical Models for Monitoring and Regulating Ground-level Ozone,
   Environmetrics, 16:535--546. 2005.

a) Gilleland, Eric and Katz, Richard W. Tutorial for the 'Extremes Toolkit: Weather and Climate
 Applications of Extreme Value Statistics.' \url{http://www.assessment.ucar.edu/toolkit}, 2005.

b) Gilleland, Eric, Nychka, Douglas, and Schneider, Uli. Spatial models for the distribution of extremes, Applications of
 Computational Statistics in the Environmental Sciences: Hierarchical Bayes and MCMC Methods Edited by J.S. Clark & A.
 Gelfand. Oxford University Press, 2005. (to appear)
}
\examples{
data(Ozone4H)
str(Ozone4H)
plot(Ozone4H)
# See the extRemes tutorial (Gilleland et al. (2005a)) for a much better example (uses the GUI windows).
}
\keyword{datasets}
