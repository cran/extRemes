\name{extRemes-package}
\alias{extRemes-package}
\alias{extRemes}
\docType{package}

\title{extRemes -- Weather and Climate Applications of Extreme Value Analysis (EVA)}
\description{
\pkg{extRemes} is a suite of functions for carrying out analyses on the extreme values of a process of interest; be they block maxima over long blocks or excesses over a high threshold.

Versions >= 2.0-0 of this package differ considerably from the original package (versions <= 1.65), which was largely a package of graphical user interfaces (GUIs) mostly calling functions from the \pkg{ismev} package; a companion software package to Coles (2001).  The former GUI windows of \pkg{extRemes} (<= 1.65) now run the command-line functions of \pkg{extRemes} (>= 2.0) and have been moved to a new package called \pkg{in2extRemes}.

For assistance using \pkg{extRemes} (>= 2.0-0), please see the tutorial at:

\url{https://www.jstatsoft.org/article/view/v072i08} 

Extreme Value Statistics:

Extreme value statistics are used primarily to quantify the stochastic behavior of a process at unusually large (or small) values. Particularly, such analyses usually require estimation of the probability of events that are more extreme than any previously observed. Many fields have begun to use extreme value theory and some have been using it for a very long time including meteorology, hydrology, finance and ocean wave modeling to name just a few.  See Gilleland and Katz (2011) for a brief introduction to the capabilities of \pkg{extRemes}.

Example Datasets:

There are several example datasets included with this toolkit.  In each case, it is possible to load these datasets into R using the \code{data} function.  Each data set has its own help file, which can be accessed by \code{help([name of dataset])}.  Data included with \pkg{extRemes} are:

Denmint -- Denver daily minimum temperature.

Flood.dat -- U.S. Flood damage (in terms of monetary loss) ('dat' file used as example of reading in common data
                        using the extRemes dialog).

ftcanmax -- Annual maximum precipitation amounts at one rain gauge in Fort Collins, Colorado.

HEAT -- Summer maximum (and minimum) temperature at Phoenix Sky Harbor airport.

Ozone4H.dat -- Ground-level ozone order statistics from 1997 from 513 monitoring stations in the eastern United States.

PORTw -- Maximum and minimum temperature data (and some covariates) for Port Jervis, New York.

Rsum -- Frequency of Hurricanes.

SEPTsp -- Maximum and minimum temperature data (and some covariates) for Sept-Iles, Quebec.

damage -- Hurricane monetary damage.

Denversp -- Denver precipitation.

FCwx -- data frame giving daily weather data for Fort Collins, Colorado, U.S.A. from 1900 to 1999.

Flood -- R source version of the above mentioned 'Flood.dat' dataset.

Fort -- Precipitation amounts at one rain gauge in Fort Collins, Colorado.

Peak -- Salt River peak stream flow.

Potomac -- Potomac River peak stream flow.

Tphap -- Daily maximum and minimum temperatures at Phoenix Sky Harbor Airport.


Primary functions available in \pkg{extRemes} include:

\code{fevd}: Fitting extreme value distribution functions (EVDs: GEV, Gumbel, GP, Exponential, PP) to data (block maxima or threshold excesses).

\code{ci}: Method function for finding confidence intervals for EVD parameters and return levels.

\code{taildep}: Estimate chi and/or chibar; statistics that inform about tail dependence between two variables.

\code{atdf}: Auto-tail dependence function and plot.  Helps to inform about possible dependence in the extremes of a process.  Note that a process that is highly correlated may or may not be dependent in the extremes.

\code{decluster}: Decluster threshold exceedance in a data set to yield a new related process that is more closely independent in the extremes.  Includes two methods for declustering both of which are based on runs declustering.

\code{extremalindex}: Estimate the extremal index, a measure of dependence in the extremes.  Two methods are available, one based on runs declustering and the other is the intervals estiamte of Ferro and Segers (2003).

\code{devd, pevd, qevd, revd}: Functions for finding the density, cumulative probability distribution (cdf), quantiles and make random draws from EVDs.

\code{pextRemes, rextRemes, return.level}: Functions for finding the cdf, make random draws from, and find return levels for fitted EVDs.

To see how to cite \pkg{extRemes} in publications or elsewhere, use \code{citation("extRemes")}.
}

\section{Acknowledgements}{
Funding for \pkg{extRemes} was originally provided by the Weather and Climate Impacts Assessment Science (WCIAS) Program at the National Center for Atmospheric Research (NCAR) in Boulder, Colorado. WCIAS was funded by the National Science Foundation (NSF).  Curent funding is provided by the  Regional  Climate  Uncertainty  Program  (RCUP),  an  NSF-supported  program  at NCAR.  NCAR is operated by the nonprofit University Corporation for Atmospheric Research (UCAR) under the sponsorship of the NSF. Any opinions, findings, conclusions, or recommendations expressed in this publication/software package are those of the author(s) and do not necessarily reflect the views of the NSF.
}

\references{
Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Ferro, C. A. T. and Segers, J. (2003). Inference for clusters of extreme values. \emph{Journal of the Royal Statistical Society B}, \bold{65}, 545--556.

Gilleland, E. and Katz, R. W. (2011). New software to analyze how extremes change over time. \emph{Eos}, 11 January, \bold{92}, (2), 13--14.
}

\keyword{ package }
