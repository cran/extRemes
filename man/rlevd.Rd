\name{rlevd}
\alias{rlevd}
\title{
Return Levels for Extreme Value Distributions
}
\description{
Calculate return levels for extreme value distributions (EVDs).
}
\usage{
rlevd(period, loc = 0, scale = 1, shape = 0, threshold = 0,
    type = c("GEV", "GP", "PP", "Gumbel", "Frechet", "Weibull",
    "Exponential", "Beta", "Pareto"),
    npy = 365.25, rate = 0.01)
}
\arguments{
  \item{period}{
numeric vector giving the desired return periods.
}
  \item{loc, scale, shape}{
single numbers giving the parameter values.
}
  \item{threshold}{
number giving the threshold, if applicable.
}
  \item{type}{
character string naming which EVD to calculate return levels from.  If \code{type} is \dQuote{PP}, then it is converted to \dQuote{GEV}.
}
  \item{npy}{
number stating how many values per year.
}
  \item{rate}{
The rate of exceedance.
}
}
\details{
The extreme value distributions (EVD's) are generalized extreme value (GEV) or generalized Pareto (GP).  The point process characterization is an equivalent form, but is not handled here.  The GEV df is given by

    Pr{X <= x} = G(x) = exp[-{1 + shape*(x - location)/scale}^(-1/shape)]

for 1 + shape*(x - location) > 0 and scale > 0.  It the shape parameter is zero, then the df is defined by continuity and simplies to

    G(x) = exp(-exp((x - location)/scale)).

The GEV df is often called a family of df's because it encompasses the three types of EVD's: Gumbel (shape = 0, light tail), Frechet (shape > 0, heavy tail) and the reverse Weibull (shape < 0, bounded upper tail at location - scale/shape).  It was first found by R. von Mises (1936) and also independently noted later by meteorologist A. F. Jenkins (1955).  It enjoys theretical support for modeling maxima taken over large blocks of a series of data.

The generalized Pareo df is given by (Pickands, 1975)

    Pr{X <= x} = F(x) = 1 - [1 + shape*(x - threshold)/scale]^(-1/shape)

where 1 + shape*(x - threshold)/scale > 0, scale > 0, and x > threshold.  If shape = 0, then the GP df is defined by continuity and becomes

    F(x) = 1 - exp(-(x - threshold)/scale).

There is an approximate relationship between the GEV and GP df's where the GP df is approximately the tail df for the GEV df.  In particular, the scale parameter of the GP is a function of the threshold (denote it scale.u), and is equivalent to scale + shape*(threshold - location) where scale, shape and location are parameters from the \dQuote{equivalent} GE Vdf.  Similar to the GEV df, the shape parameter determines the tail behavior, where shape = 0 gives rise to the exponential df (light tail), shape > 0 the Pareto df (heavy tail) and shape < 0 the Beta df (bounded upper tail at location - scale.u/shape).  Theoretical justification supports the use of the GP df family for modeling excesses over a high threshold (i.e., y = x - threshold).  It is assumed here that \code{x}, \code{q} describe x (not y = x - threshold).  Similarly, the random draws are y + threshold.

See Coles (2001) and Reiss and Thomas (2007) for a very accessible text on extreme value analysis and for more theoretical texts, see for example, Beirlant et al. (2004), de Haan and Ferreira (2006), as well as Reiss and Thomas (2007).

Return levels are essentially the same as quantiles.  In the case of the GEV family, they are the same.  In the case of the GP df, they are very similar, but the exceedance rate is taken into consideration.
}
\value{
named numeric vector of same length as \code{period} giving the calculated return levels for each return period.
}
\references{
Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004) \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

de Haan, L. and Ferreira, A. (2006) \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Jenkinson, A. F. (1955) The frequency distribution of the annual maximum (or minimum) of meteorological elements. \emph{Quart. J. R.  Met. Soc.}, \bold{81}, 158--171.

Pickands, J. (1975) Statistical inference using extreme order statistics.  \emph{Annals of Statistics}, \bold{3}, 119--131.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkh\"{a}user, 530pp., 3rd edition.

von Mises, R. (1936) La distribution de la plus grande de n valeurs, \emph{Rev. Math. Union Interbalcanique} \bold{1}, 141--160.

}
\author{
Eric Gilleland
}
\note{
Currently, this function does not handle the PP type.  Return levels for this case can be handled in several different ways.  For example, they could be calculated from the equivalent GEV df or equivalent GP df.  In any case, one needs first to determine how to handle the frequency component.
}

\seealso{
\code{\link{devd}}, \code{\link{return.level}}
}
\examples{
rlevd(c(2, 20, 100), loc=10, scale=2, shape=0.5)

rlevd(c(2, 20, 100), scale=2, shape=0.5, type="GP")

}
\keyword{ distribution }
