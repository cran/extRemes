\name{devd}
\alias{devd}
\alias{pevd}
\alias{qevd}
\alias{revd}
\title{
Extreme Value Distributions
}
\description{
Density, distribution function (df), quantile function and random generation for the generalized extreme value and generalized Pareto distributions.
}
\usage{
devd(x, loc = 0, scale = 1, shape = 0, threshold = 0, log = FALSE,
    type = c("GEV", "GP"))

pevd(q, loc = 0, scale = 1, shape = 0, threshold = 0, lambda = 1, 
    npy, type = c("GEV", "GP", "PP", "Gumbel", "Frechet", "Weibull", 
        "Exponential", "Beta", "Pareto"), lower.tail = TRUE, log.p = FALSE)

qevd(p, loc = 0, scale = 1, shape = 0, threshold = 0,
    type = c("GEV", "GP", "PP", "Gumbel", "Frechet", "Weibull", "Exponential", "Beta",
    "Pareto"), lower.tail = TRUE)

revd(n, loc = 0, scale = 1, shape = 0, threshold = 0,
    type = c("GEV", "GP"))
}
\arguments{
  \item{x,q}{numeric vector of quantiles.}
  \item{p}{numeric vector of probabilities.  Must be between 0 and 1 (non-inclusive).}
  \item{n}{number of observations to draw.}
  \item{npy}{Number of points per period (period is usually year).  Currently not used.  PP model is converted to equivalent GEV df.}
  \item{lambda}{Event frequency base rate.  Currently not used.  PP model is converted to equivalent GEV df.}
  \item{loc, scale, shape}{location, scale and shape parameters.  Each may be a vector of same length as \code{x} (\code{devd} or length \code{n} for \code{revd}.  Must be length 1 for \code{pevd} and \code{qevd}.}
  \item{threshold}{numeric giving the threshold for the GP df.  May be  a vector of same length as \code{x} (\code{devd} or length \code{n} for \code{revd}.  Must be length 1 for \code{pevd} and \code{qevd}.}
  \item{log, log.p}{logical; if TRUE, probabilites p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].}
  \item{type}{character; one of "GEV" or "GP" describing whether to use the GEV or GP.}
}
\details{
The extreme value distributions (EVD's) are generalized extreme value (GEV) or generalized Pareto (GP).  The point process characterization is an equivalent form, but is not handled here.  The GEV df is given by

    Pr{X <= x} = G(x) = exp[-{1 + shape * (x - location)/scale}^(-1/shape)]

for 1 + shape*(x - location) > 0 and scale > 0.  It the shape parameter is zero, then the df is defined by continuity and simplies to

    G(x) = exp(-exp((x - location)/scale)).

The GEV df is often called a family of df's because it encompasses the three types of EVD's: Gumbel (shape = 0, light tail), Frechet (shape > 0, heavy tail) and the reverse Weibull (shape < 0, bounded upper tail at location - scale/shape).  It was first found by R. von Mises (1936) and also independently noted later by meteorologist A. F. Jenkins (1955).  It enjoys theretical support for modeling maxima taken over large blocks of a series of data.

The generalized Pareo df is given by (Pickands, 1975)

    Pr{X <= x} = F(x) = 1 - [1 + shape * (x - threshold)/scale]^(-1/shape)

where 1 + shape * (x - threshold)/scale > 0, scale > 0, and x > threshold.  If shape = 0, then the GP df is defined by continuity and becomes

    F(x) = 1 - exp(-(x - threshold)/scale).

There is an approximate relationship between the GEV and GP df's where the GP df is approximately the tail df for the GEV df.  In particular, the scale parameter of the GP is a function of the threshold (denote it scale.u), and is equivalent to scale + shape*(threshold - location) where scale, shape and location are parameters from the \dQuote{equivalent} GE Vdf.  Similar to the GEV df, the shape parameter determines the tail behavior, where shape = 0 gives rise to the exponential df (light tail), shape > 0 the Pareto df (heavy tail) and shape < 0 the Beta df (bounded upper tail at location - scale.u/shape).  Theoretical justification supports the use of the GP df family for modeling excesses over a high threshold (i.e., y = x - threshold).  It is assumed here that \code{x}, \code{q} describe x (not y = x - threshold).  Similarly, the random draws are y + threshold.

See Coles (2001) and Reiss and Thomas (2007) for a very accessible text on extreme value analysis and for more theoretical texts, see for example, Beirlant et al. (2004), de Haan and Ferreira (2006), as well as Reiss and Thomas (2007).
}
\value{
'devd' gives the density function, 'pevd' gives the distribution function, 'qevd' gives the quantile function, and 'revd' generates random deviates for the GEV or GP df depending on the type argument.
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
There is a similarity between the location parameter of the GEV df and the threshold for the GP df.  For clarity, two separate arguments are emplyed here to distinguish the two instead of, for example, just using the location parameter to describe both.
}

\seealso{
\code{\link{fevd}}
}
\examples{
## GEV df (Frechet type)
devd(2:4, 1, 0.5, 0.8) # pdf
pevd(2:4, 1, 0.5, 0.8) # cdf 
qevd(seq(1e-8,1-1e-8,,20), 1, 0.5, 0.8) # quantiles
revd(10, 1, 0.5, 0.8) # random draws

## GP df
devd(2:4, scale=0.5, shape=0.8, threshold=1, type="GP")
pevd(2:4, scale=0.5, shape=0.8, threshold=1, type="GP")
qevd(seq(1e-8,1-1e-8,,20), scale=0.5, shape=0.8, threshold=1, type="GP")
revd(10, scale=0.5, shape=0.8, threshold=1, type="GP")

\dontrun{
# The fickleness of extremes.
z1 <- revd(100, 1, 0.5, 0.8)
hist(z1, breaks="FD", freq=FALSE, xlab="GEV distributed random variables", col="darkblue")
lines(seq(0,max(z1),,200), devd(seq(0,max(z1),,200), 1, 0.5, 0.8), lwd=1.5, col="yellow")
lines(seq(0,max(z1),,200), devd(seq(0,max(z1),,200), 1, 0.5, 0.8), lwd=1.5, lty=2) 

z2 <- revd(100, 1, 0.5, 0.8)
qqplot(z1, z2)

z3 <- revd(100, scale=0.5, shape=0.8, threshold=1, type="GP")
# Or, equivalently
z4 <- revd(100, 5, 0.5, 0.8, 1, type="GP") # the "5" is ignored.
qqplot(z3, z4)

# Just for fun.
qqplot(z1, z3)

# Compare
par(mfrow=c(2,2))
plot(density(z1), xlim=c(0,100), ylim=c(0,1))
plot(density(z2), xlim=c(0,100), ylim=c(0,1))
plot(density(z3), xlim=c(0,100), ylim=c(0,1))
plot(density(z4), xlim=c(0,100), ylim=c(0,1))

# Three types
x <- seq(0,10,,200)
par(mfrow=c(1,2))
plot(x, devd(x, 1, 1, -0.5), type="l", col="blue", lwd=1.5,
    ylab="GEV df")
# Note upper bound at 1 - 1/(-0.5) = 3 in above plot.

lines(x, devd(x, 1, 1, 0), col="lightblue", lwd=1.5)
lines(x, devd(x, 1, 1, 0.5), col="darkblue", lwd=1.5)
legend("topright", legend=c("(reverse) Weibull", "Gumbel", "Frechet"),
    col=c("blue", "lightblue", "darkblue"), bty="n", lty=1, lwd=1.5)

plot(x, devd(x, 1, 1, -0.5, 1, type="GP"), type="l", col="blue", lwd=1.5,
    ylab="GP df")
lines(x, devd(x, 1, 1, 0, 1, type="GP"), col="lightblue", lwd=1.5)
lines(x, devd(x, 1, 1, 0.5, 1, type="GP"), col="darkblue", lwd=1.5)
legend("topright", legend=c("Beta", "Exponential", "Pareto"), 
    col=c("blue", "lightblue", "darkblue"), bty="n", lty=1, lwd=1.5)

# Emphasize the tail differences more by using different scale parameters.
par(mfrow=c(1,2))
plot(x, devd(x, 1, 0.5, -0.5), type="l", col="blue", lwd=1.5,
    ylab="GEV df")
lines(x, devd(x, 1, 1, 0), col="lightblue", lwd=1.5)
lines(x, devd(x, 1, 2, 0.5), col="darkblue", lwd=1.5)
legend("topright", legend=c("(reverse) Weibull", "Gumbel", "Frechet"), 
    col=c("blue", "lightblue", "darkblue"), bty="n", lty=1, lwd=1.5)

plot(x, devd(x, 1, 0.5, -0.5, 1, type="GP"), type="l", col="blue", lwd=1.5,
    ylab="GP df")
lines(x, devd(x, 1, 1, 0, 1, type="GP"), col="lightblue", lwd=1.5)
lines(x, devd(x, 1, 2, 0.5, 1, type="GP"), col="darkblue", lwd=1.5)
legend("topright", legend=c("Beta", "Exponential", "Pareto"),
    col=c("blue", "lightblue", "darkblue"), bty="n", lty=1, lwd=1.5)

}
}
\keyword{ distribution }
\keyword{ datagen }
