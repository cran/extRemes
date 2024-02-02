\name{pextRemes}
\alias{pextRemes}
\alias{pextRemes.fevd}
\alias{pextRemes.fevd.bayesian}
\alias{pextRemes.fevd.lmoments}
\alias{pextRemes.fevd.mle}
\alias{rextRemes}
\alias{rextRemes.fevd}
\alias{rextRemes.fevd.bayesian}
\alias{rextRemes.fevd.lmoments}
\alias{rextRemes.fevd.mle}
\title{
Probabilities and Random Draws from Fitted EVDs
}
\description{
Calculate probabilities from fitted extreme value distributions (EVDs) or draw random samples from them.
}
\usage{
pextRemes(x, q, lower.tail = TRUE, ...)

rextRemes(x, n, ...)

\method{pextRemes}{fevd}(x, q, lower.tail = TRUE, ..., qcov = NULL)

\method{pextRemes}{fevd.bayesian}(x, q, lower.tail = TRUE, ...,
    qcov = NULL, burn.in = 499, FUN = "mean")

\method{pextRemes}{fevd.lmoments}(x, q, lower.tail = TRUE, ...)

\method{pextRemes}{fevd.mle}(x, q, lower.tail = TRUE, ..., qcov = NULL)

\method{rextRemes}{fevd}(x, n, ...)

\method{rextRemes}{fevd.bayesian}(x, n, ..., burn.in = 499, FUN = "mean",
    qcov = NULL)

\method{rextRemes}{fevd.lmoments}(x, n, ...)

\method{rextRemes}{fevd.mle}(x, n, ..., qcov = NULL)

}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.
}
  \item{q}{
Vector of quantiles.
}
  \item{n}{number of random draws to take from the model.}

  \item{qcov}{
numeric matrix with rows the same length as \code{q} and columns equal to the number of parameters (+ 1 for the threshold, if a POT model).  This gives any covariate values for a nonstationary model.  If NULL, and model is non-stationary, only the intercept terms for modeled parameters are used, and if a non-constant threshold, only the first threshold value is used.  Not used if model is stationary.
}
  \item{lower.tail}{
logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].
}
   \item{burn.in}{the burn in period.}
   \item{FUN}{cahracter string naming a function, or a function, to be used to find the parameter estimates from the posterior df.  Default is the posterior mean.}
  \item{\dots}{
Not used.
}
}
\details{

These functions are essentially wrapper functions for the low-level functions \code{pevd} and \code{revd}.  The idea is that they take parameter values from a fitted model in order to calculate probabilities or draw random samples.  In the case of non-stationary models, for probabilities, covariate values should be given.  If not, the intercept terms (or first threshold value) are used only; and a warning is given.  In the case of \code{rextRemes} for non-stationary values, \code{n} samples of length equal to the length of the data set to which the model was fit are generated and returned as a matrix.  In this case, the random draws represent random draws using the current covariate values.

The extreme value distributions (EVD's) are generalized extreme value (GEV) or generalized Pareto (GP).  The point process characterization is an equivalent form, but is not handled here; parameters are converted to those of the (approx.) equivalent GP df.  The GEV df is given by

    Pr(X <= x) = G(x) = exp[-(1 + shape*(x - location)/scale)^(-1/shape)]

for 1 + shape*(x - location) > 0 and scale > 0.  It the shape parameter is zero, then the df is defined by continuity and simplies to

    G(x) = exp(-exp((x - location)/scale)).

The GEV df is often called a family of df's because it encompasses the three types of EVD's: Gumbel (shape = 0, light tail), Frechet (shape > 0, heavy tail) and the reverse Weibull (shape < 0, bounded upper tail at location - scale/shape).  It was first found by R. von Mises (1936) and also independently noted later by meteorologist A. F. Jenkins (1955).  It enjoys theretical support for modeling maxima taken over large blocks of a series of data.

The generalized Pareo df is given by (Pickands, 1975)

    Pr(X <= x) = F(x) = 1 - [1 + shape*(x - threshold)/scale]^(-1/shape)

where 1 + shape*(x - threshold)/scale > 0, scale > 0, and x > threshold.  If shape = 0, then the GP df is defined by continuity and becomes

    F(x) = 1 - exp(-(x - threshold)/scale).

There is an approximate relationship between the GEV and GP df's where the GP df is approximately the tail df for the GEV df.  In particular, the scale parameter of the GP is a function of the threshold (denote it scale.u), and is equivalent to scale + shape*(threshold - location) where scale, shape and location are parameters from the \dQuote{equivalent} GE Vdf.  Similar to the GEV df, the shape parameter determines the tail behavior, where shape = 0 gives rise to the exponential df (light tail), shape > 0 the Pareto df (heavy tail) and shape < 0 the Beta df (bounded upper tail at location - scale.u/shape).  Theoretical justification supports the use of the GP df family for modeling excesses over a high threshold (i.e., y = x - threshold).  It is assumed here that \code{x}, \code{q} describe x (not y = x - threshold).  Similarly, the random draws are y + threshold.

See Coles (2001) and Reiss and Thomas (2007) for a very accessible text on extreme value analysis and for more theoretical texts, see for example, Beirlant et al. (2004), de Haan and Ferreira (2006), as well as Reiss and Thomas (2007).
}
\value{
A numeric vector of probabilites or random sample is returned.  In the case of non-stationary models, a matrix of random samples is returned by \code{rextRemes}.
}
\references{
Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004) \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

de Haan, L. and Ferreira, A. (2006) \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Jenkinson, A. F. (1955) The frequency distribution of the annual maximum (or minimum) of meteorological elements. \emph{Quart. J. R.  Met. Soc.}, \bold{81}, 158--171.

Pickands, J. (1975) Statistical inference using extreme order statistics.  \emph{Annals of Statistics}, \bold{3}, 119--131.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkhauser, 530pp., 3rd edition.

von Mises, R. (1936) La distribution de la plus grande de n valeurs, \emph{Rev. Math. Union Interbalcanique} \bold{1}, 141--160.
}
\author{
Eric Gilleland
}

\note{It is recommended to use \code{make.qcov} when creating a \code{qcov} matrix.}

\section{Warning }{
In the case of non-stationary models, the code in its current state is somewhat less than ideal.  It requires great care on the part of the user.  In particular, the \code{qcov} argument becomes critical.  Parameters that are fixed in the model can be changed if \code{qcov} is not correctly used.  Any parameter that is fixed at a given value (including the intercept terms) should have all ones in their columns.  Presently, nothing in the code will force this requirement to be upheld.  Using \code{make.qcov} will help, as it has some checks to ensure constant-valued parameters have all ones in their columns.
}

\seealso{
\code{\link{pevd}}, \code{\link{revd}}, \code{\link{fevd}}, \code{\link{make.qcov}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

pextRemes(fit, q=quantile(z, probs=c(0.85, 0.95, 0.99)), lower.tail=FALSE)

z2 <- rextRemes(fit, n=1000)
qqplot(z, z2)

\dontrun{
data(PORTw)
fit <- fevd(TMX1, PORTw, units="deg C")
fit

pextRemes(fit, q=c(17, 20, 25, 30), lower.tail=FALSE)
# Note that fit has a bounded upper tail at:
# location - scale/shape ~
# 15.1406132 + (2.9724952/0.2171486) = 28.82937
#
# which is why P[X > 30] = 0.  Note also that 25
# is less than the upper bound, but larger than
# the maximum observed value.

z <- rextRemes(fit, n=50)
qqplot(z, PORTw$TMX1, xlab="Simulated Data Quantiles",
    ylab="Data Quantiles (PORTw TMX1)")

# Not a great fit because data follow a non-stationary
# distribution.
fit <- fevd(TMX1, PORTw, location.fun=~AOindex, units="deg C")
fit

pextRemes(fit, q=c(17, 20, 25, 30), lower.tail=FALSE)
# Gives a warning because we did not give covariate values.

v <- make.qcov(fit, vals=list(mu1=c(1, -1, 1, -1)))
v
# find probabilities for high positive AOindex vs
# low negative AOindex.  A column for the unnecessary
# threshold is added, but is not used.

pextRemes(fit, q=c(17, 17, 30, 30), qcov=v, lower.tail=FALSE)

z <- rextRemes(fit, n=50)
dim(z)
qqplot(z[,1], PORTw$TMX1, xlab="Simulated Data Quantiles",
    ylab="Data Quantiles (PORTw TMX1)")

qqplot(z[,28], PORTw$TMX1, xlab="Simulated Data Quantiles",
    ylab="Data Quantiles (PORTw TMX1)")
# etc.

##
## GP model with non-constant threshold.
##
fit <- fevd(-MinT ~1, Tphap, threshold=c(-70,-7),
    threshold.fun=~I((Year - 48)/42), type="GP",
    time.units="62/year", verbose=TRUE)
fit

summary(fit$threshold)
v <- make.qcov(fit, vals=c(rep(1,8), c(-77, -73.5, -71.67, -70)), nr=4)
v

# upper bounded df at: u - scale/shape = 
c(-77, -73.5, -71.67, -70) + 2.9500992/0.1636367
# -58.97165 -55.47165 -53.64165 -51.97165
summary(-Tphap$MinT)
pextRemes(fit, q=rep(-58, 4), qcov=v, lower.tail=FALSE)


}
}
\keyword{ distribution }
\keyword{ models }
\keyword{ datagen }
\keyword{ methods }
