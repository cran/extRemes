\name{return.level}
\alias{return.level}
\alias{return.level.fevd}
\alias{return.level.fevd.bayesian}
\alias{return.level.fevd.lmoments}
\alias{return.level.fevd.mle}
\alias{return.level.ns.fevd.bayesian}
\alias{return.level.ns.fevd.mle}
\alias{print.return.level}
\title{
Return Level Estimates
}
\description{
Return level estimates from fitted fevd model objects.
}
\usage{
return.level(x, return.period = c(2, 20, 100), ...)

\method{return.level}{fevd}(x, return.period = c(2, 20, 100), ...)

\method{return.level}{fevd.bayesian}(x, return.period = c(2, 20, 100), ..., do.ci = FALSE,
                 burn.in = 499, FUN = "mean", qcov = NULL, qcov.base =
                 NULL)

\method{return.level}{fevd.lmoments}(x, return.period = c(2, 20, 100), ...,
    do.ci = FALSE)

\method{return.level}{fevd.mle}(x, return.period = c(2, 20, 100), ...,
    do.ci = FALSE, qcov = NULL, qcov.base = NULL)

\method{return.level}{ns.fevd.bayesian}(x, return.period = 100, ...,
    burn.in = 499, FUN = "mean", do.ci = FALSE, verbose = FALSE,
    qcov = NULL, qcov.base = NULL)

\method{return.level}{ns.fevd.mle}(x, return.period = c(2, 20, 100), ...,
    alpha = 0.05, method = c("normal"), do.ci = FALSE, verbose = FALSE,
    qcov = NULL, qcov.base = NULL)

\method{print}{return.level}(x, ...)

}
\arguments{
  \item{x}{
A list object of class \dQuote{fevd} as returned by \code{fevd}.  In the case of the \code{print} method function, an object returned by \code{return.level}.
}
  \item{return.period}{
numeric vector of desired return periods.  For \code{return.level.ns.fevd.mle}, this must have length one.
}
   \item{qcov}{numeric matrix with rows the same length as \code{q} and columns equal to the number of parameters (+ 1 for the threshold, if a POT model).  This gives any covariate values for a nonstationary model.  If NULL, and model is non-stationary, only the intercept terms for modeled parameters are used, and if a non-constant threshold, only the first threshold value is used.  Not used if model is stationary. }
   \item{qcov.base}{numeric matrix analogous to \code{qcov}.  When provided, the function returns the difference in return levels between the level for the covariates in \code{qcov} and the level for covariates in \code{qcov.base}.}
   \item{do.ci}{logical; should CIs be returned as well?}
   \item{burn.in}{number giving the burn in value.  The first 1:burn.in will not be used in obtaining parmaeter estimates. }
  \item{FUN}{character string naming a function, or a function, to use to find the parameter estimates from the MCMC sample.  Default is to take the posterior mean (after burn in).}
    \item{alpha}{The (1 - alpha) * 100 percent confidence level for confidence intervals of return levels in non-stationary models.}
    \item{method}{character string naming which CI method to employ.}
    \item{verbose}{logical, should progress information be printed to the screen?}
  \item{\dots}{
For the stationary case only, any optional arguments to the \code{ci} function.  Not used by the \code{print} method function.
}
}
\details{
The extreme value distributions (EVD's) are generalized extreme value (GEV) or generalized Pareto (GP).  The point process characterization is an equivalent form, but is not handled here.  The GEV df is given by

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

Return levels are essentially the same as quantiles.  In the case of the
GEV family, they are the same.  In the case of the GP df, they are very
similar, but the exceedance rate is taken into consideration.  For
non-stationary modeling, effective return levels are calculated for each
value of the covariate(s) used in the model fit (see, e.g., Gilleland
and Katz, 2011).

\code{return.level.ns.fevd.mle} allows one to estimate the difference in
return levels for a non-stationary model, based on subtracting the
return levels for \code{qcov.base} from those for \code{qcov}, in which
case the outputted values and CIs pertain to differences in return levels.

}
\value{
If do.ci is FALSE, an object of class \dQuote{return.level} is returned, which is either a numeric vector (stationary models) of length equal to the \code{return.period} argument giving the return levels, or a matrix of dimension equal to either n by np or q by np where n is the length of the data used to fit the model and np are the number of return periods, and q is the number of rows of qcov, if supplied.  The returned value also includes useful attributes describing how the return levels came to be estimated.  In particular, the list of attributes include:

    \item{return.period}{the return periods associated with the estimated return levels.}
    \item{data.name}{same as the data.name component of the fevd object.}
    \item{fit.call, call}{the original call for the fitted object and the call to this function, resp.}
    \item{fit.type}{character string naming which type of EVD was fit to the data, and subsequently used to estimate the return levels.}
    \item{data.assumption}{character string stating whether the model is stationary or non-stationary.}
    \item{period}{character string stating what the units (period.basis from the fevd object) of the period are.}
    \item{units}{character string giving the data units, if available.}
    \item{qcov}{name of the qcov matrix used to obtain the effective return levels.}
    \item{qcov.base}{when provided as input, the name of the qcov.base matrix used to obtain the
    difference in effective return levels.}

If do.ci is TRUE, then an object returned by the appropriate ci function is returned (stationary case only).
}
\references{
Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004) \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Gilleland, E. and Katz, R. W. (2011). New software to analyze how extremes change over time. \emph{Eos}, 11 January, \bold{92}, (2), 13--14.

de Haan, L. and Ferreira, A. (2006) \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Jenkinson, A. F. (1955) The frequency distribution of the annual maximum (or minimum) of meteorological elements. \emph{Quart. J. R.  Met. Soc.}, \bold{81}, 158--171.

Pickands, J. (1975) Statistical inference using extreme order statistics.  \emph{Annals of Statistics}, \bold{3}, 119--131.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkhauser, 530pp., 3rd edition.

von Mises, R. (1936) La distribution de la plus grande de n valeurs, \emph{Rev. Math. Union Interbalcanique} \bold{1}, 141--160.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{pextRemes}}, \code{\link{fevd}}, \code{\link{rlevd}}, \code{\link{ci.rl.ns.fevd.bayesian}}
}
\examples{
z <- revd(100, loc=20, scale=0.5, shape=-0.2)
fit <- fevd(z)
fit

return.level(fit)

fitLM <- fevd(z, method="Lmoments")
fitLM
return.level(fitLM)

\dontrun{
fitB <- fevd(z, method="Bayesian", verbose=TRUE)
fitB

return.level(fitB)

}
}
\keyword{ distribution }
\keyword{ methods }
