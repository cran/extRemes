\name{atdf}
\alias{atdf}
\alias{plot.atdf}
\title{
Auto-Tail Dependence Function
}
\description{
Computes (and by default plots) estimates of the auto-tail dependence function(s) (atdf) based on
either chi (rho) or chibar (rhobar), or both.
}
\usage{
atdf(x, u, lag.max = NULL, type = c("all", "rho", "rhobar"), plot = TRUE,
    na.action = na.fail, ...)

\method{plot}{atdf}(x, type = NULL, ...)
}
\arguments{
  \item{x}{For \code{atdf}: a univariate time series object or a numeric vector.  For the \code{plot} method function, a list object of class \dQuote{atdf}.}
  \item{u}{numeric between 0 and 1 (non-inclusive) determining the level F^(-1)(u) over which to compute the atdf.  Typically, this should be close to 1, but low enough to incorporate enough data.}
  \item{lag.max}{The maximum lag for which to compute the atdf.  Default is 10*log10(n), where n is the length of the data.  Will be automatically limited to one less than the total number of observations in the series.}
  \item{type}{character string stating which type of atdf to calculate/plot (rho, rhobar or both).  If NULL the \code{plot} method function will take the type to be whatever was passed to the call to \code{atdf}.  If \dQuote{all}, then a 2 by 1 panel of two plots are graphed.}
  \item{plot}{logical, should the plot be made or not?  If TRUE, output is returned invisibly.  If FALSE, output is returned normally.}
  \item{na.action}{function to be called to handle missing values.}
  \item{\dots}{
Further arguments to be passed to the \code{plot} method function or to \code{plot}.  Note that if \code{main}, \code{xlab} or \code{ylab} are used with type \dQuote{all}, then the labels/title will be applied to both plots, which is probably not desirable.
}
}
\details{
The tail dependence functions are those described in, e.g., Reiss and Thomas (2007) Eq (2.60) for "chi" and Eq (13.25) "chibar", and estimated by Eq (2.62) and Eq (13.28), resp.  See also, Sibuya (1960) and Coles (2001) sec. 8.4, as well as other texts on EVT such as Beirlant et al. (2004) sec. 9.4.1 and 10.3.4 and de Haan and Ferreira (2006).

Specifically, for two series X and Y with associated df's F and G, chi, a function of u, is defined as

    chi(u) = Pr[Y > G^(-1)(u) | X > F^(-1)(u)] = Pr[V > u | U > u],

where (U,V) = (F(X),G(Y))--i.e., the copula.  Define chi = limit as u goes to 1 of chi(u).

The coefficient of tail dependence, chibar(u) was introduced by Coles et al. (1999), and is given by

    chibar(u) = 2*log(Pr[U > u])/log(Pr[U > u, V > u]) - 1.

Define chibar = limit as u goes to 1 of chibar(u).

The auto-tail dependence function using chi(u) and/or chibar(u) employs X against itself at different lags.

The associated estimators for the auto-tail dependence functions employed by these functions are based on the above two coefficients of tail dependence, and are given by Reiss and Thomas (2007) Eq (2.65) and (13.28) for a lag h as

    rho.hat(u, h) = sum( min(x_i, x_(i+h) ) > sort(x)[floor(n*u)])/(n*(1-u))   [based on chi]

and

    rhobar.hat(u, h) = 2*log(1 - u)/log(sum(min(x_i,x_(i+h)) > sort(x)[floor(n*u)])/(n - h)) - 1.

Some properties of the above dependence coefficients, chi(u), chi, and chibar(u) and chibar, are that 0 <= chi(u), chi <= 1, where if X and Y are stochastically independent, then chi(u) = 1 - u, and chibar = 0.  If X = Y (perfectly dependent), then chi(u) = chi = 1.  For chibar(u) and chibar, we have that -1 <= chibar(u), chibar <= 1.  If U = V, then chibar = 1.  If chi = 0, then chibar < 1 (tail independence with chibar determining the degree of dependence).
}
\value{
A list object of class \dQuote{atdf} is returned with components: 
  \item{call }{The function calling string.}
  \item{type }{character naming the type of atdf computed.}
  \item{series}{character string naming the series used.}
  \item{lag}{numeric vector giving the lags used.}
  \item{atdf}{numeric vector or if type is \dQuote{all}, two-column matrix giving the estimated auto-tail dependence function values.}

The plot method functoin does not return anything.
}
\references{
Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004) \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London, U.K.: Springer-Verlag, 208 pp.

Coles, S., Heffernan, J. E., and Tawn, J. A. (1999) Dependence measures for extreme value analyses.  \emph{Extremes}, \bold{2}, 339--365.

de Haan, L. and Ferreira, A. (2006) \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkhauser, 530pp., 3rd edition.

Sibuya, M. (1960) Bivariate extreme statistics.  \emph{Ann. Inst. Math. Statist.}, \bold{11}, 195--210.
}
\author{
Eric Gilleland
}

\seealso{
\code{\link{acf}}, \code{\link{pacf}}, \code{\link{taildep}}, \code{\link{taildep.test}}
}
\examples{
z <- arima.sim(n = 63, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
               sd = sqrt(0.1796))
hold <- atdf(z, 0.8, plot=FALSE)
par(mfrow=c(2,2))
acf(z, xlab="")
pacf(z, xlab="")
plot(hold, type="chi")
plot(hold, type="chibar")

y <- cbind(z[2:63], z[1:62])
y <- apply(y, 1, max)
hold2 <- atdf(y, 0.8, plot=FALSE)
par(mfrow=c(2,2))
acf(y, xlab="")
pacf(y, xlab="")
plot(hold2, type="chi")
plot(hold2, type="chibar")

\dontrun{
data(Fort)
atdf(Fort[,5], 0.9)

data(Tphap)
atdf(Tphap$MaxT, 0.8)

data(PORTw)
atdf(PORTw$TMX1, u=0.9)
atdf(PORTw$TMX1, u=0.8)
}
}
\keyword{ ts }
\keyword{ univar }
\keyword{ hplot}
