\name{taildep}
\alias{taildep}
\title{
Tail Dependence
}
\description{
Function to calculate the estimated tail dependence parameters chi and chibar.
}
\usage{
taildep(x, y, u, type = c("all", "chi", "chibar"), na.rm = FALSE)
}
\arguments{
  \item{x,y}{
numeric vectors of same length.  \code{x} may be a two-column matrix or data frame, in which case each column is assumed to be the two vectors of interest (both dependence estimates are symmetric so that it does not matter which is which).
}
  \item{u}{
single numeric between 0 and 1 (non-inclusive) giving the probability threshold overwhich to compute the dependence measures (should be close to 1, but low enough to include enough data.
}
  \item{type}{
character string determining which dependence parameter to estimate (chi or chibar).  Default estimates both.
}
  \item{na.rm}{
logical, should missing values be removed?
}
}
\details{
The tail dependence parameters are those described in, e.g., Reiss and Thomas (2007) Eq (2.60) for "chi" and Eq (13.25) "chibar", and estimated by Eq (2.62) and Eq (13.28), resp.  See also, Sibuya (1960) and Coles (2001) sec. 8.4, as well as other texts on EVT such as Beirlant et al. (2004) sec. 9.4.1 and 10.3.4 and de Haan and Ferreira (2006).

Specifically, for two series X and Y with associated df's F and G, chi, a function of u, is defined as

    chi(u) = Pr[Y > G^(-1)(u) | X > F^(-1)(u)] = Pr[V > u | U > u],

where (U,V) = (F(X),G(Y))--i.e., the copula.  Define chi = limit as u goes to 1 of chi(u).

The coefficient of tail dependence, chibar(u) was introduced by Coles et al. (1999), and is given by

    chibar(u) = 2*log(Pr[U > u])/log(Pr[U > u, V > u]) - 1.

Define chibar = limit as u goes to 1 of chibar(u).

The associated estimators for the tail dependence parameters employed by these functions are based on the above two coefficients of tail dependence, and are given by Reiss and Thomas (2007) Eq (2.62) and (13.25) as

    chi.hat(x, y; u) = sum(x_i >  sort(x)[floor(n*u)] and y_i > sort(y)[floor(n*u)])/(n*(1-u))   [based on chi]

and

    chibar.hat(x, y; u) = 2*log(1 - u)/log(mean(x_i > sort(x)[floor(n*u)] and y_i > sort(y)[floor(n*u)])) - 1.

Some properties of the above dependence coefficients, chi(u), chi, and chibar(u) and chibar, are that 0 <= chi(u), chi <= 1, where if X and Y are stochastically independent, then chi(u) = 1 - u, and chibar = 0.  If X = Y (perfectly dependent), then chi(u) = chi = 1.  For chibar(u) and chibar, we have that -1 <= chibar(u), chibar <= 1.  If U = V, then chibar = 1.  If chi = 0, then chibar < 1 (tail independence with chibar determining the degree of dependence).

}
\value{
numeric vector of length 1 or 2 depending on the type argument giving the estimated tail dependence parameters.
}
\references{
Beirlant, J., Goegebeur, Y., Teugels, J. and Segers, J. (2004) \emph{Statistics of Extremes: Theory and Applications}.  Chichester, West Sussex, England, UK: Wiley, ISBN 9780471976479, 522pp.

Coles, S.  (2001) \emph{An introduction to statistical modeling of extreme values}, London: Springer-Verlag.

Coles, S., Heffernan, J. E., and Tawn, J. A. (1999) Dependence measures for extreme value analyses.  \emph{Extremes}, \bold{2}, 339--365.

de Haan, L. and Ferreira, A. (2006) \emph{Extreme Value Theory: An Introduction}.  New York, NY, USA: Springer, 288pp.

Reiss, R.-D. and Thomas, M. (2007) \emph{Statistical Analysis of Extreme Values: with applications to insurance, finance, hydrology and other fields}. Birkhauser, 530pp., 3rd edition.

Sibuya, M. (1960) Bivariate extreme statistics.  \emph{Ann. Inst. Math. Statist.}, \bold{11}, 195--210.

}
\author{
Eric Gilleland
}
\seealso{
\code{\link{atdf}}, \code{\link{taildep.test}}
}
\examples{
##
## Example where a r.v. is completely dependent in
## terms of the variables, but completely tail
## independent (see Reiss and Thomas p. 75).
z <- runif(100, -1, 0)
w <- -1*(1 + z)
taildep(z,w,u=0.8)

\dontrun{
data(FCwx)
taildep(FCwx$MxT, FCwx$MnT, 0.8)
taildep(FCwx$MxT, FCwx$Prec, 0.8)
}
}
\keyword{ multivariate }
