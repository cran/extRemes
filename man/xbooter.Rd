\name{xbooter}
\alias{xbooter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	Additional Bootstrap Functions for Univariate EVA
}
\description{
	Additonal bootstrap capabilities for extreme-value analysis for fevd objects.
}
\usage{
xbooter(x, B, rsize, block.length = 1,
	return.period = c(10, 20, 50, 100, 200, 500),
	qcov = NULL, qcov.base = NULL, shuffle = NULL,
	replace = TRUE, verbose = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	  list object of class \dQuote{fevd}
}
  \item{B, rsize, block.length, shuffle, replace}{ See the help file for booter from the distillery package.  }
  \item{return.period}{
	  numeric value for the desired return period for which CIs are desired.
}
\item{qcov}{numeric matrix with rows the same length as \code{q} and columns equal to the number of parameters (+ 1 for the threshold, if a POT model).  This gives any covariate values for a nonstationary model.  If NULL, and model is non-stationary, only the intercept terms for modeled parameters are used, and if a non-constant threshold, only the first threshold value is used.  Not used if model is stationary. }
   \item{qcov.base}{numeric matrix analogous to \code{qcov}.  When provided, the function returns the difference in return levels between the level for the covariates in \code{qcov} and the level for covariates in \code{qcov.base}.}
  \item{verbose}{
	  logical if TRUE progress information is printed to the screen.
}
  \item{\dots}{
	  Additonal optional arguments to the \code{booter} function.
}
}
\details{
	The \code{ci} method function will perform parametric bootstrapping for \dQuote{fevd} objects, but this function is a wrapper to \code{booter}, which allows for greater flexibility with \dQuote{fevd} objects.  Gives CIs for the EVD parameters and return levels.
}
\value{
	Object of class \dQuote{booted} is returned.  See the help file for \code{booter} for more information.
}
\author{
	Eric Gilleland
}
\references{
Gilleland, E. (2020) Bootstrap methods for statistical inference. Part I: Comparative forecast verification for continuous variables. \emph{Journal of Atmospheric and Oceanic Technology}, \bold{37} (11), 2117 - 2134, doi: 10.1175/JTECH-D-20-0069.1.

Gilleland, E. (2020) Bootstrap methods for statistical inference. Part II: Extreme-value analysis. \emph{Journal of Atmospheric and Oceanic Technology}, \bold{37} (11), 2135 - 2144, doi: 10.1175/JTECH-D-20-0070.1.
}
\seealso{
\code{\link{fevd}}, \code{\link{booter}}, \code{\link{xtibber}}, \code{\link{ci.fevd}}
}
\examples{
set.seed( 409 )
z <- apply( matrix( rnorm( 100 * 1000 ), 1000, 100 ), 2, max )
fit <- fevd( z )

# In order to keep the code fast for CRAN compiling, 
# a low value for B is used here, but should use a larger
# value in general.
bfit <- xbooter( fit, B = 50, verbose = TRUE )
ci( bfit, type = "perc" )
}
\keyword{ htest }% use one of  RShowDoc("KEYWORDS")
